# author: Xian Sun, Duke University, 2020

estimator_inputs <- function(train_df, test_df,
                             treated_column_name = 'treated',outcome_column_name = 'outcome',
                             black_box='BART', cv= T) {

  n_train <- nrow(train_df)
  n_units <- nrow(train_df) + nrow(test_df)
  p <- ncol(train_df) - 2
  #f <- formula(paste('treated ~', paste(colnames(train_df)[1:p], collapse = ' + ')))

  train_treat_col_ind <- which(colnames(train_df) == treated_column_name)
  train_out_col_ind <- which(colnames(train_df) == outcome_column_name)
  test_treat_col_ind <- which(colnames(test_df) == treated_column_name)
  test_out_col_ind <- which(colnames(test_df) == outcome_column_name)
  test_covs_ind <- which(colnames(test_df) != treated_column_name & colnames(test_df) != outcome_column_name)
  train_covs_ind <- which(colnames(train_df) != treated_column_name & colnames(train_df) != outcome_column_name)

  train_df[,train_treat_col_ind] <- train_df[,train_treat_col_ind] == 1
  test_df[,test_treat_col_ind] <- test_df[,test_treat_col_ind] == 1

  train_control <- which(!train_df[,train_treat_col_ind] )
  train_treated <- which(train_df[,train_treat_col_ind])

  # test_covs <- test_df[, 1:p]
  test_covs <- test_df[, test_covs_ind]

  test_control <- which(!test_df[,test_treat_col_ind])
  test_treated <- which(test_df[,test_treat_col_ind])


  train_covs <- train_df[, train_covs_ind]

  n_test_control <- length(test_control)
  n_test_treated <- length(test_treated)

  if (black_box == 'BART') {
    k <- 2
    n.trees <- 200
    k0 <- 2
    k1 <- 2
    n.trees0 <- 200
    n.trees1 <- 200
    if (cv) {
      message('Cross-validating BART; should take a minute\r')
      flush.console()
      n.trees <- c(100, 200)
      nu_q <- list(c(3, 0.9), c(3, 0.99), c(10, 0.75))
      k <- c(2, 3, 5)
      alpha <- c(0.5, 0.95)[2]
      beta <- c(.5, 2)[2]

      train_df1 <- train_df[which(train_df[,train_treat_col_ind]==T), ]
      train_df0 <- train_df[which(train_df[,train_treat_col_ind]==F), ]

      formula1 <-  train_df1[, which(colnames(train_df1) != outcome_column_name)]
      cv1 <- apply(xbart(formula = formula1,
                         data = train_df1[, which(colnames(train_df1) == outcome_column_name)],
                         verbose = FALSE,
                         method = 'k-fold',
                         n.test = 5,
                         n.reps = 5,
                         loss = 'rmse',
                         n.trees = n.trees,
                         k = k,
                         power = beta,
                         base = alpha),c(2, 3), mean)
      best_params1 <- arrayInd(which.min(cv1), dim(cv1))
      n.trees1 <- n.trees[best_params1[1]]
      k1 <- k[best_params1[2]]

      formula0 <-  train_df0[, which(colnames(train_df0) != outcome_column_name)]
      cv0 <- apply(xbart(formula = formula0,
                         data = train_df0[, which(colnames(train_df0) == outcome_column_name)],
                         verbose = FALSE,
                         method = 'k-fold',
                         n.test = 5,
                         n.reps = 5,
                         loss = 'rmse',
                         n.trees = n.trees,
                         k = k,
                         power = beta,
                         base = alpha),c(2, 3), mean)
      best_params0 <- arrayInd(which.min(cv0), dim(cv0))
      n.trees0 <- n.trees[best_params0[1]]
      k0 <- k[best_params0[2]]
    }

    bbf0 <- bart(x.train = train_df[which(!train_df[,train_treat_col_ind]), train_covs_ind],
                 y.train = train_df[which(!train_df[,train_treat_col_ind]), train_out_col_ind],
                 x.test = test_df[which(test_df[,test_treat_col_ind]), test_covs_ind], # Prognostic score on test units
                 keeptrees = TRUE,
                 keepevery = 10,
                 verbose = FALSE,
                 k = k0,
                 ntree = n.trees0)
    bbf1 <- bart(x.train = train_df[which(train_df[,train_treat_col_ind]), train_covs_ind],
                 y.train = train_df[which(train_df[,train_treat_col_ind]), train_out_col_ind],
                 keeptrees = TRUE,
                 keepevery = 10,
                 verbose = FALSE,
                 k = k1,
                 ntree = n.trees1)
  }
  else {
    stop("black_box must be one of: BART, xgb, LASSO")
  }
  return(list(f = 1,
              n = n_units,
              n_train = n_train,
              p = p,
              train_df = train_df,
              train_covs = train_covs,
              train_control = train_control,
              train_treated = train_treated,
              test_df = test_df,
              test_covs = test_covs,
              test_control = test_control,
              test_treated = test_treated,
              n_test_control = n_test_control,
              n_test_treated = n_test_treated,
              bart_fit0 = bbf0,
              bart_fit1 = bbf1))
}

#' ATE of a matched dataset
#'
#' \code{ATE} computes the average treatment effect (ATE) of a matched dataset.
#'
#' The ATE is computed as the difference between the weighted treated and the
#' weighted control outcomes in the dataset. A unit's weight is the number of
#' times it was matched.
#'
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export

ATE <- function(AHB_out){
  CATEs <- AHB_out$CATE
  MGs <- AHB_out$MGs
  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_CATE_sum <- 0

  for (j in 1:length(MGs)) {
    MG_weight <- sum(weight[MGs[[j]]])
    weight_sum <- weight_sum + MG_weight
    weighted_CATE_sum <- weighted_CATE_sum + MG_weight * CATEs[[j]]
  }
  ATE <- weighted_CATE_sum / weight_sum

  return (ATE)
}

#' ATT of a matched dataset
#'
#' \code{ATT} computes the average treatment effect on the treated (ATT) of a
#' matched dataset.
#'
#' The counterfactual outcome of each treated unit is estimated via the mean
#' outcome of control units in its matched group. This value is then averaged
#' across all treated units to compute the ATT.
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export
ATT <- function(AHB_out) {
  ind_treated <- which(colnames(AHB_out$data) == AHB_out$verbose[1])
  ind_outcome <- which(colnames(AHB_out$data) == AHB_out$verbose[2])
  controls <-  which(AHB_out$data[, ind_treated] == 0)
  treated <- which(AHB_out$data[, ind_treated] == 1)
  outcomes <- AHB_out$data[,ind_outcome]
  MGs <- AHB_out$MGs

  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_TT_sum <- 0
  for (j in 1:length(MGs)) {
    MG_controls <- MGs[[j]][MGs[[j]] %in% controls]
    MG_treated <- MGs[[j]][MGs[[j]] %in% treated]

    MG_weight <- sum(weight[MG_controls])
    weight_sum <- weight_sum + MG_weight
    mean_control_outcome <- mean(outcomes[MG_controls])

    for (k in seq_along(MG_treated)) {
      weighted_TT_sum <-
        weighted_TT_sum +
        MG_weight * (outcomes[MG_treated[k]] - mean_control_outcome)/length(MG_treated)
    }
  }

  ATT <- weighted_TT_sum / weight_sum
  return(ATT)
}

