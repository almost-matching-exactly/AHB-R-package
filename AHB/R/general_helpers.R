# author: Xian Sun, Duke University, 2020

estimator_inputs <- function(train_df, test_df,
                             treated_column_name = 'treated',outcome_column_name = 'outcome',
                             user_PE_fit = NULL, user_PE_fit_params = NULL,
                             user_PE_predict = NULL, user_PE_predict_params = NULL,
                             black_box='xgb', cv= T) {

  n_train <- nrow(train_df)
  n_units <- nrow(train_df) + nrow(test_df)
  p <- ncol(train_df) - 2
  #get indexes
  train_treat_col_ind <- which(colnames(train_df) == treated_column_name)
  train_out_col_ind <- which(colnames(train_df) == outcome_column_name)
  test_treat_col_ind <- which(colnames(test_df) == treated_column_name)
  test_out_col_ind <- which(colnames(test_df) == outcome_column_name)
  test_covs_ind <- which(colnames(test_df) != treated_column_name & colnames(test_df) != outcome_column_name)
  train_covs_ind <- which(colnames(train_df) != treated_column_name & colnames(train_df) != outcome_column_name)
  #Get data for later use
  train_df[,train_treat_col_ind] <- train_df[,train_treat_col_ind] == 1
  test_df[,test_treat_col_ind] <- test_df[,test_treat_col_ind] == 1
  train_covs <- train_df[, train_covs_ind]
  train_control <- which(!train_df[,train_treat_col_ind] )
  train_treated <- which(train_df[,train_treat_col_ind])
  test_covs <- test_df[, test_covs_ind]
  test_control <- which(!test_df[,test_treat_col_ind])
  test_treated <- which(test_df[,test_treat_col_ind])
  n_test_control <- length(test_control)
  n_test_treated <- length(test_treated)
  tmp_cov1 <- as.matrix(train_df[which(train_df[,train_treat_col_ind]), train_covs_ind])
  tmp_Y1 <-  as.matrix(train_df[which(train_df[,train_treat_col_ind]), train_out_col_ind])
  tmp_cov0 <-  as.matrix(train_df[which(!train_df[,train_treat_col_ind]), train_covs_ind])
  tmp_Y0 <-  as.matrix(train_df[which(!train_df[,train_treat_col_ind]), train_out_col_ind])

  if (!is.null(user_PE_fit)) {
    PE_fit <- user_PE_fit
    PE_fit_params <- user_PE_fit_params
  }
  else if (black_box == 'BART') {
    if (!requireNamespace("dbarts", quietly = TRUE)) {
      stop("The bart package must be installed")
    }
    PE_fit <- dbarts::bart
    PE_fit_params <- list(keeptrees = TRUE,
                          keepevery = 10,
                          verbose = FALSE,
                          k = 2,
                          ntree=200)
    if (cv) {
      message('Some versions of dbarts might lead to your R crashing, plaease downloaded the lastest version of dbarts')
      message('Cross-validating BART; should take a minute\r')
      flush.console()
      n.trees <- c(100, 200, 300)
      k <- c(2, 3, 5)
      nu_q <- list(c(3, 0.9), c(3, 0.99), c(10, 0.75))
      alpha <- c(0.5, 0.95)[2]
      beta <- c(.5, 2)[2]
      cv <- apply(dbarts::xbart(formula = as.matrix(train_covs),
                                 data =as.matrix(train_df[,train_out_col_ind]),
                                 verbose = FALSE,
                                 method = 'k-fold',
                                 n.test = 5,
                                 n.reps = 5,
                                 loss = 'rmse',
                                 n.trees = n.trees,
                                 k = k,
                                 power = beta,
                                 base = alpha),c(2, 3), mean)
      best_params <- arrayInd(which.min(cv), dim(cv))
      n.trees <- n.trees[best_params[1]]
      k <- k[best_params[2]]
      PE_fit_params<-list(keeptrees = TRUE,
                          keepevery = 10,
                          verbose = FALSE,
                          k = k,
                          ntree=n.trees)
    }
  }
  else if (black_box =="xgb" ) {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("The xgboost package must be installed")
      }

    PE_fit_params <- list(list(objective = 'reg:squarederror',
                               eta = 1,
                               max_depth = 2,
                               alpha = 0.5,
                               subsample = 0.5),
                              nrounds = 2,
                              verbose = 0)
    PE_fit <-  xgboost::xgboost

    if(cv){
      message('Cross-validating xgb\r')
      eta <- c(.01, .05, .1, .2, .3, .5)
      max_depth <- c(2, 3, 4, 6, 8)
      alpha <- c(.01, .1, .5, 1, 5)
      nrounds <- c(5, 10, 50, 100, 200)
      subsample <- c(0.1, 0.3, 0.5, 0.75, 1)
      param_combs <- as.data.frame(expand.grid(eta, max_depth, alpha, nrounds, subsample))
      colnames(param_combs) <- c('eta', 'max_depth', 'alpha', 'nrounds', 'subsample')

      RMSE <- vector(mode = 'numeric', length = length(param_combs))
      for (i in 1:length(param_combs)) {
        params <- list(objective = 'reg:squarederror',
                       eta = param_combs$eta[i],
                       max_depth = param_combs$max_depth[i],
                       alpha = param_combs$alpha[i],
                       subsample = param_combs$subsample[i])
        cv <- xgboost::xgb.cv(data = as.matrix(train_covs),
                      label = as.matrix(train_df[,train_out_col_ind]),
                      params = params, metrics = list('rmse'),
                      nrounds = param_combs$nrounds[i], nfold = 5, verbose = 0)
        RMSE[i] <- cv$evaluation_log$test_rmse_mean[param_combs$nrounds[i]]
      }
      best_params <- param_combs[which.min(RMSE), ]
      PE_fit_params <- list(list(objective = 'reg:squarederror',
                                eta = best_params$eta,
                                max_depth = best_params$max_depth,
                                alpha = best_params$alpha,
                                subsample = best_params$subsample),
                                nrounds = best_params$nrounds,
                                verbose = 0)
    }
  }
  else {
    stop("black_box must be one of: BART, xgb")
  }

  bbf0 <- do.call(PE_fit, c(list(tmp_cov0, tmp_Y0), PE_fit_params))
  bbf1 <- do.call(PE_fit, c(list(tmp_cov1, tmp_Y1), PE_fit_params))

  return(list(f = NULL,
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

