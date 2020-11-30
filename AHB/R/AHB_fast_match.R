# author: Xian Sun, Duke University, 2020



#Get the predicted outcomes of sample units in the box we constructed now
expansion_variance_tmp <- function(name, current_lower, current_upper,
                                   proposed_bin, bart_fit0, bart_fit1, n_grid_pts = 8) {

  p <- length(current_lower)
  n <-n_grid_pts*p*p
  M <- matrix(c(1:n), nrow = n_grid_pts*p, byrow = TRUE)

  #Get the sample data in the box
  for(cov in 1:p){
    new_val = proposed_bin[cov]
    if (new_val > current_upper[cov]) {grid_pts <- seq(current_upper[cov], new_val, length.out = n_grid_pts)
    }else {grid_pts <- seq(current_lower[cov], new_val, length.out = n_grid_pts)}
    bin_centers <- (current_lower + current_upper) / 2
    pred_data <-t(sapply(grid_pts, function(x) {
      bin_centers[cov] <- x
      bin_centers
    }))
    start_r = 1 + (cov-1)*n_grid_pts
    end_r = start_r+n_grid_pts-1
    M[start_r:end_r,] <- pred_data
  }


  df<-as.data.frame(M)
  names(df)<-name
  M <-df
  bin_mean = c(colMeans(predict(bart_fit1, M)),colMeans(predict(bart_fit0, M)))

  bin_mean = matrix(bin_mean, nrow = 2*p, byrow = TRUE)
  return (bin_mean)
}



make_mg <- function(X, lbs, ubs){
  return(which(colMeans((t(X) <= ubs)*(t(X) >= lbs))==1))
}

#function used in Cpp
#Preprocessing Data to only select those units near the treated unit
#the output is a integer vector containing the indexes of units selected
preprocess_cands <- function(i, test_covs, treatment, bart_fit1, bart_fit0, fhat1, fhat0, n_prune = 200){
  #Do preprocessing, only do MIP on varibales with most similar outcome
  ord = order(abs(fhat1[i] - fhat1)/sd(fhat1) + abs(fhat0[i] - fhat0)/sd(fhat0))
  cands = c(ord[treatment[ord]==T][2:n_prune], ord[treatment[ord]==F][1:n_prune])
  cands = cands[cands != i]
  cands <-cands[!is.na(cands)]

  return (cands)
}

#function used in Cpp
#get the covs, given indexes from preprocess_cands
preprocess_covs <- function(cands, test_covs){
  return (test_covs[cands, ])
}




#'A Fast Approximate Implementation of AHB Matching
#'
#'\code{AHB_fast_match} runs a (fast) approximate implementation of AHB Matching
#'
#'@section Introduction: \code{AHB_fast_match} is a matching algorithm that
#'  matches units with others in unit-specific, hyper-box-shaped regions of the
#'  covariate space.These regions are large enough that many matches are created
#'  for each unit and small enough that the treatment effect is roughly constant
#'  throughout. The regions are found as the solution to using a (fast)
#'  approximation algorithm. For more details, please see the AHB paper
#'  \href{https://arxiv.org/pdf/2003.01805.pdf}{here}.
#'
#'@param data Either a data frame or a path to a .csv file to be read. If
#'  holdout is not a numeric value, this is the data to be matched. If holdout
#'  is a numeric scalar between 0 and 1, that proportion of data will be made
#'  into a holdout set and only the remaining proportion of data will be
#'  matched. Covariates can either be categorical, continuous or mixed
#'  (categorical and continuous). Outcome must be either binary or continuous
#'  (both numeric) with name \code{outcome_column_name} . Treatment must be
#'  described by a logical or binary column with name
#'  \code{treated_column_name}.
#'
#'@param holdout  A data frame, a path to a .csv file to be read or a numeric
#'  scalar between 0 and 1 .  If a numeric scalar, that proportion of data will be
#'  made into a holdout set and only the remaining proportion of data will be
#'  matched. Otherwise, a dataframe or a path to a .csv file. Must have the same
#'  column names and order as data. This data will not be matched. Covariates
#'  can either be categorical, continuous or mixed (categorical and
#'  continuous).Outcome must be either binary or continuous (both numeric) with
#'  name \code{outcome_column_name} . Treatment must be described by a logical
#'  or binary column with name \code{treated_column_name}.

#'@param treated_column_name A character with the name of the treatment column
#'  in data and holdout. Defaults to 'treated'.
#'
#'@param outcome_column_name A character with the name of the outcome column in
#'  holdout and also in data, if supplied in the latter. Defaults to 'outcome'.
#'
#'@param black_box Denotes the method to be used to generate outcome model Y. If
#'  "BART" and cv = F, uses \code{dbarts::bart} with keeptrees = TRUE, keepevery
#'  = 10, verbose = FALSE, k = 2 and ntree =200 and then the default predict
#'  method to estimate the outcome. If "BART" and cv = T, k and ntree will be
#'  best values from cross validation. Defaults to 'BART'. There will be
#'  multiple choices about black_box in the future.
#'
#'@param cv A logical scalar. If \code{TURE}, do cross-validation on the train
#'  set to generate outcome model Y . Defaults to \code{TRUE}.
#'
#'@param C A positive scalar. Determines the stopping condition for Fast AHB.
#'  When the variance in a newly expanded region exceeds C times the variance in
#'  the previous expansion region, the algorithm stops. Thus, higher C
#'  encourages coarser bins while lower C encourages finer ones. The user should
#'  analyze the data with multiple values of C to see how robust results are to
#'  its choice.
#'@param n_prune A numeric value, the number of candidate units selected to
#'  construct the box. Defualt n_prune = 50.
#'
#'@return The basic object returned by \code{AHB_fast_match} is a list of 5
#'  entries: \describe{\item{data}{Data set was matched by
#'  \code{AHB_fast_match}. If holdout is not a numeric value, the
#'  \code{AHB_fast_out$data} is the same as the data input into
#'  \code{AHB_fast_match}.  If holdout is a numeric scalar between 0 and 1,
#'  \code{AHB_fast_out$data} is the remaining proportion of data that were
#'  matched.} \item{units_id}{A integer vector with unit_id for test treated
#'  units} \item{CATE}{A numeric vector with the conditional average treatment
#'  effect estimates for every test treated unit in its matched group in
#'  \code{MGs}} \item{bins}{ An array of two lists where the first one contains
#'  lower bounds and the other contains upper bounds for each test treated unit.
#'  Each row of each list is a vector corresponding to a test treated unit.}
#'  \item{MGs}{A list of all the matched groups formed by AHB_fast_match. For
#'  each test treated unit, each row contains all unit_id of the other units
#'  that fall into its box, including itself. } }
#'
#'
#'@importFrom stats  predict rbinom rnorm var formula runif
#'@importFrom utils combn flush.console
#'@importFrom dbarts xbart bart
#'@export


AHB_fast_match<-function(data,
                         holdout = 0.1,
                         treated_column_name = 'treated',
                         outcome_column_name = 'outcome',
                         black_box = 'BART',
                         cv = F,
                         C = 1.1,
                         n_prune = ifelse(is.numeric(holdout),
                                          round(0.1*(1-holdout) * nrow(data)),
                                          round(0.1*nrow(data)))){

  df <- read_data(data,holdout,treated_column_name,outcome_column_name)
  check_args_fast(data = df[[1]],holdout = df[[2]],
                  treated_column_name=treated_column_name, outcome_column_name=outcome_column_name,
                  black_box = black_box, cv = cv, C = C)

  inputs <- estimator_inputs(df[[2]], df[[1]],
                             treated_column_name = treated_column_name, outcome_column_name = outcome_column_name,
                             black_box = black_box, cv = cv)

  f <- inputs[[1]]
  n <- inputs[[2]]
  n_train <- inputs[[3]]
  p <- inputs[[4]]
  train_df <- inputs[[5]]
  train_covs <- inputs[[6]]
  train_control <- inputs[[7]]

  train_treated <- inputs[[8]]
  test_df <- inputs[[9]]
  test_covs <- inputs[[10]]
  test_control <- inputs[[11]]
  test_treated <- inputs[[12]]
  n_test_control <- inputs[[13]]
  n_test_treated <- inputs[[14]]
  bart_fit0 <- inputs[[15]]
  bart_fit1 <- inputs[[16]]
  #counterfactuals <- inputs[[17]]


  message("Running AHB_fast_matching")

  if(n_prune >= 200){
    message(paste0("If computation speed is very slow, please adjust
the parameter 'n_prune' into smaller values.
For now, n_prune = ", n_prune, ". Try to set n_prune below 400 or even smaller"))}

  start_time <- Sys.time()
  fast_bins <- vector(mode = 'list', length = 1)
  fast_cates = numeric(n_test_treated)

  #Greedy
  test_df_treated <- test_df[, which(colnames(test_df) == treated_column_name)]
  test_df_outcome <- test_df[, which(colnames(test_df) == outcome_column_name)]

  fhat1 = colMeans(predict(bart_fit1, newdata = test_covs))
  fhat0 = colMeans(predict(bart_fit0, newdata = test_covs))

  greedy_out<-greedy_cpp(names(test_covs),as.matrix(test_covs[test_treated, ]), test_control-1, test_treated-1,
                         as.matrix(test_covs), as.logical(test_df_treated), test_df_outcome,
                         1, 15, C,bart_fit0, bart_fit1, fhat0, fhat1, expansion_variance_tmp,
                         preprocess_cands, preprocess_covs, n_prune)

  fast_cates <- greedy_out[[1]]
  lower_bounds <- do.call(rbind, greedy_out[[2]])
  upper_bounds <- do.call(rbind, greedy_out[[3]])
  fast_bins <- array(c(lower_bounds, upper_bounds), dim = c(dim(lower_bounds), 2))
  MGs <- greedy_out[[4]]

  end_time <- Sys.time()
  t = difftime(end_time, start_time, units = "auto")
  message(paste0("Time to match ", length(test_treated), " units: ", format(round(t, 2), nsmall = 2) ))
  return(list(data = test_df, units_id = test_treated, CATE = fast_cates, bins = fast_bins, MGs = MGs, verbose = c(treated_column_name,outcome_column_name)))
}
