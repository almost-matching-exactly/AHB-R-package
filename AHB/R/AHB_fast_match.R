# author: Xian Sun, Duke University, 2020



#Get the predicted outcomes of sample units in the box we constructed now
expansion_variance_tmp <- function(name, black_box, current_lower, current_upper,
                                   proposed_bin,user_PE_fit, user_PE_predict,PE_predict,
                                   bart_fit0, bart_fit1, n_grid_pts = 8) {
  # PE_predict_params <- pkg.env$PE_predict_params
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
  bin_mean = 0
  fhat1 <- do.call(PE_predict, c(list(bart_fit1, as.matrix(M)), PE_predict_params))
  fhat0 <- do.call(PE_predict, c(list(bart_fit0, as.matrix(M)), PE_predict_params))
  if(black_box=='BART' && is.null(user_PE_predict) && is.null(user_PE_fit)){
    fhat1<- colMeans(fhat1)
    fhat0<- colMeans(fhat0)
  }
  bin_mean = c(fhat1,fhat0)
  bin_mean = matrix(bin_mean, nrow = 2*p, byrow = TRUE)
  return (bin_mean)
}


#Construct a matched group with covs and lower and higher bounds
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
#'@param PE_method Denotes the method to be used to generate outcome model Y.
#' If "BART", use bart in dbarts as ML model to do prediction.
#' If "xgb, use xgboost as ML model to do prediction.
#'  Defaults to "BART".
#'
#' @param user_PE_fit An optional function supplied by the user that can be used
#'   instead of those allowed for by \code{PE_method} to fit a model for the
#'   outcome from the covariates. Must take in a matrix of covariates as its
#'   first argument and a vector outcome as its second argument. Defaults to
#'   \code{NULL}.
#' @param user_PE_fit_params A named list of optional parameters to be used by
#'   \code{user_PE_fit}. Defaults to \code{NULL}.
#' @param user_PE_predict An optional function supplied by the user that can be
#'   used to generate predictions from the output of \code{user_PE_fit}. As its
#'   first argument, must take an object of the type returned by
#'  \code{user_PE_fit} and as its second, a matrix of values for which to
#'  generate predictions. When the outcome is binary or multi-class, must
#'  return the maximum probability class label. If not supplied,
#'  defaults to \code{predict}.
#' @param user_PE_predict_params A named list of optional parameters to be used
#'   by \code{user_PE_predict}. Defaults to \code{NULL}.
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
#'@param n_prune A numeric value, the number of candidate units selected to run
#'  the mip on for constructing the box. Dataset mentioned below is refered to
#'  the dataset for matching. If you match a small dataset with the number of
#'  units smaller than 400, it will run MIP on all dataset for each treated
#'  unit. If you match larger dataset and your memory of your computer cannot
#'  support such much computation, plase adjust n_prune below 400 or even
#'  smaller. The smaller number of candidate units selected to run the mip on
#'  for constructing the box, the faster this program runs.Defualt n_prune =
#'  0.1* nrow(dataset).
#'
#'
#' @param missing_data Specifies how to handle missingness in \code{data}. If
#'   'none' (default), assumes no missing data; if 'drop', effectively drops
#'   units with missingness from the data and does not match them;
#'   if 'impute', imputes the missing data via \code{mice::mice}.
#' @param missing_holdout Specifies how to handle missingness in \code{holdout}.
#'   If 'none' (default), assumes no missing data; if 'drop', effectively drops
#'   units with missingness and does not use them to compute PE; and if 'impute',
#'   imputes the missing data via \code{mice::mice}. In this last case, the PE
#'   at an iteration will be given by the average PE across all imputations.
#' @param impute_with_treatment A logical scalar. If \code{TRUE}, uses treatment
#'   assignment to impute covariates when \code{missing_data = 'impute'} or
#'   \code{missing_holdout = 'impute'}. Defaults to \code{TRUE}.
#' @param impute_with_outcome A logical scalar. If \code{TRUE}, uses outcome
#'   information to impute covariates when \code{missing_data = 'impute'} or
#'   \code{missing_holdout = 'impute'}. Defaults to \code{FALSE}.
#'
#'@return The basic object returned by \code{AHB_MIP_match} is a list of 5
#'  entries: \describe{\item{data}{Clean data set after preprocessing was matched by \code{AHB_MIP_match}.
#'  If holdout is not a numeric value, the \code{AHB_MIP_out$data} is the same
#'  as the data input into \code{AHB_MIP_match}.  If holdout is a numeric scalar
#'  between 0 and 1, \code{AHB_MIP_out$data} is the remaining proportion of data
#'  that were matched.}
#'  \item{data_dummy}{This is dummy version of \code{data}. AHB will convert all categorical data into dummies}
#'   \item{units_id}{A integer vector with unit_id for test
#'  treated units} \item{CATE}{A numeric vector with the conditional average
#'  treatment effect estimates for every test treated unit in its matched group
#'  in \code{MGs}} \item{bins}{ An array of two lists where the first one
#'  contains lower bounds and the other contains upper bounds for each test
#'  treated unit. Each row of each list is a vector corresponding to a test
#'  treated unit.} \item{MGs}{A list of all the matched groups formed by
#'  AHB_fast_match. For each test treated unit, each row contains all unit_id of
#'  the other units that fall into its box, including itself. } }
#'  \item{list_dummyCols}{This is a list of dummy cols after mapping}
#'  \item{verbose}{This is used for postprocessing. Contains \code{treated_column_name} and \code{outcome_column_name}}

#'@importFrom stats  predict rbinom rnorm var formula runif complete.cases
#'@importFrom utils combn flush.console
#'@import fastDummies
#'@export


AHB_fast_match<-function(data,
                         holdout = 0.1,
                         treated_column_name = 'treated',
                         outcome_column_name = 'outcome',
                         PE_method = "BART",
                         user_PE_fit = NULL, user_PE_fit_params = NULL,
                         user_PE_predict = NULL, user_PE_predict_params = NULL,
                         cv = F,
                         C = 1.1,
                         n_prune = ifelse(is.numeric(holdout),
                                          round(0.1*(1-holdout) * nrow(data)),
                                          round(0.1*nrow(data))),
                         missing_data = 'none', missing_holdout = 'none',
                         impute_with_treatment = TRUE, impute_with_outcome = FALSE
                         ){

  df <- read_data(data,holdout)
  check_args_fast(df[[1]],df[[2]], treated_column_name, outcome_column_name,
                 PE_method, user_PE_fit, user_PE_fit_params,
                 user_PE_predict, user_PE_predict_params,cv,C,n_prune)

  df[[2]] <- mapCategoricalToFactor(df[[2]],treated_column_name, outcome_column_name)
  df[[1]] <- mapCategoricalToFactor(df[[1]],treated_column_name, outcome_column_name)
  train_ <-handle_missing(df[[2]], "training dataset", missing_holdout,
                          treated_column_name, outcome_column_name,
                          impute_with_treatment, impute_with_outcome)
  test_ <- handle_missing(df[[1]], "testing dataset", missing_data,
                          treated_column_name, outcome_column_name,
                          impute_with_treatment, impute_with_outcome)
  map1<- mapFactorToDummy(train_, treated_column_name, outcome_column_name)
  train_dummy<- map1[[1]]
  list_dummyCols <- map1[[2]]
  test_dummy <- mapFactorToDummy(test_, treated_column_name, outcome_column_name)[[1]]

  inputs <- estimator_inputs(train_df = train_dummy, test_df = test_dummy,
                             user_PE_fit = user_PE_fit, user_PE_fit_params = user_PE_fit_params,
                             user_PE_predict = user_PE_predict, user_PE_predict_params = user_PE_predict_params,
                             treated_column_name= treated_column_name, outcome_column_name=outcome_column_name,
                             black_box =  PE_method, cv = cv)

  p = inputs[[4]]
  test_df = inputs[[9]]
  test_covs = inputs[[10]]
  test_control = inputs[[11]]
  test_treated = inputs[[12]]
  n_test_control = inputs[[13]]
  n_test_treated = inputs[[14]]
  bart_fit0 = inputs[[15]]
  bart_fit1 = inputs[[16]]

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
  fhat1 = 0
  fhat0 = 0
  PE_predict <- predict
  PE_predict_params <- user_PE_predict_params
  assign("PE_predict_params", PE_predict_params, envir = .GlobalEnv)# Make PE_predict_params as globalEnv for greedy_cpp

  if(!is.null(user_PE_predict)){
    PE_predict <- user_PE_predict
  }
  fhat1 <- do.call(PE_predict, c(list(bart_fit1, as.matrix(test_covs)), PE_predict_params))
  fhat0 <- do.call(PE_predict, c(list(bart_fit0, as.matrix(test_covs)), PE_predict_params))
  if(PE_method=='BART' && is.null(user_PE_predict) && is.null(user_PE_fit)){
    fhat1<- colMeans(fhat1)
    fhat0<- colMeans(fhat0)
  }

  greedy_out<-greedy_cpp(names(test_covs),PE_method, as.matrix(test_covs[test_treated, ]), test_control-1, test_treated-1,
                         as.matrix(test_covs), as.logical(test_df_treated), test_df_outcome,
                         1, 15, C,user_PE_fit, user_PE_predict, PE_predict, bart_fit0, bart_fit1,
                         fhat0, fhat1, expansion_variance_tmp,
                         preprocess_cands, preprocess_covs, n_prune)

  fast_cates <- greedy_out[[1]]
  lower_bounds <- do.call(rbind, greedy_out[[2]])
  upper_bounds <- do.call(rbind, greedy_out[[3]])
  fast_bins <- array(c(lower_bounds, upper_bounds), dim = c(dim(lower_bounds), 2))
  MGs <- greedy_out[[4]]
  end_time <- Sys.time()
  t = difftime(end_time, start_time, units = "auto")
  message(paste0("Time to match ", length(test_treated), " units: ", format(round(t, 2), nsmall = 2) ))
  return(list(data = test_, data_dummy = test_df,treated_unit_ids = test_treated, CATE = fast_cates, bins = fast_bins, MGs = MGs,list_dummyCols = list_dummyCols, verbose = c(treated_column_name,outcome_column_name)))
}
