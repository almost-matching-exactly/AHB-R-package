# author: Xian Sun, Duke University, 2020


check_hpyerparameter_fast <-function(PE_method,cv, C){
  if(!(PE_method %in% c('BART', 'xgb'))){
    stop(paste('black_box must be BART or xgb now.'))
  }
  if(length(cv)!=1 | !is.logical(cv)){
    stop('cv must be a logical scalar.')
  }

  if (length(C)!=1 | !is.numeric(C) | C < 0 | is.infinite(C)) {
    stop('C must be a finite, nonnegative scalar.')
  }
}


check_hpyerparameter_MIP <-function(PE_method,cv,gamma0,gamma1,beta,m,M,MIP_solver){
  if (!is.function(PE_method)) {
    if (!(PE_method %in% c('BART', 'xgb'))) {
      stop("`PE_method` must be one of 'Bart' or 'xgb', or a user-supplied function.")
    }
  }

  if(length(cv)!=1 | !is.logical(cv)){
    stop('cv must be a logical scalar.')
  }
  if (length(gamma0)!=1 | !is.numeric(gamma0) | is.infinite(gamma0)) {
    stop('gamma0 must be a finite, numeric scalar.')
  }
  if (length(gamma1)!=1 | !is.numeric(gamma1) | is.infinite(gamma1)) {
    stop('gamma1 must be a finite, numeric scalar.')
  }
  if (length(beta)!=1 | !is.numeric(beta) | is.infinite(beta)) {
    stop('beta must be a finite, numeric scalar.')
  }
  if (length(m)!=1 | (m-floor(m))!=0 | m<1) {
    stop('m must be a integer scalar, equal to or larger than 1 .')
  }
  if (length(M)!=1 | !is.numeric(M) | M<1e5) {
    stop('M must be a numeric scalar, equal to or larger than 1e5 .')
  }
  if (MIP_solver!="Rglpk" && MIP_solver!="Rcplex") {
    stop('MIP_solver must be Rglpk or Rcplex .')
  }

}



check_dataset_type<-function(data,treated_column_name, outcome_column_name){
  if (!is.data.frame(data)) {
    stop(paste('dataset must be a data frame or a character denoting a .csv file',
               'in the working directory. For holdout, it can also be a numeric number between 0 and 1, exclusive'))
  }
  data_cols <- colnames(data)

  if (!is.character(treated_column_name)) {
    stop('treated_column_name must be a character.')
  }
  if (!(treated_column_name %in% data_cols)) {
    stop('treated_column_name must be the name of a column in data.')
  }
  check_treat_col_type(data,data_cols,treated_column_name)
  if (!is.character(outcome_column_name)) {
    stop('Outcome_column_name must be a character.')
  }
  if (!(outcome_column_name %in% data_cols)) {
    stop('outcome_column_name must be the name of a column in data.')
  }
  if (!is.numeric(data[, which(data_cols == outcome_column_name)])) {
    stop('Outcome variable in data must be numeric value.')
  }
}

check_treat_col_type<-function(data,data_cols, column_name){
  data_treat <- data[, which(data_cols == column_name)]
  for(l in 1:length(data_treat)){
    if (!is.logical(data_treat[l]) && ((!is.numeric(data_treat[l])) && (data_treat[l] != 1 &&data_treat[l] != 0)) ){
      stop('Treated variable in data must be numeric binary or logical.')
    }
  }
}


#This is for checking if the both datasets: data and holdout are legal
check_args_dataset<-function(data,holdout,treated_column_name, outcome_column_name){
  check_dataset_type(data, treated_column_name, outcome_column_name)
  check_dataset_type(holdout, treated_column_name, outcome_column_name)
  data_cols <- colnames(data)
  holdout_cols <- colnames(holdout)
  if (!identical(data_cols, holdout_cols)) {
    stop(paste('data and holdout must contain',
               'identical column names.'))
  }
}


check_users_PE<-function( user_PE_fit, user_PE_fit_params,
                          user_PE_predict, user_PE_predict_params){
  # if (is.function(PE_method) & !is.null(user_PE_fit)) {
  #   stop('`PE_method` and `user_PE_fit` should not both be supplied. ',
  #        'We recommend the former as the latter will soon be deprecated.',
  #        call. = FALSE)
  # }
  # if (!is.function(PE_method)) {
  #   if (!(PE_method %in% c('ridge', 'xgb'))) {
  #     stop("`PE_method` must be one of 'ridge' or 'xgb', or a user-supplied function.")
  #   }
  # }
  if (!is.null(user_PE_fit_params) & is.null(user_PE_fit)) {
    stop("May not have `user_PE_fit` be NULL if ",
         "`user_PE_fit_params` is not NULL.")
  }
  if (!is.null(user_PE_predict_params) & is.null(user_PE_predict)) {
    stop("May not have `user_PE_predict` be NULL if ",
         "`user_PE_predict_params` is not NULL.")
  }
  if (!is.null(user_PE_fit_params)) {
    if (!is.list(user_PE_fit_params)) {
      stop("`user_PE_fit_params` must be a list")
    }
  }
  if (!is.null(user_PE_predict_params)) {
    if (!is.list(user_PE_predict_params)) {
      stop("`user_PE_predict_params` must be a list")
    }
  }
}



check_args_MIP<-function(data,
                     holdout,
                     treated_column_name,
                     outcome_column_name,
                     PE_method,
                     user_PE_fit, user_PE_fit_params,
                     user_PE_predict, user_PE_predict_params,
                     cv,gamma0, gamma1, beta,m,M,n_prune,
                     MIP_solver){
  if((!is.numeric(n_prune)) | n_prune <= 0 | n_prune > NROW(data)){
    stop("n_prune must be numeric between 0 and the number of units in the dataset")
  }
  check_args_dataset(data,holdout,treated_column_name, outcome_column_name)
  check_users_PE(user_PE_fit, user_PE_fit_params,user_PE_predict, user_PE_predict_params)
  check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,m,M,MIP_solver)
}

check_args_fast<-function(data,
                         holdout,
                         treated_column_name,
                         outcome_column_name,
                         PE_method,
                         user_PE_fit, user_PE_fit_params,
                         user_PE_predict, user_PE_predict_params,
                         cv,
                         C,
                         n_prune){
  if((!is.numeric(n_prune)) | n_prune <= 0 | n_prune > NROW(data)){
    stop("n_prune must be numeric between 0 and the number of units in the dataset")
  }
  check_args_dataset(data,holdout,treated_column_name, outcome_column_name)
  check_users_PE(user_PE_fit, user_PE_fit_params,user_PE_predict, user_PE_predict_params)
  check_hpyerparameter_fast(PE_method,cv,C)
}
