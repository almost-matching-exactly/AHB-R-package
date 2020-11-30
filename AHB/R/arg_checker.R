# author: Xian Sun, Duke University, 2020

check_treat_col_type<-function(data,data_cols, column_name){
  data_treat <- data[, which(data_cols == column_name)]
  for(l in 1:length(data_treat)){
    if (!is.logical(data_treat[l]) && (data_treat[l] != 1 &&data_treat[l] != 0)){
      stop('Treated variable in data must be numeric binary or logical.')
    }
  }
}

check_args_fast <-function(data, holdout,
                           treated_column_name, outcome_column_name,
                           black_box,cv, C){

  if (!is.data.frame(data)) {
    stop(paste('data must be a data frame or a character denoting a .csv file',
               'in the working directory.'))
  }
  if (!is.data.frame(holdout)) {
    stop(paste('holdout must be a data frame, a character denoting a .csv',
               'file in the working directory, or a numeric proportion of',
               'data to use as a holdout set.'))
  }

  if(is.numeric(holdout)){
    if(holdout<=0 || holdout >=1){
      stop(paste('holdout should bigger than 0 and smaller than 1.'))
    }
  }

  if(black_box != 'BART'){
    stop(paste('black_box must be BART now.'))
  }

  data_cols <- colnames(data)
  holdout_cols <- colnames(holdout)

  if (!(outcome_column_name %in% holdout_cols)) { # Check if can remove
    stop('Holdout must contain outcome column with name outcome_column_name')
  }
  if (!identical(data_cols, holdout_cols)) {
    stop(paste('data and holdout must contain',
               'identical column names.'))
  }


  if (!is.character(treated_column_name)) {
    stop('treated_column_name must be a character.')
  }

  if (!(treated_column_name %in% data_cols)) {
    stop('treated_column_name must be the name of a column in data.')
  }

  if (!(treated_column_name %in% holdout_cols)) {
    stop('treated_column_name must be the name of a column in holdout.')
  }

  check_treat_col_type(data,data_cols,treated_column_name)
  check_treat_col_type(holdout,holdout_cols,treated_column_name)

  if (!is.character(outcome_column_name)) {
    stop('Outcome_column_name must be a character.')
  }

  if (!(outcome_column_name %in% data_cols)) {
    stop('outcome_column_name must be the name of a column in data.')
  }

  if (!(outcome_column_name %in% holdout_cols)) {
    stop('outcome_column_name must be the name of a column in holdout.')
  }

  if (!is.numeric(data[, which(data_cols == outcome_column_name)])) {
    stop('Outcome variable in data must be numeric value.')
  }

  if (!is.numeric(holdout[, which(holdout_cols == outcome_column_name)])) {
    stop('Outcome variable in holdout must be numeric value')
  }

  if(length(cv)!=1 | !is.logical(cv)){
    stop('cv must be a logical scalar.')
  }

  if (length(C)!=1 | !is.numeric(C) | C < 0 | is.infinite(C)) {
    stop('C must be a finite, nonnegative scalar.')
  }

}


check_args_MIP <-function(data, holdout,
                          treated_column_name, outcome_column_name,
                          black_box,cv,gamma0,gamma1,beta,m,M){

  if (!is.data.frame(data)) {
    stop(paste('data must be a data frame or a character denoting a .csv file',
               'in the working directory.'))
  }
  if (!is.data.frame(holdout)) {
    stop(paste('holdout must be a data frame, a character denoting a .csv',
               'file in the working directory, or a numeric proportion of',
               'data to use as a holdout set.'))
  }

  if(black_box != 'BART'){
    stop(paste('black_box must be BART now.'))
  }
  data_cols <- colnames(data)
  holdout_cols <- colnames(holdout)

  if (!(outcome_column_name %in% holdout_cols)) { # Check if can remove
    stop('Holdout must contain outcome column with name outcome_column_name')
  }
  if (!identical(data_cols, holdout_cols)) {
    stop(paste('data and holdout must contain',
               'identical column names.'))
  }


  if (!is.character(treated_column_name)) {
    stop('treated_column_name must be a character.')
  }

  if (!(treated_column_name %in% data_cols)) {
    stop('treated_column_name must be the name of a column in data.')
  }

  if (!(treated_column_name %in% holdout_cols)) {
    stop('treated_column_name must be the name of a column in holdout.')
  }

  check_treat_col_type(data,data_cols,treated_column_name)
  check_treat_col_type(holdout,holdout_cols,treated_column_name)

  if (!is.character(outcome_column_name)) {
    stop('Outcome_column_name must be a character.')
  }

  if (!(outcome_column_name %in% data_cols)) {
    stop('outcome_column_name must be the name of a column in data.')
  }

  if (!(outcome_column_name %in% holdout_cols)) {
    stop('outcome_column_name must be the name of a column in holdout.')
  }

  if (!is.numeric(data[, which(data_cols == outcome_column_name)])) {
    stop('Outcome variable in data must be numeric value.')
  }

  if (!is.numeric(holdout[, which(holdout_cols == outcome_column_name)])) {
    stop('Outcome variable in holdout must be numeric value')
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
}
