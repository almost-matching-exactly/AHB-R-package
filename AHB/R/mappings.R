# This function to map the non-numeric and integer column in the data frame into factor
mapCategoricalToFactor<- function(data, treated_column_name, outcome_column_name){
  # Map everything to factor
  cov_inds_data <- which(!(colnames(data) %in% c(treated_column_name, outcome_column_name)))
  cov_inds_numeric <- which(unlist(lapply(data, is.double)))
  cov_inds_numeric <- Reduce(intersect,list(cov_inds_numeric,cov_inds_data))
  cov_inds_categorical <- cov_inds_data[!(cov_inds_data %in% cov_inds_numeric)]
  data[, cov_inds_categorical] <-
    lapply(data[, cov_inds_categorical, drop = FALSE], as.factor)
  return (data = data);
}



# This function convert the all factor column into dummies
mapFactorToDummy <- function(data, treated_column_name, outcome_column_name){
  if (!requireNamespace("fastDummies", quietly = TRUE)) {
    stop("The fastDummies package must be installed")
  }
  listDummyCol <- list()
  colNames <- colnames(data)
  data_dummy <- NULL
  ind <- 1
  for(i in 1:length(colNames)){
    cov = colNames[i]
    if(is.factor(data[,cov]) && !(cov %in% c(treated_column_name, outcome_column_name))){
      toAdd <- fastDummies::dummy_cols(data[cov], remove_selected_columns=TRUE)
      listDummyCol[[ind]] <- collectColNames(cov, colnames(toAdd))
      ind <- ind + 1
    }
    else{
      toAdd<-data[cov]
    }
    if(is.null(data_dummy)){
      data_dummy <- toAdd
    }
    else{
      data_dummy<-cbind(data_dummy,toAdd)
    }
  }
  return (list(data_dummy = data_dummy,listDummyCol = listDummyCol))
}

#This helps to remove the original column name from the dummy column
removeHeadString<- function(headStr, totalStr){
  return (substr(totalStr, nchar(headStr)+2, nchar(totalStr)))
}

#This collects the dummy columNames
collectColNames<- function(headStr, colNames){
  list_names <- c()
  for(i in 1:sum(lengths(colNames))){
    list_names[i] <- removeHeadString(headStr, colNames[i])
  }
  return (list_names)
}
