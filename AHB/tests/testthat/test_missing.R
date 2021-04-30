
library("testthat")
if (!requireNamespace("assert", quietly = TRUE)) {
  warning("The assert package must be installed")
  return(NULL)
}

gen_missing <- function(n=100,p=5){
  data <- gen_data(n,p)
  data[1,"X1"] <- NA
  data[2,"X2"] <- NA
  data["X4"] <- 1
  data[c(1,2,3,4),"X4"]<-0
  data["X4"] <-
    lapply(data[, "X4", drop = FALSE], as.factor)
  return (data)
}

test_that("impute_missing", {
  data <- gen_missing()
  expect_equal(sum(is.na(data)) > 0, TRUE)
  data <- mapCategoricalToFactor(data,treated_column_name = 'treated',outcome_column_name = 'outcome')
  res<-impute_missing(data,1,'treated','outcome', T,  F)
  expect_equal(sum(is.na(res)) == 0, TRUE)
  res<-impute_missing(data,1,'treated','outcome', F,  T)
  expect_equal(sum(is.na(res)) == 0, TRUE)
  res<-impute_missing(data,1,'treated','outcome', T,  T)
  expect_equal(sum(is.na(res)) == 0, TRUE)
  res<-impute_missing(data,1,'treated','outcome', F,  F)
  expect_equal(sum(is.na(res)) == 0, TRUE)
})

test_that("handle_missing", {
  data_missing <- gen_missing()
  data_missing <- mapCategoricalToFactor(data_missing,treated_column_name = 'treated',outcome_column_name = 'outcome')
  expect_error(handle_missing(data_missing,"data",missing_data = "none",'treated','outcome', T, F))
  res <- handle_missing(data_missing,"data",missing_data = "drop",'treated','outcome', T, F)
  expect_equal(sum(is.na(res)) == 0, TRUE)
  res <- handle_missing(data_missing,"data",missing_data = "impute",'treated','outcome', T, F)
  expect_equal(sum(is.na(res)) == 0, TRUE)
  expect_equal(colnames(data_missing), colnames(res))
  data_missing1 <- data_missing
  data_missing1["treated"]<-NA
  expect_error(handle_missing(data_missing1,"data",missing_data = "none",'treated','outcome', T, F))
  data_missing2<- data_missing
  data_missing2[1,"treated"]<-NA
  res <- handle_missing(data_missing,"data",missing_data = "impute",'treated','outcome', T, F)
  data<-gen_data()
  res <- handle_missing(data,"data",missing_data = "impute",'treated','outcome', T, F)
})
