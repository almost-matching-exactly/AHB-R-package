library("testthat")
if (!requireNamespace("assert", quietly = TRUE)) {
  warning("The assert package must be installed")
  return(NULL)
}

test_that("mapCategoricalToFactor", {
  data <- gen_data()
  data[1,"X1"] <- NA
  data[2,"X2"] <- NA
  data["X3"] = as.factor(letters[1:100])
  data <- mapCategoricalToFactor(data,treated_column_name = 'treated',outcome_column_name = 'outcome')
  expect_equal(is.double(data[,"X1"]), TRUE)
  expect_equal(is.numeric(data[,"X2"]) , TRUE)
  expect_equal(is.factor(data[,"X3"]), TRUE)
})



test_that("mapFactorToDummy ", {
  data <- gen_data()
  data["X3"] = as.factor(letters[1:100])
  data["X4"] <- "a"
  data[c(1:4),"X4"] <- "0"
  data["X2"] <- 1
  data[c(1:4),"X2"] <- 0
  data <- mapCategoricalToFactor(data,treated_column_name = 'treated',outcome_column_name = 'outcome')
  data <- handle_missing(data,"data",missing_data = "drop",'treated','outcome', T, F)
  res <- mapFactorToDummy(data, 'treated','outcome')
  data_dummy <- res[[1]]
  listDummyCol<-res[[2]]
  expect_equal(is.double(data_dummy[,"X1"]), TRUE)
  expect_equal(is.numeric(data_dummy[,"X2"]) , TRUE)
  expect_equal(is.numeric(data_dummy[,"X3_z"]), TRUE)
  expect_equal(is.numeric(data_dummy[,"X4_a"]), TRUE)
})


test_that("collectColNames ", {
  colNames = c("a_1", "a_2", "a_3")
  res <- collectColNames("a", colNames)
  expect_equal(c("1", "2", "3"), res)
})
