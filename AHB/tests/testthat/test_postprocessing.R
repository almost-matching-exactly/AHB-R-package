

test_that("printOneLineForFactor", {
  colName = "sex"
  container = c("male", "female", "gay", "middle")
  info <- printOneLineForFactor(colName, container)
  expect_equal(info, "sex in { male , female , gay , middle }")
})


test_that("printOneLineNumeric", {
  colName = "age"
  info <- printOneLineNumeric(colName, 0,45)
  expect_equal(info, "age from  0.000 to 45.000")
})

test_that("mapCategoricalBoundsToOriginal", {
  data <- gen_mixedData()
  data[1,"treated"]<-1
  AHB_out <- AHB_fast_match(data,data)
  treated_unit_id <- 1
  data <- AHB_out$data
  bins <- AHB_out$bins
  data_dummy <- AHB_out$data_dummy
  treated_unit_ids <-AHB_out$treated_unit_ids
  list_dummyCols <- AHB_out$list_dummyCols
  treated_column_name <- AHB_out$treated_column_name
  outcome_column_name <- AHB_out$outcome_column_name
  lowerBound <- bins[which(treated_unit_ids == treated_unit_id), ,1]
  higherBound<-bins[which(treated_unit_ids == treated_unit_id), ,2]
  unit_dummy <- data_dummy[1,]
  cov <- "gender"
  info <- mapCategoricalBoundsToOriginal(lowerBound,higherBound,cov,list_dummyCols[[1]],unit_dummy)
  print(info)
  expect_equal(info,"gender in { female , male }")
})

test_that("test_print_box", {
  data <- gen_mixedData()
  data[1,"treated"] <- 1
  AHB_out <- AHB_fast_match(data,data)
  expect_error(print_box(AHB_out, 10000))
  print_box(AHB_out, 1)
})
