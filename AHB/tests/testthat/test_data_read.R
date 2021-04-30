check_read_data<-function(data,holdout){
  df <- read_data(data, holdout)
  expect_equal(is.data.frame(df[[1]]),T)
  expect_equal(is.data.frame(df[[2]]),T)
}


test_that("test_read_data", {
  check_read_data("data.csv","holdout.csv")
  check_read_data(gen_data(),"holdout.csv")
  check_read_data("data.csv",gen_data())
  check_read_data("data.csv",0.5)
  check_read_data(gen_data(),0.5)
  expect_error(read_data(gen_data(),-0.5))
  expect_error(read_data(gen_data(),1.5))
  expect_error(read_data("sadf.csv",1.5))
  expect_error(read_data(gen_data(),"asdf.csv"))
})
