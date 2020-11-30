# author: Xian Sun, Duke University, 2020
test_that("holdout is not offered by users", {
  p <- 4
  data <- gen_data(n_units = 100, p = p)
  fast_out <- AHB_fast_match(data = data, holdout = 0.5)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5)
})


test_that("Cross validation", {
  p <- 4
  data <- gen_data(n_units = 100, p = p)
  fast_out <- AHB_fast_match(data = data, holdout = 0.5,cv = T)
  fast_out <- AHB_fast_match(data = data, holdout = 0.5,cv = F)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, cv = T)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, cv = F)
})

test_that("Hpyerparameters for Fast", {
  p <- 4
  data <- gen_data(n_units = 100, p = p)
  fast_out <- AHB_fast_match(data = data, holdout = 0.5, C = 2)
})

test_that("Hpyerparameters for MIP", {
  p <- 4
  data <- gen_data(n_units = 100, p = p)

  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, gamma0=4)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, gamma1=5)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, beta=6)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, M=1e6)
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, m=3)

})


test_that("Rglpk for MIP", {
  p <- 4
  data <- gen_data(n_units = 100, p = p)

  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, gamma0=4, MIP_solver = "Rglpk")
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, gamma1=5, MIP_solver = "Rglpk")
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, beta=6, MIP_solver = "Rglpk")
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, M=1e6, MIP_solver = "Rglpk")
  MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, m=3, MIP_solver = "Rglpk")

})
