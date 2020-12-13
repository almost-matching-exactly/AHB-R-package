# author: Xian Sun, Duke University, 2020

# test_that("Try middle dataset on MIP ", {
#   p <- 5
#   data <-  gen_data(n_units = 1000, p = p)
#   MIP_out <- AHB_MIP_match(data = data, n_prune = 50)
#   print(AHB::ATE(MIP_out))
#   print(AHB::ATT(MIP_out))
# })

# test_that("Try large dataset on MIP ", {
#   p <- 5
#   data <-  gen_data(n_units = 10000, p = p)
#   MIP_out <- AHB_MIP_match(data = data, n_prune = 50)
#   print(AHB::ATE(MIP_out))
#   print(AHB::ATT(MIP_out))
# })

test_that("Try small dataset on MIP ", {
  p <- 5
  df <- gen_data(n_units = 100, p = p)
  MIP_out <- AHB_MIP_match(data = df, holdout = 0.5,black_box='BART')
  print(AHB::ATE(MIP_out))
  print(AHB::ATT(MIP_out))
})

test_that("Try small dataset on fast", {
  p <- 5
  df <- gen_data(n_units = 100, p = p)
  fast_out <- AHB_fast_match(data = df, holdout = 0.5,n_prune = 50)
  print(AHB::ATE(fast_out))
  print(AHB::ATT(fast_out))
})

# test_that("Try middle dataset on fast", {
#   p <- 5
#   df <- gen_data(n_units = 1000, p = p)
#   fast_out <- AHB_fast_match(data = df, holdout = 0.5,n_prune = 50)
#   print(AHB::ATE(fast_out))
#   print(AHB::ATT(fast_out))
# })
#
# test_that("Try large dataset on fast", {
#   p <- 5
#   df <- gen_data(n_units = 10000, p = p)
#   fast_out <- AHB_fast_match(data = df, holdout = 0.5,n_prune = 50)
#   print(AHB::ATE(fast_out))
#   print(AHB::ATT(fast_out))
# })
