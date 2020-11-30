# author: Xian Sun, Duke University, 2020
test_that("column order doesn't matter", {
  p <- 4
  data <- gen_data(n_units = 50, p = p)
  holdout <-  gen_data(n_units = 50, p = p)

  scrambling <- order(sample(1:(p + 2)))
  scrambled_data <- data[, scrambling]
  scrambled_holdout <- holdout[, scrambling]

  scrambled_fast_out <- AHB_fast_match(data = scrambled_data, holdout = scrambled_holdout)

  scrambled_MIP_out <- AHB_MIP_match(data = scrambled_data, holdout = scrambled_holdout)

})


test_that("outcome/treatment name doesn't matter", {
  p <- 4
  data <- gen_data(n_units = 50, p = p)
  holdout <-  gen_data(n_units = 50, p = p)

  renamed_data <- data
  renamed_holdout <- holdout

  colnames(renamed_data)    <- c('X1', 'X2', 'X3', 'X4', 'myout', 'myt')
  colnames(renamed_holdout) <- c('X1', 'X2', 'X3', 'X4', 'myout', 'myt')

  renamed_fast_out <- AHB_fast_match(data = renamed_data, holdout = renamed_holdout,
                         treated_column_name = 'myt',
                         outcome_column_name = 'myout')
  renamed_MIP_out <- AHB_MIP_match(data = renamed_data, holdout = renamed_holdout,
                                     treated_column_name = 'myt',
                                     outcome_column_name = 'myout')
})


