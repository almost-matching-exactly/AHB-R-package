test_that("getColIndexForINTandOther", {
  test_df <- gen_mixedData()
  test_df <- mapCategoricalToFactor(test_df,"treated","outcome")
  test_df <- mapFactorToDummy(test_df, "treated","outcome")[[1]]
  indexesForIntAndOther <- getColIndexForINTandOther(test_df,"treated","outcome")
  ind_integer = indexesForIntAndOther$ind_integer
  ind_other = indexesForIntAndOther$ind_other
  expect_equal(ind_integer, c(3,4,5,6,7));
  expect_equal(ind_other, c(1,2));
})
