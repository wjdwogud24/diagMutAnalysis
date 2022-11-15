
library(diagMutAnalysis)

test_that("if function creates correct plot and that two plots are returned", {
  test <- mutationTypePlot(icgc_data)

  expect_type(test, "list")
  expect_length(test, 2)
})


test_that("testing mutationPercentage if correct data is returned according to
          ICGC", {
  test <- mutationPercentage(icgc_data)

  expect_type(test, "list")
  expect_identical(max(test$mutation_count), as.integer(25))
})
