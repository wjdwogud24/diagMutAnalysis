
library(diagMutAnalysis)

test_that("if function creates correct plot and that two plots are returned", {
  test <- mutationTypePlot(icgc_data)

  expect_type(test, "list")
  expect_length(test, 2)
})

test_that("Check chromosome on plots", {
  test <- mutationTypePlot(icgc_data, chromosome = "X")

  expect_type(test, "list")
  expect_identical(sum(test$substitution_types$data$n), as.integer(10))

  })

test_that("Check if population works",{
  test <- mutationTypePlot(icgc_data, population = 10)

  expect_type(test, "list")
  expect_identical(sum(test$substitution_types$data$n), as.integer(2))

})

test_that("Check if population works large number",{
  test <- mutationTypePlot(icgc_data, population = 10000)

  expect_type(test, "list")
  expect_identical(sum(test$substitution_types$data$n), as.integer(530))

})

test_that("Check if population works decimal",{
  test <- mutationTypePlot(icgc_data, population = 10000.1)

  expect_type(test, "list")
  expect_identical(sum(test$substitution_types$data$n), as.integer(530))

})

test_that("testing mutationPercentage if correct data is returned according to
          ICGC", {
  test <- mutationPercentage(icgc_data)

  expect_type(test, "list")
  expect_identical(max(test$mutation_count), as.integer(25))
})
