test_that("a single attribute may be numeric", {
  filename <- tempfile()
  write("
@relation test
@attribute Test numeric
@data
{}
{0 -1}
{0 1}", filename)
  ds <- read.arff(filename)

  expect_true(ds[1,1] == 0)
  expect_true(ds[2,1] == -1)
  expect_true(ds[3,1] == 1)
})
