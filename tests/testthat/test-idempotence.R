context("test-idempotence")

test_that("read.write = 1", {
  filename <- tempfile()
  write.arff(iris, "iris", file = filename)
  ir2 <- read.arff(filename)
  expect_true(all(ir2 == iris))
})

test_that("write.read.write = write", {
  filename <- tempfile()
  fn2 <- tempfile()
  write.arff(iris, "iris", file = filename)
  ir2 <- read.arff(filename)
  write.arff(ir2, "iris", file = fn2)
  expect_true(all(readLines(file(filename, "r")) == readLines(file(fn2, "r"))))
})
