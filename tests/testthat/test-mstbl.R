context("test function mstbl")

#-------------------------------------------------------------------------
# call mstbl function
x <- mstbl(cms2016oct_input); xd <- x$mstbl_std; xd <- xd[order(names(xd))]
xmtbl <- create_measure_tbl(xd)

# compare to the std dataset
data("cms2016oct_std");
cmtbl <- create_measure_tbl(cms2016oct_std)

test_that("1. provider_id matches the std", {
  expect_identical(x$msdtbl$provider_id, cms2016oct_std$provider_id)
})

test_that("2. measure table matches the std", {
  expect_identical(xmtbl,cmtbl)
})

# -------------------------------------------------------------------------
xd <- xd[!(names(xd) %in% "provider_id")]
std<-cms2016oct_std[!(names(cms2016oct_std) %in% "provider_id")]

test_that("3. the stadardized score matches the std", {
  for (col in names(xd)) {
    expect_equal(xd[[col]],std[[col]])
  }
  expect_equal(xd$cac_3, std$cac_3)
})

