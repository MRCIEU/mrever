context("Connecting and querying mr-eve")
library(mrever)


test_that("can connect", {
	expect_equal(length(connect()$version), 1)
})

test_that("can get all traits", {
	expect_gt(nrow(get_traits()), 100)
})

test_that("can get one trait", {
	expect_equal(nrow(get_traits("2")), 1)
})

test_that("can get several trait", {
	expect_equal(nrow(get_traits(c("2", "7", "1001"))), 3)
})

test_that("can extract instruments", {
	a <- get_instruments(2)
	expect_equal(sum(a$pval < 5e-8), nrow(a))
})

test_that("can extract instruments, multiple", {
	a <- get_instruments(c(2, 7))
	expect_equal(length(unique(a$bgcidId)), 2)
})

