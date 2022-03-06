
require(testthat)

load("analysis/input/tmb.rda")

test_that("splitfun updates correctly", {
	orig <- list(a = 1, b = 2)
	update1 <- c(d = 3, e = 4)
	update2 <- c(f = 5, f = 6)
	update3 <- c(b = 5, b = 6)
	expect_equal(splitfun(orig, update1), c(orig, as.list(update1)))
	expect_equal(splitfun(orig, update2), c(orig, list(f=c(5,6))))
	expect_equal(splitfun(orig, update3), list(a=1, b=c(5,6)))
})

test_that("fix_prov_names handles vectors, errors on matrices", {
	egprovs <- c("AA", "BB", "CC")
	refvec <- c(a=1, b=1, b=1, b=2, d=1, d=2, d=3, e=5)
	correctnames <- c("a", paste("b", egprovs, sep="."), paste("d", egprovs, sep="."), "e")
	correctnames2 <- c("a", paste("b", egprovs, sep="."), rep("d", length(egprovs)), "e")
	refmat <- matrix(rep(1, length(refvec)^2), ncol = length(refvec), dimnames = list(names(refvec),names(refvec)))
	expect_equal(names(fix_prov_names(refvec, egprovs)), correctnames)
	expect_equal(names(fix_prov_names(refvec, egprovs, "b")), correctnames2)
	expect_true(all(refvec == fix_prov_names(refvec, egprovs)))
	expect_error(fix_prov_names(refmat, egprovs))
})

test_that("fix_prov_names complains when provided fix_vars with no matches", {
	egprovs <- c("AA", "BB", "CC")
	refvec <- c(a=1, b=1, b=1, b=2, d=1, d=2, d=3, e=5)
	expect_error(fix_prov_names(refvec, egprovs, "z"))
})

test_that("fix_prov_names complains when fix_vars aren't the same length as provs", {
	egprovs <- c("AA", "BB", "CC")
	refvec <- c(a=1, b=1, b=1, b=2, d=1, d=2, d=3, e=5)
	expect_error(fix_prov_names(refvec, egprovs, "e"))	
})

test_that("anonymize_names is the inverse of fix_prov_names", {
	egprovs <- c("AA", "BB", "CC")
	refvec <- c(a=1, b=1, b=1, b=2, d=1, d=2, d=3, e=5)
	expect_equal(names(anonymize_names(fix_prov_names(refvec, egprovs))), names(refvec))
	expect_equal(anonymize_names(fix_prov_names(refvec, egprovs)), refvec)
})

dt <- readRDS("analysis/input/sgtf_trim.rds")

test_that("est_thresholds is giving a reasonable guess", {
	res <- est_thresholds(dt)
})