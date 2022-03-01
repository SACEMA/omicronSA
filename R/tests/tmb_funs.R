
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

test_that("fix_prov_names handles both vectors and matrices", {
	egprovs <- c("AA", "BB", "CC")
	refvec <- c(a=1, b=1, b=1, b=2, d=1, d=2, d=3, e=5)
	correctnames <- c("a", paste("b", egprovs, sep="."), paste("d", egprovs, sep="."), "e")
	refmat <- matrix(rep(1, length(refvec)^2), ncol = length(refvec), dimnames = list(names(refvec),names(refvec)))
	newvec <- fix_prov_names(refvec, egprovs)
	newmat <- fix_prov_names(refmat, egprovs)
	expect_equal(names(newvec), correctnames)
	expect_equal(colnames(newmat), correctnames)
	expect_true(all(refvec == newvec))
	expect_true(all(refmat == newmat))
})

test_that("fix_prov_names complains when provided fix_vars with no matches", {
	
})

test_that("fix_prov_names complains when fix_vars aren't the same length as provs", {
	
})