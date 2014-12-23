library(ffbw)

context("Correctness of blockwise operations")

# Data generation

xcol <- 890
ycol <- 340
n_row <- 1000
x <- matrix(rnorm(xcol * n_row), ncol = xcol)
y <- matrix(rnorm(ycol * n_row), ncol = ycol)
y[ ,1] <- 5 * x[ ,1] + rnorm(n_row)
colnames(x) <- paste0("x_var_", 1:xcol)
colnames(y) <- paste0("y_var_", 1:ycol)
x[sample(length(x), size = 100)] <- NA
y[sample(length(y), size = 100)] <- NA

# apply-at-once
r_cor <- cor(x, y, use = "pairwise.complete.obs")

ncores <- min(parallel::detectCores(), 2)

# block-wise
ff_cor <- block_wise(x = x,
	                 y = y,
	     size_of_block = 100,
	             ncore = ncores,
	         file_name = "test_ff",
	              path = tempdir(),
	             vmode = "double",
	               FUN = cor,
	               use = "pairwise.complete.obs")

# coerce into memory
ff_cor_carbon <- ff_cor[]

test_that("block_wise and apply-function-at-once give the same results", {
  expect_equal(r_cor, ff_cor_carbon, check.attr = FALSE)
})