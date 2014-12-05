# create data with NAs and a correlation structure

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
r_cor <- cor(x, y, use = "pairwise.complete.obs")
#------------------------------------------------------------------------------

ff_cor <- block_wise(x = x,
	                  y = y,
	      size_of_block = 100,
	              ncore = 8,
	         file_name = "test_ff",
	              path = tempdir(),
	             vmode = "double",
	               FUN = cor,
	               use = "pairwise.complete.obs")

ff_cor_carbon <- ff_cor[]
attr(ff_cor_carbon, "Csingle") <- NULL

all.equal(r_cor, ff_cor_carbon)
save(ff_cor, file = file.path(tempdir(), "test_ff.RData"))
close(ff_cor)
rm(ff_cor)

library(ff)
load(file.path(tempdir(), "test_ff.RData"))
open(ff_cor)
ff_cor