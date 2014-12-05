# create data with NAs and a correlation structure
source("functions.r")
source("fishe.r")

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

all.equal(r_cor, ff_cor_carbon, check.attr = FALSE)

# save(ff_cor, file = file.path(tempdir(), "test_ff.RData"))
# close(ff_cor)
# rm(ff_cor)

# library(ff)
# load(file.path(tempdir(), "test_ff.RData"))
# open(ff_cor)
# ff_cor

# copy
xx <- x
yy <- y

# if missing zero, one otherwise
xx[which(!is.na(xx), arr.ind = TRUE)] <- 1L
xx[which(is.na(xx), arr.ind = TRUE)] <- 0L

yy[which(!is.na(yy), arr.ind = TRUE)] <- 1L
yy[which(is.na(yy), arr.ind = TRUE)] <- 0L

non_missing_pairs <- crossprod(xx, yy)

ff_nm <- block_wise(x = xx,
	                y = yy,
	    size_of_block = 100,
	            ncore = 8,
	        file_name = "test_nm",
	             path = tempdir(),
	              FUN = crossprod,
	            vmode = "short")

ff_nm_carbon <- ff_nm[]
all.equal(non_missing_pairs, ff_nm_carbon, check.attr = FALSE)
table(non_missing_pairs - ff_nm_carbon)

pval <- fisher_to_pval(r_cor, non_missing_pairs)

ff_pval <- block_wise3(x = ff_cor,
	                   y = ff_nm,
	       size_of_block = 100,
	               ncore = 8,
	           file_name = "test_pval",
	                path = tempdir(),
	                 FUN = fisher_to_pval,
	               vmode = "double")

ff_pval_carbon <- ff_pval[]
all.equal(pval, ff_nm_carbon, check.attr = FALSE)
table(pval - ff_pval_carbon)




