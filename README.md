ffbw
====

R package for blockwise operations on ff matrices

## Installation

```R
# install.packages("devtools")
devtools::install_github("kn3in/ffbw")
```

## Usage

```R
library(ffbw)

# Generate data
xcol <- 890
ycol <- 340
n_row <- 1000
x <- matrix(rnorm(xcol * n_row), ncol = xcol)
y <- matrix(rnorm(ycol * n_row), ncol = ycol)

# Correlations between columns of `x` and `y`
ff_cor <- block_wise(x = x,
	                 y = y,
	     size_of_block = 100,
	             ncore = 8,
	         file_name = "test_ff",
	              path = tempdir(),
	             vmode = "double",
	               FUN = cor,
	               use = "pairwise.complete.obs")
```