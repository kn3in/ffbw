# to split rectangular matrix into blocks,
# create block indices of size size_of_block by size_of_block
# e.g. for a correlation matrix of two matrices with ncol_x and ncol_y columns. 

split_into_blocks <- function(size_of_block, ncol_x, ncol_y) {
  # final correlation matrix will have rows mapped to
  # columns of x and colums to columns of y
  row_reminder <- ncol_x %% size_of_block
  col_reminder <- ncol_y %% size_of_block

  nrow_blocks <- ncol_x %/% size_of_block
  ncol_blocks <- ncol_y %/% size_of_block
  
  row_group <- rep(1:nrow_blocks, each = size_of_block)
  col_group <- rep(1:ncol_blocks, each = size_of_block)

  if(row_reminder > 0) {
  	row_group <- c(row_group, rep(nrow_blocks + 1, row_reminder))
  }

  if(col_reminder > 0) {
    col_group <- c(col_group, rep(ncol_blocks + 1, col_reminder))
  }
  
  row_split <- split(1:ncol_x, row_group)
  col_split <- split(1:ncol_y, col_group)
  
  block_indices <- as.matrix(expand.grid(1:length(row_split), 1:length(col_split)))

  apply(block_indices, 1, function(x) {
  	list(r_ind = row_split[[x[1]]], c_ind = col_split[[x[2]]])
  })
}

# prepare ff matrix
reserve_space <- function(base_path, file_name, nrow_out, ncol_out, row_names, col_names, ...) {
  final <- ff(dim = c(nrow_out, ncol_out), filename = file.path(base_path, file_name), ...)   
    rownames(final) <- row_names
    colnames(final) <- col_names
    final
}


