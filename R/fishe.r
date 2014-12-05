# Fisher Z transformation
fisher_z <- function(rho) {
  0.5 * log((1 + rho) / (1 - rho))
}
# Asymptotic p-value
fisher_to_pval <- function(rho, samp_size) {
	2 * pnorm(fisher_z(-abs(rho)) * sqrt(samp_size - 3))
}