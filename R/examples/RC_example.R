# R/examples/RC_example.R
n <- 3 # Number of objects in choice universe

# Grids for function evaluation
p_grid <- seq(0, 1, by=1/80)
n_alpha_grid <- 40

# Group quantiles to compute
quant_p <- c(0.025, 0.05, 0.5, 0.95, 0.975)

# Set up prior, data, simulation parameters for a small (n=3) example
alpha_prior <- create_alpha_prior(n, 4, 0.1)
N <- T_1972_counts["Gambles", 1, , ] # Only choices from menus xz and xyz
u <- create_universe(n, object_names=dimnames(N)[[2]])
Nv <- vectorize(u, N)
J <- 40
M <- 1000
RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nv)

# Extract posterior mean and quantiles, numerical errors, etc.
alpha_stats <- ind_groups_stats(RC_sim$alpha, J, quant_p)

# Extract posterior densities of p(x,y) and p(x,z) and plot them
RC_binp_funcs <- compute_RC_binp_funcs(u, RC_sim$alpha, J, Nv, p_grid)
plot(RC_binp_funcs[[1]]$pdf$x, RC_binp_funcs[[1]]$pdf$func, 'l')
plot(RC_binp_funcs[[2]]$pdf$x, RC_binp_funcs[[2]]$pdf$func, 'l')

# Extract posterior density of alpha and plot it
RC_alpha_funcs <- compute_pdf_cdf_on_grid(RC_sim$alpha, J, n_alpha_grid)
plot(RC_alpha_funcs$pdf$x, RC_alpha_funcs$pdf$func, 'l')
