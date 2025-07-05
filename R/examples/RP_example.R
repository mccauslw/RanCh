# R/examples/RP_example.R
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
J <- 60
M <- 100
lambda_values <- seq(0.01, 1.00, by=0.01)
cycle_schedule <- create_cycle_schedule(lambda_values)
RP_sim <- run_RP_sim(u, J, M, alpha_prior, Nv, lambda_values, cycle_schedule)

# Extract posterior mean and quantiles, numerical errors, etc.
alpha_stats <- ind_groups_stats(RP_sim$alpha, J, quant_p)

# Extract posterior densities of p(x,y) and p(x,z) and plot them
RP_binp_funcs <- compute_RP_binp_funcs(u, RP_sim$gamma, J, Nv, p_grid)
xy_pdf <- RP_binp_funcs[[1]]$pdf # posterior pdf function for p(x,y)
plot(xy_pdf$x, xy_pdf$func, 'l', xlab = "p(x,y)", ylab = "density value")
lines(xy_pdf$x, xy_pdf$func + xy_pdf$nse, col='red')
lines(xy_pdf$x, xy_pdf$func - xy_pdf$nse, col='red')
xz_pdf <- RP_binp_funcs[[2]]$pdf # posterior pdf function for p(x,z)
plot(xz_pdf$x, xz_pdf$func, 'l', xlab = "p(x,z)", ylab = "density value")
lines(xz_pdf$x, xz_pdf$func + xz_pdf$nse, col='red')
lines(xz_pdf$x, xz_pdf$func - xz_pdf$nse, col='red')

# Extract posterior density of alpha and plot it
RP_alpha_funcs <- compute_pdf_cdf_on_grid(RP_sim$alpha, J, n_alpha_grid)
al_pdf <- RP_alpha_funcs$pdf # cdf is also possible
plot(al_pdf$x, al_pdf$func, 'l', xlab = "alpha", ylab = "density value")
lines(al_pdf$x, al_pdf$func + al_pdf$nse, col='red')
lines(al_pdf$x, al_pdf$func - al_pdf$nse, col='red')
