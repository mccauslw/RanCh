# R/examples/RP_example.R

# This small example illustrates the following functions:
# RP_sim, ind_group_stats, compute_RP_binp_funcs

# Set up data
n <- 3 # Number of objects in choice universe
subject <- 1
N <- T_1972_counts["Gambles", subject, , ]
u <- create_universe(3, colnames(N))
Nv <- vectorize(u, N)

# Set up prior distribution and SMC simulation parameters
a <- 4      # Gamma shape parameter
b <- 1.0    # Gamma rate (inverse scale) parameter
alpha_prior <- create_alpha_prior(n, a, b)
J <- 40     # Number of independent particle groups
M <- 120    # Number of particles in each group
set.seed(123)
lambda_values <- seq(0.01, 1.00, by=0.01) # Indices of hybrid models to include
cycle_schedule <- create_cycle_schedule(lambda_values) #

# SMC simulation for hybrid models and the Dirichlet Random Preference model
RP_sim <- run_RP_sim(u, J, M, alpha_prior, Nv, lambda_values, cycle_schedule)

# Posterior statistics for alpha, RP model
quant_p <- c(0.025, 0.05, 0.5, 0.95, 0.975) # Quantiles to report
alpha_stats <- ind_groups_stats(RP_sim$alpha, J, quant_p)

# Posterior density of alpha, RP model
n_alpha_grid <- 40
RP_alpha_funcs <- compute_pdf_cdf_on_grid(RP_sim$alpha, J, n_alpha_grid)
al_pdf <- tibble::as_tibble(RP_alpha_funcs$pdf) # cdf is also possible
al_pdf$ymin <- al_pdf$func + qnorm(0.975) * al_pdf$nse
al_pdf$ymax <- al_pdf$func + qnorm(0.025) * al_pdf$nse
bin_width <- (max(al_pdf$x) - min(al_pdf$x)) / n_alpha_grid
# Kernel density of log(alpha) transformed back to density of alpha
ln_alpha = log(RP_sim$alpha)
pdf_ln_al_kde <- density(ln_alpha, bw = "nrd0", adjust = 2)
al = exp(pdf_ln_al_kde$x)
df <- tibble::tibble(al = al, pdf = pdf_ln_al_kde$y / al)
# Plot histogram with error bars, kernel density
ggplot2::ggplot() +
  ggplot2::geom_col(data = al_pdf, ggplot2::aes(x = x, y = func),
                    width = bin_width, fill = "skyblue", alpha = 0.6) +
  ggplot2::geom_errorbar(data = al_pdf,
                         ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
                         width = bin_width * 0.4) +
  ggplot2::geom_line(data = df, ggplot2::aes(x = al, y = pdf),
                     color = "darkred", linewidth = 1.2) +
  ggplot2::labs(x = "x", y = "Density", title = "Histogram of alpha") +
  ggplot2::theme_minimal()

# Posterior density of the binary choice probability p(x,z), RP model
p_grid <- seq(0, 1, by=1/80) # Grid of binary choice probabilites
RP_binp_funcs <- compute_RP_binp_funcs(u, RP_sim$gamma, J, Nv, p_grid)
xz_pdf <- tibble::as_tibble(RP_binp_funcs[[2]]$pdf) # posterior pdf function for p(x,z)
xz_pdf$upper <- xz_pdf$func + qnorm(0.995) * xz_pdf$nse
xz_pdf$lower <- xz_pdf$func + qnorm(0.005) * xz_pdf$nse
ggplot2::ggplot(xz_pdf, ggplot2::aes(x = x)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                       fill = "green", alpha = 0.2) +
  ggplot2::geom_line(ggplot2::aes(y = func), color = "black") +
  ggplot2::labs(x = "p(x,z)", y = "density") +
  ggplot2::theme_minimal()
