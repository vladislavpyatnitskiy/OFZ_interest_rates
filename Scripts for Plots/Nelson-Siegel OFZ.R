nelson_siegel_fit <- function(maturities, yields) {
  
  # Nelson–Siegel function
  nelson_siegel <- function(tau, beta0, beta1, beta2, tau0) {
    tau <- pmax(tau, 0.001)
    factor1 <- (1 - exp(-tau / tau0)) / (tau / tau0)
    factor2 <- factor1 - exp(-tau / tau0)
    beta0 + beta1 * factor1 + beta2 * factor2
  }
  
  # Objective function: sum of squared errors
  objective <- function(params, tau, yields) {
    beta0 <- params[1]
    beta1 <- params[2]
    beta2 <- params[3]
    tau0  <- params[4]
    fitted <- nelson_siegel(tau, beta0, beta1, beta2, tau0)
    sum((yields - fitted)^2)
  }
  
  # Initial guesses
  start_params <- c(mean(yields), -1, -1, 3)
  
  # Fit model
  fit <- optim(
    par = start_params,
    fn = objective,
    tau = maturities,
    yields = yields,
    method = "BFGS"
  )
  
  # Extract fitted parameters
  ns_params <- fit$par
  
  # Compute fitted yields on a fine grid
  maturity_fine <- seq(min(maturities), max(maturities), length.out = 100)
  ns_yields <- nelson_siegel(
    maturity_fine, ns_params[1], ns_params[2], ns_params[3], ns_params[4])
  
  # Return results as list
  list(
    params = ns_params,
    fitted_curve = data.frame(Maturity = maturity_fine, Yield = ns_yields)
  )
  
  plot(
    maturities,
    yields,
    pch = 19,
    col = "blue",
    xlab = "Maturity (years)",
    ylab = "Yield (%)",
    main = "Nelson–Siegel Fit to Yield Curve",
    xpd = F,
    las = 1
  )
  
  lines(maturity_fine, ns_yields, col = "red", lwd = 3)
  
  grid(nx = NULL, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  legend(
    "bottomright",
    legend = c("Observed", "Fitted"),
    xpd = T,
    bty = "n",
    col = c("blue", "red"),
    pch = c(19, NA),
    lty = c(NA, 1),
    cex = .75,
    seg.len = 0.8,
    y.intersp = 0.25,
    lwd = 2
  )
}
nelson_siegel_fit(c(0.25, 0.5, 0.75, 1, 2, 3, 5, 7, 10, 15, 20, 30),
                  c(14.29, 14.09, 14.02, 14.01, 14.18, 14.25, 14.35, 14.42, 14.50, 14.60, 14.65, 14.70))
