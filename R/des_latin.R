#' @export
des_latin <- function(D = 2, alpha = 0.05, beta = 0.2, delta = 0.2, sigma_e = 1,
                      mcp = "dunnett", williams = F, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(delta, "delta", c(0, Inf), 1)
  check_real_range_strict(sigma_e, "sigma_e", c(0, Inf), 1)
  check_belong(mcp, "mcp", c("bonferroni", "dunnett", "none"))
  check_logical(williams, "williams")
  check_logical(summary, "summary")
  if (all(D == 2, mcp != "dunnett")) {
    warning("mcp has been changed from default but will not be used based on ",
            "the value of D.")
  }

  ##### Main computations ######################################################

  if (mcp == "bonferroni") {
    alpha_star  <- alpha/(D - 1)
  } else if (mcp == "dunnett") {
    alpha_star  <- mvtnorm::qmvnorm(1 - alpha,
                                    sigma = diag(0.5, D - 1, D - 1) +
                                              matrix(0.5, D - 1, D - 1))
  } else {
    alpha_star  <- alpha
  }
  N_exact       <- 2*((qnorm(1 - alpha_star) + qnorm(1 - beta))^2*sigma_e^2)/
                     (delta^2)
  sequences     <- seq_williams(D, summary = F)
  K             <- nrow(sequences)
  n_exact       <- rep(N_exact/(K*D), K)
  n             <- ceiling(n)
  N             <- sum(n)

  ##### Outputting #############################################################

  output        <- list(des = list(N = N, N_exact, n = n, n_exact = n_exact,
                                   sequences = sequences, alpha = alpha,
                                   beta = beta, delta = delta,
                                   sigma_e = sigma_e), D = D, alpha = alpha,
                        beta = beta, delta = delta, sigma_e = sigma_e,
                        mcp = mcp, williams = williams, summary = summary)
  class(output) <- c(class(output), "des_latin")
  return(output)

}
