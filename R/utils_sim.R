sim_bin_one_trial <- function(rep, base_data, random, J, N, mean_responses,
                              sigma_b, seeds, message_production, summary) {
  set.seed(seeds[rep])
  data             <- base_data
  if (random) {
    mean_responses <- mean_responses + rnorm(N, sd = sigma_b)[base_data$subject]
  } else if (all(!random, sigma_b != 0)) {
    mean_responses <- mean_responses + stats::rnorm(N*J, sd = sigma_b)
  }
  data$outcome     <- factor(rbinom(n = 1:nrow(data), size = rep(1, N),
                                    prob = stats::plogis(mean_responses)), 0:1)
  if (summary) {
    if (rep %in% message_production) {
      message("...data for ", round(100*rep/message_production[10]),
              "% of the replicates generated...")
    }
  }
  class(data)      <- c(class(data), "xover_data")
  return(data)
}

sim_cont_one_trial <- function(rep, base_data, random, J, N, mean_responses,
                               sigma_b, sigma_e, seeds, message_production,
                               summary) {
  set.seed(seeds[rep])
  data             <- base_data
  if (random) {
    mean_responses <- mean_responses + rnorm(N, sd = sigma_b)[base_data$subject]
  } else if (all(!random, sigma_b != 0)) {
    mean_responses <- mean_responses + stats::rnorm(N*J, sd = sigma_b)
  }
  data$outcome     <- stats::rnorm(mean_responses, sd = sigma_e)
  if (summary) {
    if (rep %in% message_production) {
      message("...data for ", round(100*rep/message_production[10]),
              "% of the replicates generated...")
    }
  }
  class(data)      <- c(class(data), "xover_data")
  return(data)
}
