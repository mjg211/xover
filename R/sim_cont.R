#' Simulate continuous outcome data from a cross-over trial
#'
#' Facilitates the simulation of continuous outcome data from a cross-over
#' trial, for a given cross-over design.
#'
#' \code{sim_bin()} supports the simulation of continuous outcome data from a
#' cross-over trial. Precisely, a cross-over design of class \code{xover_seq} is
#' provided (see \code{sequences}). Then, the values of \code{treatment},
#' \code{period}, and several other input arguments determines the distribution
#' of the simulated data. See the package vignette for further dettails.
#'
#' @param sequences An object of class \code{xover_seq}, describing the
#' cross-over design for which data will be simulated.
#' @param n Either a single \code{\link[base]{numeric}} integer describing the
#' number of subjects assigned to all of the sequences, or a
#' \code{\link[base]{numeric}} vector of integers having length equal to the
#' number of sequences implied by \code{sequences}, with the elements describing
#' the number of subject assigned to each of the sequences.
#' @param replicates A single \code{\link[base]{numeric}} integer describing the
#' number of replicate datasets to generate.
#' @param intercept A single \code{\link[base]{numeric}} describing the
#' intercept of the GLMM.
#' @param period A \code{\link[base]{numeric}} vector of length equal to one
#' less than the number of periods implied by \code{sequences}, describing the
#' values of each of the period effects.
#' @param treatment A \code{\link[base]{numeric}} vector of length equal to one
#' less than the number of treatments implied by \code{sequences}, describing
#' the values of each of the treatment effects.
#' @param carryover A \code{\link[base]{numeric}} vector of length equal to one
#' less than the number of treatments implied by \code{sequences}, describing
#' the values of each of the carryover effects.
#' @param subject A \code{\link[base]{numeric}} vector of length equal to one
#' less than the number of subjects implied by \code{n} and \code{sequences},
#' describing the values of each of the subject effects.
#' @param sigma_b A single strictly positive \code{\link[base]{numeric}}
#' describing the value of the between subject standard deviation.
#' @param sigma_e A single strictly positive \code{\link[base]{numeric}}
#' describing the value of the residual standard deviation.
#' @param random A \code{\link[base]{logical}} variable indicating whether to
#' treat the subject effects as random or fixed.
#' @param seed A random number seed, to be passed to
#' \code{\link[base]{set.seed}}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[base]{list}} or class \code{xover_sim} containing in
#' particular a \code{\link[base]{list}} \code{sim_data} which holds the
#' simulated data.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_seq}} for converting designs to class
#' \code{xover_seq}.
#' @export
sim_cont <- function(sequences, n = 1, replicates = 1000, intercept, period,
                     treatment, carryover, subject, sigma_b = 1, sigma_e = 1,
                     random = T, seed = Sys.time(), summary = T) {

  ##### Input checking #########################################################

  check_sequences(sequences)
  if (tibble::is_tibble(sequences)) {
    sequences            <- tibble_to_matrix(sequences)
  }
  labels                 <- unique(as.vector(sequences))
  D                      <- length(labels)
  J                      <- ncol(sequences)
  K                      <- nrow(sequences)
  if (!is.numeric(n)) {
    stop("n must be numeric.")
  } else if (!(length(n) %in% c(1, K))) {
    stop("n must either have length one or have length equal to the number of ",
         "sequences.")
  } else if (length(n) == 1) {
    n                    <- rep(n, K)
  }
  N                      <- sum(n)
  check_integer_range(replicates, "replicates", c(0, Inf), 1)
  if (missing(intercept)) {
    intercept            <- c("(intercept)" = 0)
  } else {
    check_real_range_strict(intercept, "intercept", c(-Inf, Inf), 1)
    if (all(!is.null(names(intercept)), names(intercept) != "(intercept)")) {
      warning("The name of intercept will be over-written")
      names(intercept)   <- "(intercept)"
    } else if (is.null(names(intercept))) {
      names(intercept)   <- "(intercept)"
    }
  }
  if (missing(period)) {
    period               <- numeric(J - 1)
    names(period)        <- paste("period", 2:J, sep = "")
  } else {
    check_real_range_strict(period, "period", c(-Inf, Inf), J - 1)
    if (!is.null(names(period))) {
      if (!all(names(period) == paste("period", 2:J, sep = ""))) {
        warning("The name(s) of period will be over-written")
        names(period)    <- paste("period", 2:J, sep = "")
      }
    } else {
      names(period)      <- paste("period", 2:J, sep = "")
    }
  }
  if (missing(treatment)) {
    treatment            <- numeric(D - 1)
    names(treatment)     <- paste("treatment", labels[2:D], sep = "")
  } else {
    check_real_range_strict(treatment, "treatment", c(-Inf, Inf), D - 1)
    if (!is.null(names(treatment))) {
      if (!all(names(treatment) == paste("treatment", labels[2:D], sep = ""))) {
        warning("The name(s) of treatment will be over-written")
        names(treatment) <- paste("treatment", labels[2:D], sep = "")
      }
    } else {
      names(treatment)   <- paste("treatment", labels[2:D], sep = "")
    }
  }
  if (!missing(carryover)) {
    check_real_range_strict(carryover, "carryover", c(-Inf, Inf), D - 1)
    if (!is.null(names(carryover))) {
      if (!all(names(carryover) == paste("carryover", labels[2:D], sep = ""))) {
        warning("The name(s) of carryover will be over-written")
        names(carryover) <- paste("carryover", labels[2:D], sep = "")
      }
    } else {
      names(carryover)   <- paste("carryover", labels[2:D], sep = "")
    }
  }
  check_logical(random, "random")
  if (all(random, !missing(subject))) {
    warning("subject has been changed from default but will not be used based",
            " on the choice of random")
  } else if (all(!random, !missing(subject))) {
    check_real_range_strict(subject, "subject", c(-Inf, Inf), N - 1)
    if (!is.null(names(subject))) {
      if (!all(names(subject) == paste("subject", 2:N, sep = ""))) {
        warning("The name(s) of subject will be over-written")
        names(subject)   <- paste("subject", 2:N, sep = "")
      }
    } else {
      names(subject)     <- paste("subject", 2:N, sep = "")
    }
  } else if (all(!random, missing(subject))) {
    subject              <- numeric(N - 1)
    names(subject)       <- paste("subject", 2:N, sep = "")
  }
  check_real_range_strict(sigma_e, "sigma_e", c(0, Inf), 1)
  check_real_range_strict(sigma_b, "sigma_b", c(0, Inf), 1)
  check_logical(summary, "summary")

  #### Print summary ###########################################################

  if (summary) {

  }

  ##### Main computations ######################################################

  if (summary) {
    message("   Building required objects for data simulation...")
  }
  set.seed(seed)
  seeds                        <- rnorm(replicates)
  collapsed_sequences          <- numeric(K)
  for (k in 1:K) {
    collapsed_sequences[k]     <- paste(sequences[k, ], collapse = "")
  }
  treatment_vector             <- sequence_vector <- sequence_indices <-
                                    numeric(J*N)
  treatment_vector[1:(J*n[1])] <- rep(sequences[1, ], n[1])
  sequence_vector[1:(J*n[1])]  <- rep(collapsed_sequences[1], each = J*n[1])
  sequence_indices[1:(J*n[1])] <- 1
  for (k in 2:K) {
    treatment_vector[(1 + J*sum(n[1:(k - 1)])):(J*sum(n[1:k]))] <-
      rep(sequences[k, ], n[k])
    sequence_vector[(1 + J*sum(n[1:(k - 1)])):(J*sum(n[1:k]))]  <-
      rep(collapsed_sequences[k], n[k])
    sequence_indices[(1 + J*sum(n[1:(k - 1)])):(J*sum(n[1:k]))] <- k
  }
  base_data   <- tibble::tibble(outcome          = NA,
                                subject          = factor(rep(1:N, each = J),
                                                          1:N),
                                period           = factor(rep(1:J, N), 1:J),
                                sequence         = factor(sequence_vector,
                                                     collapsed_sequences),
                                `sequence index` = factor(sequence_indices, 1:K),
                                treatment        = factor(treatment_vector,
                                                          labels))
  if (!missing(carryover)) {
    base_data                  <- add_carryover_data(base_data)
  }
  mean_responses               <- rep(intercept, J*N) + rep(c(0, period), N)
  local_treatment              <- c(0, treatment)
  names(local_treatment)[1]    <- paste("treatment", labels[1], sep = "")
  mean_responses               <- mean_responses +
                                      local_treatment[paste("treatment",
                                                            base_data$treatment,
                                                            sep = "")]
  if (!missing(carryover)) {
    local_carryover            <- c(0, carryover)
    names(local_carryover)[1]  <- paste("carryover", labels[1], sep = "")
    mean_responses             <- mean_responses +
                                    local_carryover[paste("carryover",
                                                          base_data$carryover,
                                                          sep = "")]
  }
  if (!random) {
    mean_responses             <- mean_responses + rep(c(0, subject), each = J)
  }
  names(mean_responses)        <- NULL
  message_production           <- c(numeric(9), replicates)
  for (i in 1:9) {
    message_production[i]      <- max(which((100*(1:replicates)/replicates) ==
                                              10*i))
  }
  if (summary) {
    message("...completed building required objects. Beginning data ",
            "simulation...")
  }
  sim_data                     <- lapply(1:replicates, sim_cont_one_trial,
                                         base_data = base_data, random = random,
                                         J = J, N = N,
                                         mean_responses = mean_responses,
                                         sigma_b = sigma_b, sigma_e = sigma_e,
                                         seeds = seeds,
                                         message_production =
                                           message_production,
                                         summary = summary)
  if (summary) {
    message("...completed data simulation. Preparing outputs...")
  }
  if (missing(carryover)) {
    carryover   <- NULL
  }
  if (missing(subject)) {
    subject     <- NULL
  }

  ##### Outputting #############################################################

  output        <- list(sim_data = sim_data, sequences = sequences, n = n,
                        replicates = replicates, intercept = intercept,
                        period = period, treatment = treatment,
                        carryover = carryover, subject = subject,
                        sigma_b = sigma_b, sigma_e = sigma_e, random = random,
                        seed = seed, summary = summary)
  class(output) <- c(class(output), "xover_sim")
  if (summary) {
    message("...outputting.")
  }
  return(output)

}
