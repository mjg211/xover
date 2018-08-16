#' Supported cross-over designs
#'
#' Determine the cross-over designs supported by the \code{seq_#()} functions in
#' \code{\link[xover]{xover}}.
#'
#' \code{seq_all()} supports the determination of the cross-over designs
#' supported by the \code{seq_#()} functions in \code{\link[xover]{xover}} for a
#' specified number of treatments (see \code{D}), periods (see \code{J}), and
#' sequences (see \code{K}).
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{2}.
#' @param J The number of periods. Must be a \code{\link[base]{numeric}} vector
#' of integers, whose values are greater than or equal to two. Defaults to
#' \code{2}.
#' @param K The number of sequences. Must be a \code{\link[base]{numeric}}
#' vector of integers, whose values are greater than or equal to two. Defaults
#' to \code{2}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return If at least one supported design is identified for the chosen input
#' parameters, a \code{\link[tibble]{tibble}} describing the supported
#' sequences. Otherwise, it will be \code{\link[base]{NULL}}. In either case,
#' the returned object will have class \code{xover_seq}.
#' @examples
#' # Available sequences for three-treatment three-period trials with between
#' # two and 20 sequences
#' three_by_three        <- seq_all(D = 3, J = 3, K = 2:20)
#' # Available sequences for three-treatment two-period trials with between
#' # two and 20 sequences
#' three_by_two          <- seq_all(D = 3, J = 2, K = 2:20)
#' # Available sequences for three-treatment two- or three-period trials with
#' between two and 20 sequences
#' three_by_two_or_three <- seq_all(D = 3, J = c(2, 3), K = 2:20)
#' @export
seq_all <- function(D = 2, J = 2, K = 2, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_integer_range(J, "J", c(1, Inf), "any")
  check_integer_range(K, "K", c(1, Inf), "any")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    message("-----")
    message("Supported cross-over designs")
    message("-----\n")
  }
  message("   You have chosen to search for the supported cross-over designs ",
          "with:\n")
  message("      \u2022 D = ", D, ";")
  if (length(J) == 1) {
    message("      \u2022 J = ", J, ";")
  } else if (length(J) <= 10) {
    message("      \u2022 J \u2208 {", paste(J, collapse = ", "), "};")
  } else {
    message("      \u2022 J \u2208 {", paste(J[1:9], collapse = ", "), ",...,",
            J[length(J)], "};")
  }
  if (length(K) == 1) {
    message("      \u2022 K = ", K, ".")
  } else if (length(K) <= 10) {
    message("      \u2022 K \u2208 {", paste(K, collapse = ", "), "}.")
  } else {
    message("      \u2022 K \u2208 {", paste(K[1:9], collapse = ", "), ",...,",
            K[length(K)], "}.")
  }

  ##### Main computations ######################################################

  if (summary) {
    message("\n   Beginning the search for compatible designs...")
  }
  supported   <- NULL
  if (summary) {
    message("...considering possibilities from seq_anderson_preece()...")
  }
  if (all(D == 7, 7 %in% J, 42 %in% K)) {
    supported <- c("seq_anderson_preece", 7, 7, 42, "")
  }
  if (summary) {
    message("...considering possibilities from seq_anderson()...")
  }
  if (all(D == 7, 14 %in% J, 42 %in% K)) {
    supported <- rbind(supported, c("seq_anderson", 7, 14, 42, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_archdeacon()...")
  }
  if (all(D == 9, 9 %in% J, 9 %in% K)) {
    supported <- rbind(supported, c("seq_archdeacon", 9, 9, 9, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_atkinson()...")
  }
  if (all(D == 3, 6 %in% J, 6 %in% K)) {
    supported <- rbind(supported, c("seq_atkinson", 3, 6, 6, ""))
  } else if (all(D == 4, 6 %in% J, 6 %in% K)) {
    supported <- rbind(supported, c("seq_atkinson", 4, 8, 4, ""))
  } else if (all(D == 5, 10 %in% J, 10 %in% K)) {
    supported <- rbind(supported, c("seq_atkinson", 5, 10, 10, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_balaam()...")
  }
  if (all(D == 3, 2 %in% J, 9 %in% K)) {
    supported <- rbind(supported, c("seq_balaam", 3, 2, 9, ""))
  } else if (all(D == 4, 2 %in% J, 16 %in% K)) {
    supported <- rbind(supported, c("seq_balaam", 4, 2, 16, ""))
  } else if (all(D == 5, 2 %in% J, 25 %in% K)) {
    supported <- rbind(supported, c("seq_balaam", 5, 2, 25, ""))
  } else if (all(D == 6, 2 %in% J, 36 %in% K)) {
    supported <- rbind(supported, c("seq_balaam", 5, 2, 36, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_balmin_rmd()...")
  }
  possible_J        <- J[which((D - 1)%%(J - 1) == 0)]
  if (length(possible_J) > 0) {
    corresponding_K <- D*(D - 1)/(possible_J - 1)
    possible_J      <- possible_J[which(corresponding_K %in% K)]
    if (length(possible_J) > 0) {
      for (j in possible_J) {
        supported   <- rbind(supported,
                             c("seq_balmin_rmd", D, j, D*(D - 1)/(j - 1), ""))
      }
    }
  }
  if (summary) {
    message("...considering possibilities from seq_bate_jones()...")
  }
  if (all(D == 5, 10 %in% J, 15 %in% K)) {
    supported <- rbind(supported, c("seq_bate_jones", 5, 10, 15, ""))
  } else if (all(D == 8, 16 %in% J, 16 %in% K)) {
    supported <- rbind(supported, c("seq_bate_jones", 8, 16, 16, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_berenblut()...")
  }
  if (all(D == 3, 6 %in% J, 9 %in% K)) {
    supported   <- rbind(supported, c("seq_berenblut", 3, 6, 9, ""))
  } else if (D == 4) {
    if (all(8 %in% J, 16 %in% K)) {
      supported <- rbind(supported, c("seq_berenblut", 4, 8, 16, ""))
    }
    if (all(10 %in% J, 25 %in% K)) {
      supported <- rbind(supported, c("seq_berenblut", 5, 10, 25, ""))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_bib()...")
  }
  if (D > 2) {
    JK            <- expand.grid(J = J, K = K)
    conditions    <- cbind(JK$J*JK$K/D, JK$J*JK$K*(JK$J - 1)/(D*(D - 1)))%%1
    JK            <- JK[rowSums(conditions) == 0, ]
    if (nrow(JK) > 0) {
      for (i in 1:nrow(JK)) {
        supported <- rbind(supported, c("seq_bib", D, JK[i, 1], JK[i, 2], ""))
      }
    }
  }
  if (summary) {
    message("...considering possibilities from seq_blaisdell_raghavarao()...")
  }
  if (all(D == 6, 3 %in% J, 24 %in% K)) {
    supported <- rbind(supported, c("seq_blaisdell_raghavarao", 6, 3, 24, ""))
  } else if (all(D == 8, 5 %in% J, 8 %in% K)) {
    supported <- rbind(supported, c("seq_blaisdell_raghavarao", 8, 5, 8, ""))
  } else if (all(D == 9, 4 %in% J, 18 %in% K)) {
    supported <- rbind(supported, c("seq_blaisdell_raghavarao", 9, 4, 18, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_clatworthy()...")
  }
  clatworthy    <- summary_seq_clatworthy(D = D, J = J, K = K)
  if (nrow(clatworthy) > 0) {
    for (i in 1:nrow(clatworthy)) {
      supported <- rbind(supported,
                         c("seq_claworthy", clatworthy[i, 3:5],
                           paste("type = ", clatworthy[i, 1], ", selection = ",
                                 clatworthy[i, 2], sep = "")))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_combs()...")
  }
  poss_J_oc_rep   <- expand.grid(J = J, max_occurrences = 1:max(J),
                                 max_repetitions = 1:max(J))
  poss_J_oc_rep   <- dplyr::filter(poss_J_oc_rep, max_occurrences <= J &
                                     max_repetitions <= max_occurrences &
                                     (J <= D | (J > D & max_occurrences > 1)))
  for (i in 1:nrow(poss_J_oc_rep)) {
    sequences_i   <- seq_combs(D = D, J = poss_J_oc_rep[i, 1],
                               max_occurrences = poss_J_oc_rep[i, 2],
                               max_repetitions = poss_J_oc_rep[i, 3])
    if (!is.null(sequences_i)) {
      if (nrow(sequences_i) %in% K) {
        supported <-
          rbind(supported,
                c("seq_combs", D, poss_J_oc_rep[i, 1], nrow(sequences_i),
                  paste("max_occurrences = ", poss_J_oc_rep[i, 2],
                        ", max_repetitions = ", poss_J_oc_rep[i, 3], sep = "")))
      }
    }
  }
  if (summary) {
    message("...considering possibilities from seq_davis_hall()...")
  }
  if (D == 6) {
    if (all(3 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 6, 3, 12, "selection = 1"))
    }
    if (all(4 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 6, 4, 12, "selection = 2"))
    }
    if (all(5 %in% J, 6 %in% K)) {
      supported  <- rbind(supported,
                          c("seq_davis_hall", 6, 5, 6, "selection = 3"))
    }
  } else if (D == 7) {
    if (all(3 %in% J, 14 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 7, 3, 14, "selection = 1"))
    }
    if (all(4 %in% J, 14 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 7, 4, 14, "selection = 2"))
    }
    if (all(5 %in% J, 7 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 7, 5, 7, "selection = 3"))
    }
  } else if (D == 8) {
    if (all(3 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 8, 3, 16, "selection = 1"))
    }
    if (all(4 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 8, 4, 16, "selection = 2"))
    }
    if (all(5 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 8, 5, 8, "selection = 3"))
    }
  } else if (D == 9) {
    if (all(3 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 9, 3, 18, "selection = 1"))
    }
    if (all(4 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 9, 4, 18, "selection = 2"))
    }
    if (all(5 %in% J, 9 %in% K)) {
      supported <- rbind(supported,
                         c("seq_davis_hall", 9, 5, 9, "selection = 3"))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_federer_atkinson()...")
  }
  if (D == 3) {
    if (all(4 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_federer_atkinson", 3, 4, 6, "selection = 1"))
    }
    if (all(7 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_federer_atkinson", 3, 7, 6, "selection = 2"))
    }
  } else if (D == 4) {
    if (all(5 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_federer_atkinson", 4, 5, 12, "selection = 1"))
    }
    if (all(9 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_federer_atkinson", 4, 9, 12, "selection = 2"))
    }
  } else if (all(D == 5, 6 %in% J, 20 %in% K)) {
    supported   <- rbind(supported,
                         c("seq_federer_atkinson", 5, 6, 20, "selection = 1"))
  }
  if (summary) {
    message("...considering possibilities from seq_fletcher()...")
  }
  if (D == 4) {
    if (all(3 %in% J, 4 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 4, 3, 4, "selection = 1"))
    }
    if (all(3 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 4, 3, 8, "selection = 2"))
    }
    if (all(4 %in% J, 4 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 4, 4, 4, "selection = 3"))
    }
    if (all(4 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 4, 4, 8, "selection = 4"))
    }
  } else if (D == 6) {
    if (all(4 %in% J, 6 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 6, 4, 6, "selection = 1"))
    }
    if (all(4 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 6, 4, 12, "selection = 2"))
    }
    if (all(5 %in% J, 6 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 6, 5, 6, "selection = 3"))
    }
  } else if (D == 8) {
    if (all(4 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 8, 4, 8, "selection = 1"))
    }
    if (all(4 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 8, 4, 16, "selection = 2"))
    }
    if (all(5 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 8, 5, 8, "selection = 3"),
                         c("seq_fletcher", 8, 5, 8, "selection = 7"))
    }
    if (all(5 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 8, 5, 16, "selection = 4"),
                         c("seq_fletcher", 8, 5, 16, "selection = 8"))
    }
    if (all(6 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 8, 6, 8, "selection = 5"),
                         c("seq_fletcher", 8, 6, 8, "selection = 10"))
    }
    if (all(6 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 8, 6, 16, "selection = 6"),
                         c("seq_fletcher", 8, 6, 16, "selection = 11"))
    }
    if (all(5 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 8, 5, 24, "selection = 9"))
    }
    if (all(6 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 8, 6, 24, "selection = 12"))
    }
  } else if (D == 9) {
    if (all(4 %in% J, 9 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 9, 4, 9, "selection = 1"))
    }
    if (all(4 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 4, 18, "selection = 2"))
    }
    if (all(4 %in% J, 27 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 4, 27, "selection = 3"))
    }
    if (all(5 %in% J, 9 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 9, 5, 9, "selection = 4"))
    }
    if (all(5 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 5, 18, "selection = 5"))
    }
    if (all(5 %in% J, 27 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 5, 27, "selection = 6"))
    }
    if (all(6 %in% J, 9 %in% K)) {
      supported <- rbind(supported, c("seq_fletcher", 9, 6, 9, "selection = 7"))
    }
    if (all(6 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 6, 18, "selection = 8"))
    }
    if (all(6 %in% J, 27 %in% K)) {
      supported <- rbind(supported,
                         c("seq_fletcher", 9, 6, 27, "selection = 9"))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_iqbal_jones()...")
  }
  if (D == 3) {
    if (all(3 %in% J, 9 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 3, 3, 9, "selection = 1"))
    }
    if (all(4 %in% J, 3 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 3, 4, 3, "selection = 2"))
    }
    if (all(5 %in% J, 3 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 3, 5, 3, "selection = 3"))
    }
    if (all(6 %in% J, 3 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 3, 6, 3, "selection = 4"))
    }
    if (all(7 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 3, 7, 6, "selection = 5"))
    }
  } else if (D == 4) {
    if (all(3 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 4, 3, 8, "selection = 1"))
    }
    if (all(6 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 4, 6, 12, "selection = 2"))
    }
    if (all(3 %in% J, 10 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 4, 3, 10, "selection = 3"))
    }
  } else if (D == 5) {
    if (all(4 %in% J, 5 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 5, 4, 5, "selection = 1"))
    }
    if (all(4 %in% J, 10 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 5, 4, 10, "selection = 2"))
    }
    if (all(5 %in% J, 15 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 5, 5, 15, "selection = 3"))
    }
    if (all(6 %in% J, 5 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 5, 6, 5, "selection = 4"))
    }
  } else if (D == 6) {
    if (all(3 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 6, 3, 18, "selection = 1"))
    }
    if (all(4 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 6, 4, 6, "selection = 2"))
    }
    if (all(4 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 6, 4, 18, "selection = 3"))
    }
    if (all(5 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 6, 5, 12, "selection = 4"))
    }
    if (all(7 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 6, 7, 30, "selection = 5"))
    }
  } else if (D == 7) {
    if (all(4 %in% J, 7 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 4, 7, "selection = 1"))
    }
    if (all(4 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 4, 21, "selection = 2"))
    }
    if (all(5 %in% J, 14 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 5, 14, "selection = 3"))
    }
    if (all(6 %in% J, 7 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 6, 7, "selection = 4"))
    }
    if (all(6 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 6, 21, "selection = 5"))
    }
    if (all(7 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 7, 7, 21, "selection = 6"))
    }
  } else if (D == 8) {
    if (all(4 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 8, 4, 8, "selection = 1"))
    }
    if (all(4 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 8, 4, 24, "selection = 2"))
    }
    if (all(5 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 8, 5, 16, "selection = 3"))
    }
    if (all(6 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 8, 6, 8, "selection = 4"))
    }
    if (all(7 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_iqbal_jones", 8, 7, 8, "selection = 5"))
    }
    if (all(7 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                             c("seq_iqbal_jones", 8, 7, 24, "selection = 6"))
    }
  } else if (all(D == 9, 3 %in% J, 27 %in% K)) {
    supported   <- rbind(supported,
                         c("seq_iqbal_jones", 9, 3, 27, "selection = 1"))
  } else if (all(D == 10, 6 %in% J, 10 %in% K)) {
    supported   <- rbind(supported,
                         c("seq_iqbal_jones", 10, 6, 10, "selection = 1"))
  }
  if (summary) {
    message("...considering possibilities from seq_latin()...")
  }
  if (all(D %in% J, D %in% K)) {
    supported <- rbind(supported, c("seq_latin", D, D, D, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_lewis()...")
  }
  if (D == 4) {
    if (all(3 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 4, 3, 8, "selection = 1"),
                         c("seq_lewis", 4, 3, 8, "selection = 2"))
    }
    if (all(4 %in% J, 8 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 4, 4, 8, "selection = 3"),
                         c("seq_lewis", 4, 4, 8, "selection = 4"),
                         c("seq_lewis", 4, 4, 8, "selection = 5"),
                         c("seq_lewis", 4, 4, 8, "selection = 6"))
    }
  } else if (D == 6) {
    if (all(3 %in% J, 24 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 6, 3, 24, "selection = 1"),
                         c("seq_lewis", 6, 3, 24, "selection = 2"),
                         c("seq_lewis", 6, 3, 24, "selection = 5"),
                         c("seq_lewis", 6, 3, 24, "selection = 6"))
    }
    if (all(3 %in% J, 12 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 6, 3, 12, "selection = 3"),
                         c("seq_lewis", 6, 3, 12, "selection = 4"))
    }
    if (all(4 %in% J, 6 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 6, 4, 6, "selection = 7"),
                         c("seq_lewis", 6, 4, 6, "selection = 8"))
    }
  } else if (all(D == 8, 4 %in% J, 16 %in% K)) {
    supported   <- rbind(supported, c("seq_lewis", 8, 4, 16, "selection = 1"),
                         c("seq_lewis", 8, 4, 16, "selection = 2"),
                         c("seq_lewis", 8, 4, 16, "selection = 3"),
                         c("seq_lewis", 8, 4, 16, "selection = 4"))
  } else if (D == 9) {
    if (all(3 %in% J, 36 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 9, 3, 36, "selection = 1"))
    }
    if (all(4 %in% J, 36 %in% K)) {
      supported <- rbind(supported, c("seq_lewis", 9, 4, 36, "selection = 2"))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_lucas()...")
  }
  if (all(D == 3, 3 %in% J, 6 %in% K)) {
    supported <- rbind(supported, c("seq_lucas", 3, 3, 6, ""))
  } else if (all(D == 4, 3 %in% J, 12 %in% K)) {
    supported <- rbind(supported, c("seq_lucas", 4, 3, 12, ""))
  } else if (all(D == 5, 3 %in% J, 20 %in% K)) {
    supported <- rbind(supported, c("seq_lucas", 5, 3, 20, ""))
  } else if (all(D == 6, 3 %in% J, 30 %in% K)) {
    supported <- rbind(supported, c("seq_lucas", 6, 3, 30, ""))
  } else if (all(D == 7, 3 %in% J, 42 %in% K)) {
    supported <- rbind(supported, c("seq_lucas", 7, 3, 42, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_mols()...")
  }
  if (all(D %in% c(3, 4, 5, 7, 8, 9, 11, 13, 16, 17, 19, 23, 25, 27, 29, 31, 32,
                   37, 41, 43, 47, 49, 53, 59, 61, 64, 67, 71, 73, 79, 81, 83,
                   89, 97), (D*(D - 1)) %in% K)) {
    possible_J    <- J[J <= D]
    if (length(possible_J) > 0) {
      for (j in possible_J) {
        supported <- rbind(supported,
                           c("seq_mols", D, j, D*(D - 1), 'type = "single"'))
      }
    }
  }
  if (summary) {
    message("...considering possibilities from seq_patterson_lucas()...")
  }
  if (D == 3) {
    if (all(2 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 3, 2, 6, "selection = 1"))
    }
    if (all(3 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 3, 3, 6, "selection = 30"))
    }
    if (all(4 %in% J, 6 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 3, 4, 6, "selection = 31"))
    }
  } else if (D == 4) {
    if (all(2 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 2, 12, "selection = 3"))
    }
    if (all(3 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 3, 12, "selection = 4"),
                         c("seq_patterson_lucas", 4, 3, 12, "selection = 32"))
    }
    if (all(4 %in% J, 4 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 4, 4, "selection = 5"))
    }
    if (all(4 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 4, 12, "selection = 33"))
    }
    if (all(5 %in% J, 4 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 5, 4, "selection = 34"))
    }
    if (all(5 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 5, 12, "selection = 35"))
    }
    if (all(3 %in% J, 8 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 4, 3, 8, "selection = 153"))
    }
  } else if (D == 5) {
    if (all(2 %in% J, 20 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 2, 20, "selection = 7"))
    }
    if (all(3 %in% J, 20 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 3, 20, "selection = 8"),
                         c("seq_patterson_lucas", 5, 3, 20, "selection = 36"))
    }
    if (all(4 %in% J, 20 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 4, 20, "selection = 9"),
                         c("seq_patterson_lucas", 5, 4, 20, "selection = 37"))
    }
    if (all(5 %in% J, 10 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 5, 10, "selection = 10"))
    }
    if (all(3 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 3, 42, "selection = 18"))
    }
    if (all(5 %in% J, 20 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 5, 20, "selection = 38"))
    }
    if (all(6 %in% J, 10 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 6, 10, "selection = 39"))
    }
    if (all(6 %in% J, 20 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 6, 20, "selection = 40"))
    }
    if (all(2 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 2, 30, "selection = 125"))
    }
    if (all(3 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 3, 30, "selection = 126"),
                         c("seq_patterson_lucas", 5, 3, 30, "selection = 131"))
    }
    if (all(4 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 4, 30, "selection = 132"))
    }
    if (all(3 %in% J, 10 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 5, 3, 10, "selection = 154"))
    }
  } else if (D == 6) {
    if (all(2 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 2, 30, "selection = 12"))
    }
    if (all(5 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 5, 30, "selection = 13"))
    }
    if (all(6 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 6, 30, "selection = 15"),
                         c("seq_patterson_lucas", 6, 6, 30, "selection = 42"))
    }
    if (all(3 %in% J, 30 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 3, 30, "selection = 41"))
    }
    if (all(2 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 2, 24, "selection = 99"))
    }
    if (all(3 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 3, 24, "selection = 100"),
                         c("seq_patterson_lucas", 6, 3, 24, "selection = 133"))
    }
    if (all(4 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 4, 12, "selection = 101"))
    }
    if (all(4 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 4, 24, "selection = 134"))
    }
    if (all(5 %in% J, 12 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 5, 12, "selection = 135"))
    }
    if (all(3 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 6, 3, 18, "selection = 155"))
    }
  } else if (D == 7) {
    if (all(2 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 2, 42, "selection = 16"))
    }
    if (all(3 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 3, 21, "selection = 17"))
    }
    if (all(4 %in% J, 14 %in% K)) {
      supported <- rbind(supported, c("seq_patterson_lucas", 7, 4, 14,
                                          "selection = 19"))
    }
    if (all(4 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 4, 42, "selection = 20"),
                         c("seq_patterson_lucas", 7, 4, 42, "selection = 45"))
    }
    if (all(5 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 5, 21, "selection = 21"))
    }
    if (all(5 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 5, 42, "selection = 22"),
                         c("seq_patterson_lucas", 7, 5, 42, "selection = 47"))
    }
    if (all(6 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 6, 42, "selection = 23"),
                         c("seq_patterson_lucas", 7, 6, 42, "selection = 49"))
    }
    if (all(3 %in% J, 42 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 3, 42, "selection = 43"))
    }
    if (all(4 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 4, 21, "selection = 44"))
    }
    if (all(5 %in% J, 14 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 5, 14, "selection = 46"))
    }
    if (all(6 %in% J, 21 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 6, 21, "selection = 48"))
    }
    if (all(5 %in% J, 28 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 7, 5, 28, "selection = 86"))
    }
  } else if (D == 8) {
    if (all(3 %in% J, 48 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 3, 48, "selection = 102"),
                         c("seq_patterson_lucas", 8, 3, 48, "selection = 103"),
                         c("seq_patterson_lucas", 8, 3, 48, "selection = 136"))
    }
    if (all(4 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 4, 24, "selection = 104"))
    }
    if (all(6 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 6, 24, "selection = 105"))
    }
    if (all(4 %in% J, 48 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 4, 48, "selection = 137"))
    }
    if (all(5 %in% J, 24 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 5, 24, "selection = 138"))
    }
    if (all(3 %in% J, 32 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 8, 3, 32, "selection = 156"))
    }
  } else if (D == 9) {
    if (all(4 %in% J, 36 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 4, 36, "selection = 106"))
    }
    if (all(6 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 6, 18, "selection = 107"))
    }
    if (all(2 %in% J, 36 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 2, 36, "selection = 127"))
    }
    if (all(3 %in% J, 36 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 3, 36, "selection = 128"),
                         c("seq_patterson_lucas", 9, 3, 36, "selection = 139"))
    }
    if (all(4 %in% J, 36 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 4, 36, "selection = 140"))
    }
    if (all(5 %in% J, 36 %in% K)) {
      supported <- rbind(supported,
                         c("seq_patterson_lucas", 9, 5, 36, "selection = 141"))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_pbib_clatworthy_williams()...")
  }
  pbib_clatworthy_williams <-
    summary_seq_pbib_clatworthy_williams(D = D, J = J, K = K)
  if (nrow(pbib_clatworthy_williams) > 0) {
    for (i in 1:nrow(pbib_clatworthy_williams)) {
      supported            <-
        rbind(supported, c("seq_pbib_claworthy_williams",
                           pbib_clatworthy_williams[i, 4:6],
                           paste("type = ", pbib_clatworthy_williams[i, 1],
                                 ", williams_D = ",
                                 pbib_clatworthy_williams[i, 2],
                                 ", selection = ",
                                 pbib_clatworthy_williams[i, 3], sep = "")))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_pigeon()...")
  }
  if (all(D == 3, 3 %in% J, 9 %in% K)) {
    supported   <- rbind(supported, c("seq_pigeon", 3, 3, 9, "selection = 1"))
  } else if (D == 4) {
    if (all(3 %in% J, 18 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 4, 3, 18, "selection = 1"))
    }
    if (all(3 %in% J, 21 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 4, 3, 21, "selection = 2"))
    }
    if (all(3 %in% J, 27 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 4, 3, 27, "selection = 3"))
    }
    if (all(3 %in% J, 30 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 4, 3, 30, "selection = 4"))
    }
  } else if (D == 5) {
    if (all(4 %in% J, 16 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 4, 16, "selection = 1"))
    }
    if (all(4 %in% J, 32 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 4, 32, "selection = 2"))
    }
    if (all(4 %in% J, 36 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 4, 36, "selection = 3"))
    }
    if (all(3 %in% J, 28 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 3, 28, "selection = 4"))
    }
    if (all(3 %in% J, 38 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 3, 38, "selection = 5"))
    }
    if (all(3 %in% J, 36 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 5, 3, 36, "selection = 6"))
    }
  } else if (D == 6) {
    if (all(5 %in% J, 25 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 5, 25, "selection = 1"))
    } else if (all(5 %in% J, 50 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 5, 50, "selection = 2"))
    } else if (all(4 %in% J, 40 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 4, 40, "selection = 3"))
    } else if (all(3 %in% J, 20 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 3, 20, "selection = 4"))
    } else if (all(3 %in% J, 30 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 3, 30, "selection = 5"))
    } else if (all(3 %in% J, 40 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 3, 40, "selection = 6"))
    } else if (all(3 %in% J, 50 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 6, 3, 50, "selection = 7"))
    }
  } else if (D == 7) {
    if (all(6 %in% J, 36 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 7, 6, 36, "selection = 1"))
    }
    if (all(4 %in% J, 54 %in% K)) {
      supported <- rbind(supported, c("seq_pigeon", 7, 4, 54, "selection = 2"))
    }
  }
  if (summary) {
    message("...considering possibilities from seq_quenouille()...")
  }
  if (all(D == 3, 6 %in% J, 18 %in% K)) {
      supported <- rbind(supported,
                         c("seq_quenouille", 3, 6, 18, "selection = 1"),
                         c("seq_quenouille", 3, 6, 18, "selection = 2"))
  } else if (all(D == 4, 8 %in% J, 16 %in% K)) {
      supported <- rbind(supported,
                         c("seq_quenouille", 3, 8, 16, "selection = 1"),
                         c("seq_quenouille", 3, 8, 16, "selection = 2"),
                         c("seq_quenouille", 3, 8, 16, "selection = 3"))
  }
  if (summary) {
    message("...considering possibilities from seq_russell()...")
  }
  if (all(D == 5, 5 %in% J, 5 %in% K)) {
    supported <- rbind(supported, c("seq_russell", 5, 5, 5, ""))
  } else if (all(D == 7, 7 %in% J, 7 %in% K)) {
    supported <- rbind(supported, c("seq_russell", 7, 7, 7, ""))
  }
  if (summary) {
    message("...considering possibilities from seq_williams_bib()...")
  }
  indices         <- which(supported[, 1] == "seq_bib")
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      J_i         <- as.numeric(supported[indices[i], 3])
      K_i         <- as.numeric(supported[indices[i], 4])*J_i*(1 + J_i%%2)
      if (K_i %in% K) {
        supported <- rbind(supported,
                           c("seq_williams_bib", D, J_i, K_i,
                             paste("sequences = seq_bib(D = ", D, ", J = ", J_i,
                                   ", K = ",
                                   as.numeric(supported[indices[i], 4]), ")",
                                   sep = "")))
      }
    }
  }
  if (summary) {
    message("...considering possibilities from seq_williams()...")
  }
  if (D%%2 == 0) {
    if (all(D %in% J, D %in% K)) {
      supported <- rbind(supported, c("seq_williams", D, D, D, ""))
    }
  } else {
    if (all(D %in% J, (2*D) %in% K)) {
      supported <- rbind(supported, c("seq_williams", D, D, 2*D, ""))
    }
  }
  if (summary) {
    message("...completed the required search. Preparing outputs...")
  }
  if (!is.null(supported)) {
    colnames(supported)  <- c("function", "D", "J", "K", "required arguments")
    supported            <- tibble::as_tibble(supported)
    supported$`function` <- as.factor(supported$`function`)
    supported$D          <- as.numeric(supported$D)
    supported$J          <- as.numeric(supported$J)
    supported$K          <- as.numeric(supported$K)
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(supported)

}
