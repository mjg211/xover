#' Link matrix specification
#'
#' Specifies the link matrix corresponding to a particular analysis model.
#'
#' \code{mat_link()} supports the specification of link matrices. Precisely,
#' a number of treatments (see \code{D}) and a chosen model (see \code{model})
#' are specified. The link matrix for use in computing design matrices is then
#' returned. The value of \code{model} must be an integer between one and nine
#' inclusive, corresponding to the following models:
#'
#' \itemize{
#' \item 1. Standard additive model;
#' \item 2. Second-order carry-over effects;
#' \item 3. Full-set of interactions;
#' \item 4. Self-adjacency model;
#' \item 5. Placebo model;
#' \item 6. No carry-over into self model;
#' \item 7. Treatment decay model;
#' \item 8. Proportionality model;
#' \item 9. No carry-over effects.
#' }
#'
#' See the package vignette for further details on these models.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{2}.
#' @param model A single \code{\link[base]{numeric}} integer between one and
#' nine inclusive, specifying which model to compute the row-column design for.
#' See \strong{Details} for further information. Defaults to \code{1}.
#' @param prop Only used when \code{model = 3}, when it should be a single
#' \code{\link[base]{numeric}} describing the proportionality parameter.
#' @param placebos Only used when \code{model = 4}, when it should be a single
#' \code{\link[base]{numeric}} integer between zero and \code{D - 1} inclusive,
#' describing the number of placebos in the trial.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[base]{matrix}} detailing the determined link matrix.
#' @author Based on code from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
mat_link <- function(D = 4, model = 1, prop = 0.5, placebos = 1, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_integer_range(model, "model", c(0, 10), 1)
  check_real_range(prop, "prop", c(0, 1), 1)
  check_integer_range(placebos, c(0, D), 1)
  if (all(model != 3, prop != 1)) {
    warning("prop has been changed from default but will not be used based on ",
            "the choice of model.")
  }
  if (all(model != 4, placebos != 1)) {
    warning("placebos has been changed from default but will not be used based",
            " on the choice of model.")
  }
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  if (model %in% c(1, 2, 5, 7, 8)) {
    I_D  <- diag(D)
    J_D1 <- matrix(1, D, 1)
    Z_D  <- matrix(0, D, D)
    H    <- rbind(cbind(I_D, Z_D), cbind(kronecker(J_D1, I_D),
                                         kronecker(I_D, J_D1)))
  }
  if (model == 2) {
    H                  <- cbind(H, matrix(0, D*(D + 1), D))
    for (j in (D + 1):(D*(D + 1))) {
      jD               <- (j - 1)%/%D
      if (jD == j - D*jD) {
        H[j, D + jD]   <- 0
        H[j, 2*D + jD] <- 1
      }
    }
  } else if (model == 3) {
    H                  <- matrix(0, D*(D + 1), D)
    H[1:D, 1:D]        <- diag(D)
    for (j in (D + 1):(D*(D + 1))) {
      jD               <- (j - 1)%/%D
      if (jD == j - D*jD) {
        H[j, jD]       <- 1 + prop
      } else {
        H[j, j - D*jD] <- 1
        H[j, jD]       <- prop
      }
    }
  } else if (model == 4) {
    H                <- matrix(0, D*(D + 1), 2*D)
    for (j in 1:(D*(D + 1))) {
      jD             <- (j - 1)%/%D
      H[j, j - D*jD] <- 1
      if (j > D*(placebos + 1)) {
        H[j, D + jD] <- 1
      }
    }
  } else if (model == 5) {
    for (j in (D + 1):(D*(D + 1))) {
      jD             <- (j - 1)%/%D
      if (jD == j - D*jD) {
        H[j, D + jD] <- 0
      }
    }
  } else if (model == 6) {
    H                <- matrix(0, D*(D + 1), 2*D)
    for (j in 1:(D*(D + 1))) {
      jD             <- (j - 1)%/%D
      H[j, j - D*jD] <- 1
      if (all(j > D, jD == j - D*jD)) {
        H[j, D + jD] <- -1
      }
    }
  } else if (model == 7) {
    H2                             <- matrix(0, D*(D + 1), D*D)
    for (j in (D + 1):(D*(D + 1))) {
      jD                           <- (j - 1)%/%D
      H2[j, D*(j - 1 - D*jD) + jD] <- 1
    }
    H                              <- cbind(H, H2)
  } else if (model == 8) {
    H <- rbind(cbind(I_D, Z_D, Z_D), cbind(kronecker(J_D1, I_D),
                                           kronecker(I_D, J_D1),
                                           matrix(0, D*D, D)),
               cbind(kronecker(kronecker(J_D1, J_D1), I_D),
                     kronecker(kronecker(J_D1, I_D), J_D1),
                     kronecker(kronecker(I_D, J_D1), J_D1)))
  } else if (model == 9) {
    H <- diag(D)
  }
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(H)

}
