#' Create the design matrix for a given row column design
#'
#' @param X row-column design
#' @param v number of treatments
#' @param model String or number describing the model. See \code{\link{getModelNr}}.
#' @return The design matrix for a row-column design.
#' @seealso \code{\link{rcd}} gives the row-column design to a given crossover design.
#' @export
mat_rcd <- function(rcd, D = 2, model = 1) {

  ##### Input checking #########################################################

  #check_rcd_sequences(sequences)
  check_integer_range(model, "model", c(0, 10), 1)

  ##### Main computations ######################################################

  sequences <- rcd
  if (model == 8) {
    DD <- D + D*D + D*D*D
  } else if (model == 9) {
    DD <- D
  } else {
    DD <- D + D^2
  }
  K    <- nrow(sequences)
  J    <- ncol(sequences)
  Xr   <- matrix(0, nrow(sequences)*ncol(sequences), DD)
  for (j in 1:J) {
    for (k in 1:K) {
      Xr[(k - 1)*J + (j - 1) + 1, sequences[k, j]] <- 1
    }
  }

  ##### Outputting #############################################################

  return(Xr)

}
