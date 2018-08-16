mat_xover <- function(sequences, rcd, model = 1, random = T, prop = 0.5,
                      placebos = 1, Xr, H) {


  if ("co_seq_tibble" %in% class(sequences)) {
    act_sequences    <- levels(sequences$Sequence)
    J                <- length(strsplit(act_sequences[1], split = "")[[1]])
    K                <- length(act_sequences)
    sequences        <- matrix(0, K, J)
    for (k in 1:K) {
      sequences[k, ] <- strsplit(act_sequences[k], split = "")[[1]]
    }
  } else {
    J                <- ncol(sequences)
    K                <- nrow(sequences)
  }
  D                  <- length(unique(as.vector(sequences)))

  H   <- mat_link(D, model, prop, placebos)
  Xr  <- mat_rcd(sequences, rcd, D, model)
  X_T <- Xr%*%H
  X_B <-
  Z <- matrix(0, nrow = J*K, ncol = K)
  for (k in 1:K) {
    Z[(1 + J*(k - 1)):(J*k), k] <- 1
  }
  if (!random) {
    X_B <- cbind(X_B, Z)
  }

  # Set all the column names
  #Tr <-
  #B  <-

}
