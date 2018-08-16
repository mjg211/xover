convert_labels <- function(sequences, D, new_labels, old_labels) {
  new_sequences                                      <- sequences
  for (d in 1:D) {
    new_sequences[which(sequences == old_labels[d])] <- new_labels[d]
  }
  return(new_sequences)
}

factor_comb <- function(p, n) {
  f        <- matrix(0, p^n, n)
  for (i in 1:n) {
    f[, i] <- rep(0:(p - 1), p^(i - 1), each = p^(n - i))
  }
  return(f)
}

gf <- function(p, n) {
  ord                  <- p^n
  if (n == 1) {
    elemente           <- 0:(p - 1)
    primpol            <- NULL
  } else {
    a                  <- factor_comb(p, n)
    g                  <- cbind(rep(1, ord), a)
    h                  <- matrix(0, ord, p)
    for (i in 0:(p - 1)) {
      a                <- numeric(ord)
      for (j in 1:n) {
        a              <- a + g[, j]*(i^(n + 1 - j))
      }
      h[, i + 1]       <- (a + g[, n + 1])%%p
    }
    irr                <- g[!apply(h == 0, 1, any), , drop = F]
    nirr               <- nrow(irr)
    z1                 <- numeric(nirr)
    for (i in 1:nirr) {
      inter            <- c(numeric(ord - 3), 1, 0)
      j                <- 0
      while (z1[i] == 0) {
        j              <- j + 1
        inter          <- redu_modp(c(inter, 0), irr[i, ], p)
        if (all(inter == c(numeric(n - 1), 1))) {
          z1[i]        <- j + 1
        }
      }
    }
    primpol            <- irr[which(z1 == (ord - 1)), , drop = F]
    elemente           <- matrix(0, ord, n)
    elemente[2, n]     <- 1
    elemente[3, n - 1] <- 1
    inter              <- c(numeric(ord - 3), 1, 0)
    for (i in 4:ord) {
      inter            <- redu_modp(c(inter, 0), primpol[1, ], p)
      elemente[i, ]    <- inter
    }
  }
  return(list(elemente, primpol))
}

internal_classify_seq <- function(sequences) {
  labels       <- unique(as.vector(sequences))
  D            <- length(labels)
  J            <- ncol(sequences)
  K            <- nrow(sequences)
  X            <- matrix(0, J*K, D)
  for (k in 1:K) {
    for (j in 1:J) {
      X[(k - 1)*J + j, which(labels == sequences[k, j])] <- 1
    }
  }
  Z            <- diag(K)%x%rep(1, J)
  occurrences  <- diag(t(X)%*%X)
  incidences   <- t(X)%*%Z
  concurrences <- incidences%*%t(incidences)
  type         <- rep(F, 6)
  type[1]      <- all(occurrences == (K*J/D))
  type[2]      <- all(min(incidences) > floor(J/D) - 0.1,
                      max(incidences) < floor(J/D) + 1.1)
  type[3]      <- (length(unique(concurrences[upper.tri(concurrences)])) == 1)
  type[4]      <- (J < D)
  type[5]      <- (J == D)
  type[6]      <- !any(as.logical(as.vector(incidences) - incidences[1, 1]))
  return(list(concurrences = concurrences, incidences = incidences,
              occurrences  = occurrences,  type       = type))
}

mult <- function(a, b) {
  len_a                          <- length(a)
  len_b                          <- length(b)
  output                         <- matrix(0, len_a + len_b - 1, len_b)
  for (i in 1:len_b) {
    output[i:(i + len_a - 1), i] <- a*b[i]
  }
  return(rowSums(output))
}

redu <- function(a, b) {
  len_a                      <- length(a)
  len_b                      <- length(b)
  b1                         <- b/b[1]
  for (i in 1:(len_a - len_b + 1)) {
    inter                    <- numeric(len_a)
    inter[i:(i + len_b - 1)] <- a[i]*b1
    a                        <- a - inter
  }
  return(a[(len_a - len_b + 2):len_a])
}

redu_modp <- function(a, b, p) {
  len_a                      <- length(a)
  len_b                      <- length(b)
  b1                         <- b/b[1]
  for (i in 1:(len_a - len_b + 1)) {
    inter                    <- numeric(len_a)
    inter[i:(i + len_b - 1)] <- a[i]*b1
    a                        <- (a - inter)%%p
  }
  return(a[(len_a - len_b + 2):len_a])
}

tibble_to_matrix <- function(sequences) {
  act_sequences    <- levels(sequences$sequence)
  J                <- length(strsplit(act_sequences[1, ])[[1]])
  K                <- length(act_sequences)
  sequences        <- matrix(0, K, J)
  for (k in 1:K) {
    sequences[k, ] <- strsplit(act_sequences[1, ])[[1]]
  }
  return(sequences)
}

transform_to_xover <- function(sequences, labels, as_matrix) {
  J                     <- ncol(sequences)
  K                     <- nrow(sequences)
  if (as_matrix) {
    rownames(sequences) <- paste("k =", seq_len(K))
    colnames(sequences) <- paste("j =", seq_len(J))
  } else {
    act_sequences       <- numeric(K)
    for (k in 1:K) {
      act_sequences[k]  <- paste(sequences[k, ], collapse = "")
    }
    sequences           <-
      tibble::tibble(period           = factor(rep(1:J, K), 1:J),
                     treatment        = factor(as.vector(t(sequences)), labels),
                     sequence         = factor(rep(act_sequences, each = J),
                                               unique(act_sequences)),
                     `sequence index` = factor(rep(1:K, each = J), 1:K))
  }
  class(sequences)    <- c(class(sequences), "xover_seq")
  return(sequences)
}
