check_belong <- function(value, name, allowed, len) {
  if (!is.numeric(len)) {
    for (i in 1:length(value)) {
      if (!(value[i] %in% allowed)) {
        stop(name, " must be a vector, and only contain values in ",
             paste(allowed, collapse = ", "))
      }
    }
  } else if (len > 1) {
    if (any(length(value) != len, !(value %in% allowed))) {
      stop(name, " must be a vector of length ", len, ", and only contain ",
           "values in ", paste(allowed, collapse = ", "))
    }
  } else {
    if (any(length(value) != 1, !(value %in% allowed))) {
      stop(name, " must be set to one of ", paste(allowed, collapse = ", "))
    }
  }
}

check_data <- function(data, outcome, type, D, J, K, output = F) {
  if (!("xover_data" %in% class(data))) {
    stop("data must be of class xover_data")
  }
  data     <- as_xover_data(data, F)
  if (!missing(outcome)) {
    if (!(deparse(outcome) %in% colnames(data))) {
      stop("Chosen outcome variable must be present in data.")
    }
    if (!missing(type)) {
      if (type == "binary") {
        if (any(!is.factor(eval(outcome, data)),
                length(levels(eval(outcome, data))) > 2)) {
          stop("Chosen outcome variable must be a factor with two levels")
        }
      } else if (type == "categorical") {
        if (!is.factor(eval(outcome, data))) {
          stop("Chosen outcome variable must be a factor")
        }
      } else if (type == "continuous") {
        if (!is.numeric(eval(outcome, data))) {
          stop("Chosen outcome variable must be numeric")
        }
      } else if (type == "count") {
        if (any(!is.numeric(eval(outcome, data)), eval(outcome, data)%%1 != 0)) {
          stop("Chosen outcome variable must be numeric, containing only ",
               "integers")
        }
      }
    }
  }
  if (!missing(D)) {
    D_data <- length(unique(data$treatment))
    if (D != D_data) {
      stop("data must relate to a trial with ", D, " treatments")
    }
  }
  if (!missing(J)) {
    J_data <- length(unique(data$period))
    if (J != J_data) {
      stop("data must relate to a trial with ", J, " periods")
    }
  }
  if (!missing(K)) {
    K_data <- length(unique(data$sequence))
    if (K != K_data) {
      stop("data must relate to a trial with ", K, " sequences")
    }
  }
  if (output) {
    return(data)
  }
}

check_integer_range <- function(value, name, range, len) {
  if (!is.numeric(len)) {
    if (any(value%%1 != 0, !is.finite(value), value <= range[1],
            value >= range[2])) {
      stop(name, " must be a vector of length ", len, ", and only contain ",
           "integers in (", range[1], ",", range[2], ")")
    }
  } else if (len > 1) {
    if (any(length(value) != len, value%%1 != 0, !is.finite(value),
            value <= range[1], value >= range[2])) {
      stop(name, " must be a vector of length ", len, ", and only contain ",
           "integers in (", range[1], ",", range[2], ")")
    }
  } else {
    if (any(length(value) != 1, value%%1 != 0, !is.finite(value),
            value <= range[1], value >= range[2])) {
      stop(name, " must be a single integer in (", range[1], ",", range[2], ")")
    }
  }
}

check_labels <- function(labels, D) {
  if (any(!is.vector(labels), length(labels) != D,
          length(unique(labels)) != D)) {
    stop("labels must be a vector of length ", D, ", containing ", D,
         " unique elements")
  }
}

check_logical <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_real_range <- function(value, name, range, len) {
  if (!is.numeric(len)) {
    if (any(value <= range[1], value >= range[2])) {
      stop(name, " must be a vector, and only contain numbers in [", range[1],
           ",", range[2], "]")
    }
  } else {
    if (len > 1) {
      if (any(length(value) != len, value <= range[1], value >= range[2])) {
        stop(name, " must be a vector of length ", len, ", and only contain ",
             "numbers in [", range[1], ",", range[2], "]")
      }
    } else {
      if (any(length(value) != 1, value < range[1], value > range[2])) {
        stop(name, " must be a single number in [", range[1], ",", range[2],
             "]")
      }
    }
  }
}

check_real_range_strict <- function(value, name, range, len) {
  if (!is.numeric(len)) {
    if (any(value <= range[1], value >= range[2])) {
      stop(name, " must be a vector, and only contain numbers in (", range[1],
           ",", range[2], ")")
    }
  } else if (len > 1) {
    if (any(length(value) != len, value <= range[1], value >= range[2])) {
      stop(name, " must be a vector of length ", len, ", and only contain ",
           "numbers in (", range[1], ",", range[2], ")")
    }
  } else {
    if (any(length(value) != 1, value <= range[1], value >= range[2])) {
      stop(name, " must be a single number in (", range[1], ",", range[2], ")")
    }
  }
}

check_selection <- function(selection, supported, D) {
  if (!(selection %in% 1:supported[D])) {
    if (supported[D] == 1) {
      stop("For D = ", D, " selection must be equal to 1")

    } else if (supported[D] == 2) {
      stop("For D = ", D, " selection must be either 1 or 2")

    } else {
      stop("For D = ", D, " selection must be an integer in [1,", supported[D],
           "]")

    }
  }
}

check_sequences <- function(sequences) {
  if (!("xover_seq" %in% class(sequences))) {
    stop("sequences must be of class xover_seq")
  }
  if (tibble::is_tibble(sequences)) {
    if (any(ncol(sequences) != 4,
            !(c("period", "treatment", "sequence",
                "sequence index") %in% colnames(sequences)))) {
      stop("sequences must have four columns, with the following names:\n",
           "   1. period;\n   2. treatment;\n   3. sequence;\n   ",
           "4. sequence index.")
    }
    for (i in 1:4) {
      if (!is.factor(sequences[[i]])) {
        stop("The columns of sequences must be stored as a factor.")
      }
    }
    act_sequences      <- levels(sequences$sequence)
    K                  <- length(act_sequences)
    len_sequences      <- numeric(K)
    for (k in 1:K) {
      len_sequences[k] <- length(strsplit(act_sequences[k], split = "")[[1]])
    }
    if (length(unique(len_sequences)) > 1) {
      stop("Allocation sequences must be of the same length.")
    }
    J                  <- len_sequences[1]
    if (!all(levels(sequences$period) == 1:J)) {
      stop("The levels of sequences$period must be 1 through to the number of",
           " periods.")
    }
    for (k in 1:K) {
      sequences_k      <- dplyr::filter(sequences, sequence == act_sequences[k])
      if (nrow(sequences_k) != J) {
        stop("Rows corresponding to all periods of a given allocation sequence",
             " must be present in sequences.")
      }
      if (!all(sequences_k$period == 1:J)) {
        stop("The period column of sequences must be numbered one through to ",
             "the number of periods for each allocation sequence.")
      }
      if (!all(sequences_k$treatment == strsplit(act_sequences[k],
                                                 split = "")[[1]])) {
        stop("The treatment column of sequences does not correspond correctly",
             " to the sequence column.")
      }
      if (length(unique(sequences_k$`sequence index`)) > 1) {
        stop("The sequence index must be constant across rows corresponding to",
             " a particular sequence.")
      }
    }
    if (!all(levels(sequences$`sequence index`) != 1:K)) {
      warning("The sequence index column of sequences is not numbered ",
              "sequentially starting from one.")
    }
  } else if (is.matrix(sequences)) {
    K                  <- ncol(sequences)
    J                  <- nrow(sequences)
    if (any(J == 1, K == 1)) {
      stop("sequences must contain at least two allocation sequences of length",
           " two.")
    }
    act_sequences      <- numeric(K)
    for (k in 1:K) {
      act_sequences[k] <- paste(sequences[k, ], collapse = "")
    }
    if (length(unique(act_sequences)) < K) {
      warning("sequences contains non-unique allocations.")
    }
  } else {
    stop("sequences must be of class matrix or tibble.")
  }
}

check_type <- function(value) {
  if (!(value %in% c("R", "S", "SR"))) {
    stop("type must be one of \"R\", \"S\", or \"SR\"")
  }
}

theme_xover <- function(base_size = 11, base_family = "") {
  ggplot2::theme_grey(base_size = base_size,
                      base_family = base_family) +
    ggplot2::theme(axis.ticks       =
                     ggplot2::element_line(colour = "grey70", size = 0.25),
                   complete         = T,
                   legend.key       =
                     ggplot2::element_rect(fill = "white", colour = NA),
                   legend.position  = "bottom",
                   legend.title = ggplot2::element_blank(),
                   panel.background =
                     ggplot2::element_rect(fill = "white", colour = NA),
                   panel.border     =
                     ggplot2::element_rect(fill = NA, colour = "grey70",
                                           size = 0.5),
                   panel.grid.major =
                     ggplot2::element_line(colour = "grey87", size = 0.25),
                   panel.grid.minor =
                     ggplot2::element_line(colour = "grey87", size = 0.125),
                   plot.margin = ggplot2::unit(c(0.3, 0.5, 0.3, 0.3), "cm"),
                   strip.background =
                     ggplot2::element_rect(fill = "grey70", colour = NA),
                   strip.text       =
                     ggplot2::element_text(colour = "white",
                                           size   = ggplot2::rel(0.8)))
}
