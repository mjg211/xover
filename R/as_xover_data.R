#' Convert dataset to class \code{xover_data}
#'
#' Converts an input dataset to be of class \code{xover_data}.
#'
#' \code{as_xover_data()} supports the conversion of an input dataset to be of
#' class \code{xover_data}. Precisely, a \code{\link[base]{matrix}} or
#' \code{\link[base]{data.frame}} is provided (see \code{data}) where each row
#' corresponds to a particular outcome variable, and the function
#' converts it to a \code{\link[tibble]{tibble}} with class \code{xover_data},
#' augmenting the column names as required. In adddition, checks are performed
#' that the dataset corresponds to a cross-over trial.
#'
#' The input dataset must contain information on the period, subject, and
#' treatment/sequence for each outcome variable.
#'
#' @param data A \code{\link[base]{matrix}} or \code{\link[base]{data.frame}}
#' containing data corresponding to a cross-over trial.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @export
as_xover_data <- function(data, summary = T) {

  ##### Input checking #########################################################

  if (all(!is.data.frame(data), !is.matrix(data))) {
    stop("data must be either a data.frame or a matrix.")
  }
  if (all(is.matrix(data), is.null(colnames(data)))) {
    stop("If data is a matrix, its column names must be set.")
  }
  colnames_data <- colnames(data)
  if (!all(any(c("Period", "period") %in% colnames_data),
           any(c("Subject", "subject") %in% colnames_data),
           any(c("Treatment", "treatment",
                 "Sequence", "sequence") %in% colnames_data))) {
    stop("data must contain columns with the following names: \n   1. ",
         "Period/period\n   2. Subject/subject\n   3. Treatment/treatment/",
         "Sequence/sequence")
  }
  if (ncol(data) < 4) {
    stop("data must have at least four columns, corresponding to the period,",
         " subject, treatment/sequence information, and at least one outcome",
         " variable")
  }
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  data          <- tibble::as_tibble(data)
  colnames_data <- colnames(data)
  if ("Subject" %in% colnames_data) {
    colnames(data)[which(colnames_data == "Subject")]   <- "subject"
  }
  if (is.numeric(data$subject)) {
    subjects    <- unique(data$subject)
  } else if (any(is.character(data$subject), is.factor(data$subject))) {
    subjects    <- suppressWarnings(as.numeric(as.character(data$subject)))
    if (any(is.na(subjects))) {
      stop("The subject data column must be convertible to numeric form.")
    }
    subjects    <- unique(subjects)
  } else {
    stop("The subject data column must be stored in either numeric, character,",
         " or factor form.")
  }
  if (any(subjects <= 0, subjects%%1 != 0, !is.finite(subjects))) {
    stop("The subject identifiers must be convertible to strictly positive",
         " integers.")
  }
  data$subject  <- factor(data$subject, subjects)
  if ("Period" %in% colnames_data) {
    colnames(data)[which(colnames_data == "Period")]    <- "period"
  }
  if (is.numeric(data$period)) {
    periods     <- unique(data$period)
  } else if (any(is.character(data$period), is.factor(data$period))) {
    periods     <- suppressWarnings(as.numeric(as.character(data$period)))
    if (any(is.na(periods))) {
      stop("The period data column must be convertible to numeric form.")
    }
    periods     <- unique(periods)
  } else {
    stop("The period data column must be stored in either numeric, character,",
         " or factor form.")
  }
  if (any(periods <= 0, periods%%1 != 0, !is.finite(periods))) {
    stop("The period data column must be convertible to strictly positive ",
         "integers.")
  }
  J             <- max(periods)
  data$period   <- factor(data$period, 1:J)
  if (!all(1:J %in% as.numeric(data$period))) {
    warning("The period data column contains values up to ", J,
            ", but has no data from at least one period 1,...,", J - 1, ".")
  }
  if ("Treatment" %in% colnames_data) {
    colnames(data)[which(colnames(data) == "Treatment")] <- "treatment"
  }
  if ("Sequence" %in% colnames_data) {
    colnames(data)[which(colnames(data) == "Sequence")]  <- "sequence"
  }
  if ("sequence" %in% colnames(data)) {
    if (any(is.numeric(data$sequence), is.character(data$sequence),
            is.factor(data$sequence))) {
      sequences             <- unique(as.character(data$sequence))
      K                     <- length(sequences)
      sequence_lengths      <- numeric(K)
      for (k in 1:K) {
        sequence_lengths[k] <- length(strsplit(sequences[k], split = "")[[1]])
      }
      if (length(unique(sequence_lengths)) > 1) {
        warning("Unique elements of sequence data column are not all of equal",
                " length.")
      }
      data$sequence         <- factor(data$sequence, sequences)
    } else {
      stop("The sequence data column must be stored in either numeric, ",
           "character, or factor form.")
    }
  }
  if ("treatment" %in% colnames(data)) {
    if (any(is.numeric(data$treatment), is.character(data$treatment),
            is.factor(data$treatment))) {
      treatments     <- unique(data$treatment)
      D              <- length(treatments)
      data$treatment <- factor(data$treatment, treatments)
    } else {
      stop("The treatment data column must be stored in either numeric, ",
           "character, or factor form.")
    }
  }
  if (!("sequence" %in% colnames(data))) {
    sequence_data         <- numeric(nrow(data))
    for (s in subjects) {
      data_s              <- dplyr::filter(data, subject == s)
      sequence_data[which(data$subject == s)] <-
        paste(data_s$treatment[order(data_s$period)], collapse = "")
    }
    sequences             <- unique(sequence_data)
    K                     <- length(sequences)
    sequence_lengths      <- numeric(K)
    for (k in 1:K) {
      sequence_lengths[k] <- length(strsplit(sequences[k], split = "")[[1]])
    }
    if (length(unique(sequence_lengths)) > 1) {
      warning("Inferred sequences are not all of equal length.")
    }
    data$sequence         <- factor(sequence_data, sequences)
  }
  if (any(as.numeric(data$period) > max(sequence_lengths))) {
    stop("Values in the period data column cannot be larger than the maximal",
         " sequence length.")
  }
  if (!("treatment" %in% colnames(data))) {
    treatment_data <- numeric(nrow(data))
    for (s in subjects) {
      data_s       <- dplyr::filter(data, subject == s)
      period_s     <- as.numeric(data_s$period)
      treatment_data[which(data$subject == s)[period_s]] <-
        strsplit(as.character(data_s$sequence[1]), split = "")[[1]][period_s]
    }
    treatments     <- unique(treatment_data)
    D              <- length(treatments)
    data$treatment <- factor(treatment_data, treatments)
  }
  index_options    <- c("Sequence Index", "Sequence.Index", "Sequence index",
                        "Sequence.index", "sequence index", "sequence.index")
  if (any(index_options %in% colnames(data))) {
    colnames(data)[which(colnames(data) %in% index_options)] <- "sequence index"
    if (is.numeric(data$`sequence index`)) {
      indices <- unique(data$`sequence index`)
    } else if (any(is.character(data$`sequence index`),
                   is.factor(data$`sequence index`))) {
      indices <-
        suppressWarnings(as.numeric(as.character(data$`sequence index`)))
      if (any(is.na(indices))) {
        stop("The sequence index data column must be convertible to numeric ",
             "form.")
      }
      indices <- unique(indices)
    } else {
      stop("The sequence index data column must be stored in either numeric,",
           " character, or factor form.")
    }
    data$`sequence index` <- factor(data$`sequence index`, indices)
    if (!all(1:K %in% indices)) {
      warning("The sequence index data column is not stored as integers ",
              "between 1 and ", K, ".")
    }
    linked_index                       <- list()
    for (k in 1:K) {
      linked_index[[sequences[k]]] <- numeric(0)
    }
    for (i in 1:nrow(data)) {
      linked_index[[data$sequence[i]]] <- c(linked_index[[data$sequence[i]]],
                                            data$`sequence index`[i])
    }
    for (k in sequences) {
      if (length(unique(linked_index[[k]])) > 1) {
        stop("Input sequence indices are not linked uniquely to the sequences.")
      }
    }
  } else {
    index_data            <- numeric(nrow(data))
    indices               <- 1:K
    for (k in 1:K) {
      index_data[which(data$sequence == sequences[k])] <- k
    }
    data$`sequence index` <- factor(index_data, 1:K)
  }
  for (s in subjects) {
    data_s      <- dplyr::filter(data, subject == s)
    period_s    <- as.numeric(as.character(data_s$period))
    treatment_s <- as.character(data_s$treatment)
    index_s     <- as.numeric(as.character(data_s$`sequence index`))
    sequence_s  <- as.character(data_s$sequence)
    if (length(unique(sequence_s)) > 1) {
      stop("The sequence information for each subject must be constant across ",
           "periods.")
    }
    if (length(unique(index_s)) > 1) {
      stop("The sequence index information for each subject must be constant ",
           "across periods.")
    }
    seq_s_split <- strsplit(sequence_s, split = "")[[1]]
    if (any(period_s > length(seq_s_split))) {
      stop("Values in the period data column cannot be larger than an ",
           "individuals sequence length.")
    }
    if (length(unique(period_s)) < length(period_s)) {
      stop("At most one row of the input data can correspond to a particular ",
           "subject in a particular period.")
    }
    if (!(all(treatment_s[order(period_s)] == seq_s_split))) {
      stop("The treatment information column must match up with the ",
           "period/sequence information columns.")
    }
  }
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }
  class(data) <- c(class(data), "xover_data")

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(data)

}
