#' @export
plot_sequence_by_periods <- function(data, outcome, complete_case = T,
                                     output = F, summary = T) {

  ##### Input checking #########################################################

  internal_data      <- data
  if (missing(outcome)) {
    stop("outcome must be supplied.")
  }
  outcome            <- substitute(outcome)
  check_data(internal_data, outcome, "continuous")
  check_logical(complete_case, "complete_case")
  if (complete_case) {
    internal_data_cc <- internal_complete_case_data(internal_data, outcome)
    if (summary) {
      message("   Converted the data to complete-case form. This removed ",
              nrow(internal_data) - nrow(internal_data),
              " rows from the dataset.")
    }
    if (nrow(internal_data_cc) == 0) {
      stop("The complete-case dataset consists of zero outcomes.")
    }
    internal_data <- internal_data_cc
  }
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  sequences        <- levels(data$sequence)
  K                <- length(sequences)
  seq_lengths      <- numeric(K)
  for (k in 1:K) {
    seq_lengths[k] <- length(strsplit(sequences[k], split = "")[[1]])
  }
  labels           <- levels(data$treatment)
  J                <- max(seq_lengths)
  jk_combs         <- NULL
  for (k in 1:K) {
    jk_combs       <- rbind(jk_combs, cbind(1:seq_lengths[k],
                                            rep(sequences[k], seq_lengths[k]),
                                            strsplit(sequences[k],
                                                     split = "")[[1]]))
  }
  summary_means    <- tibble::tibble(period    = factor(jk_combs[, 1],
                                                        levels = 1:J),
                                     sequence  = factor(jk_combs[, 2],
                                                        levels = sequences),
                                     treatment = factor(jk_combs[, 3], labels),
                                     means = NA)
  for (jk in 1:nrow(summary_means)) {
    summary_means$means[jk] <-
      as.numeric(dplyr::summarise(
        dplyr::filter(data, period == summary_means$period[jk] &
                            sequence == summary_means$sequence[jk]),
                      mean = mean(outcome)))
  }
  summary_means$label <- paste(summary_means$sequence,
                               summary_means$treatment, sep = "")
  plot                <-
    ggplot2::ggplot(data = summary_means,
                    ggplot2::aes(x = period, y = means)) +
    ggplot2::geom_line(ggplot2::aes(group = treatment)) +
    ggplot2::geom_point(ggplot2::aes(colour = treatment, shape = sequence),
                        size = 3) +
    ggthemes::scale_color_ptol() + ggplot2::xlab("Period") +
    ggplot2::ylab(paste("Mean", deparse(outcome))) +
    theme_xover()
  print(plot)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  if (output) {
    return(plot)
  }

}
