#' @export
plot_histograms_by_sequence <- function(data, outcome, bins, output = F,
                                        summary = T) {

  ##### Input checking #########################################################

  internal_data <- data
  if (missing(outcome)) {
    stop("outcome must be supplied.")
  }
  outcome       <- substitute(outcome)
  check_data(internal_data, outcome, "continuous", 2, 2, 2)
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
  sequences     <- levels(internal_data$sequence)
  if (!all(strsplit(sequences[1], split = "")[[1]] ==
           rev(strsplit(sequences[2], split = "")[[1]]))) {
    stop("data must relate to a trial with an AB/BA type design.")
  }
  if (!missing(bins)) {
    check_integer_range(bins, "bins", c(0, Inf), 1)
  }
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  summaries <- dplyr::summarise(dplyr::group_by(internal_data, subject),
                                `Subject totals`      = sum(outcome),
                                `Subject differences` = diff(outcome),
                                sequence = sequence[1])
  summaries <- tidyr::gather(summaries, key = "key", value = "value",
                             `Subject totals`:`Subject differences`)
  plot      <-
    ggplot2::ggplot(data = summaries, ggplot2::aes(value)) +
    ggplot2::facet_grid(sequence~key, scales = "free_x") +
    theme_xover()
  if (!missing(bins)) {
    plot    <- plot + ggplot2::geom_histogram(bins = 10)
  } else {
    plot    <- plot + ggplot2::geom_histogram()
  }
  print(plot)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  if (output) {
    return(plot)
  }

}
