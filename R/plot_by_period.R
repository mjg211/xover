#' @export
plot_by_period <- function(data, outcome, complete_case = T,
                           type = "boxplot", output = F, summary = T) {

  ##### Error checking #########################################################

  internal_data <- data
  if (missing(outcome)) {
    stop("outcome must be supplied.")
  }
  outcome       <- substitute(outcome)
  check_data(internal_data, outcome, "continuous")
  check_logical(complete_case, "complete_case")
  if (complete_case) {
    internal_data_cc <- internal_complete_case_data(internal_data, outcome)
    if (summary) {
      message(" Converted the data to complete-case form. This removed ",
              nrow(internal_data) - nrow(internal_data_cc),
              " rows from the dataset")
    }
    if (nrow(internal_data_cc) == 0) {
      stop("The complete-case dataset consists of zero outcomes")
    }
    internal_data <- internal_data_cc
  }
  check_belong(type, "type", c("boxplot", "violinplot", "ridgeplot"), 1)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning construction of plot...")
  }
  if (type != "ridgeplot") {
    plot    <- ggplot2::ggplot(data, ggplot2::aes_string(x = "period",
                                                         y = outcome))
    if (type == "boxplot") {
      plot <- plot + ggplot2::geom_boxplot()
    } else if (type == "violinplot") {
      plot <- plot + ggplot2::geom_violin(scale = "count")
    }
  } else {
    plot   <- ggplot2::ggplot(data, ggplot2::aes_string(x = outcome,
                                                         y = "period")) +
                ggridges::geom_density_ridges()
  }
  if (summary) {
    message("...completed construction of plot.")
  }
  print(plot)

  ##### Outputting #############################################################

  if (output) {
    if (summary) {
      message("   Outputting.")
    }
    return(plot)
  }
}
