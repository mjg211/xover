#' @export
plot_subject_profiles <- function(data, outcome, complete_case = T,
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

  plot                <-
    ggplot2::ggplot(data = data,
                    ggplot2::aes_string(x = "period", y = deparse(outcome))) +
    ggplot2::geom_line(ggplot2::aes(group = subject)) +
    ggplot2::geom_point(ggplot2::aes(colour = treatment)) +
    ggthemes::scale_color_ptol() +
    ggplot2::xlab("Period") + ggplot2::ylab(deparse(outcome)) +
    ggplot2::facet_wrap(~ sequence, ncol = 2) + theme_xover()
  print(plot)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  if (output) {
    return(plot)
  }

}
