#' @export
plot_hulls <- function(data, outcome, output = F, summary = T) {

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
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  summaries              <- dplyr::summarise(dplyr::group_by(internal_data,
                                                             subject),
                                             total    = sum(outcome),
                                             diff     = diff(outcome),
                                             sequence = sequence[1])
  sequences              <- levels(summaries$sequence)
  outer_hulls            <- inner_hulls <- list()
  for (k in 1:2) {
    summaries_k          <- dplyr::filter(summaries, sequence == sequences[k])
    outer_hulls[[k]]     <- find_hull_data(summaries_k)
    remaining            <- dplyr::filter(summaries_k,
                                          !(subject %in%
                                              outer_hulls[[k]]$subject))
    if (nrow(remaining) > 3) {
      while (nrow(remaining) > 3) {
        inner_hulls[[k]] <- find_hull_data(remaining)
        remaining        <- dplyr::filter(remaining,
                                          !(subject %in%
                                              inner_hulls[[k]]$subject))
      }
    } else if (nrow(remaining) == 3) {
      inner_hulls[[k]]   <- remaining
    } else {
      inner_hulls[[k]]   <- outer_hulls[[k]]
    }
  }
  outer_hulls            <- rbind(outer_hulls[[1]], outer_hulls[[2]])
  inner_hulls            <- rbind(inner_hulls[[1]], inner_hulls[[2]])
  plot                   <-
    ggplot2::ggplot() +
    ggplot2::geom_point(data = summaries,
                        ggplot2::aes(x = total, y = diff, colour = sequence)) +
    ggplot2::geom_polygon(data = outer_hulls,
                          ggplot2::aes(x = total, y = diff, group = sequence,
                                       fill = sequence), alpha = 0.2) +
    ggplot2::geom_polygon(data = inner_hulls,
                          ggplot2::aes(x = total, y = diff, group = sequence,
                                       fill = sequence), alpha = 0.6) +
    ggplot2::xlab(expression(paste("Totals (",
                                   italic(y)[italic(i)][1][italic(k)],
                                   " + ", italic(y)[italic(i)][2][italic(k)],
                                   ")", sep = ""))) +
    ggplot2::ylab(expression(paste("Differences (",
                                   italic(y)[italic(i)][2][italic(k)], " - ",
                                   italic(y)[italic(i)][1][italic(k)], ")",
                                   sep = ""))) +
    ggthemes::scale_color_ptol() + ggthemes::scale_fill_ptol() +
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
