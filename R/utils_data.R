internal_complete_case_data <- function(data, outcome) {

  subjects    <- unique(data$subject)
  drop        <- NULL
  for (s in subjects) {
    data_s    <- dplyr::filter(data, subject == s)
    if (nrow(data_s) < length(strsplit(as.character(data_s$sequence[1]),
                                       split = "")[[1]])) {
      drop    <- c(drop, s)
    } else if (any(is.na(eval(outcome, data_s)))) {
      drop    <- c(drop, s)
    }
  }
  data        <- dplyr::filter(data, !(subject %in% drop))
  class(data) <- c(class(data), "xover_data")
  return(data)
}
