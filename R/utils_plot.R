find_hull_data <- function(data) {
  data[grDevices::chull(data$total, data$diff), ]
}
