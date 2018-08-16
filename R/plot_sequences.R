#sequences$`Sequence Index` <- factor(sequences$`Sequence Index`,
#                                     levels =
#                                       rev(levels(sequences$`Sequence Index`)))
#max_seq_index <- length(levels(sequences$`Sequence Index`))
#if (max_seq_index > 20) {
#  facet_factor                 <-
#    as.numeric(as.character(sequences$`Sequence Index`))%/%20 + 1
#  for (i in seq(from = 20, by = 20, to = max_seq_index)) {
#    facet_factor[which(sequences$`Sequence Index` == i)] <- i/20
#  }
#  facet_factor                 <- paste("Sequences ", 20*(facet_factor - 1) + 1,
#                                        " to ", apply(cbind(20*facet_factor,
#                                                            max_seq_index), 1,
#                                                      min), sep = "")
#  sequences$facet_factor       <- factor(facet_factor,
#                                         levels = unique(facet_factor))
#  if (max_seq_index%/%20 != 0) {
#    sequences$`Sequence Index` <- factor(sequences$`Sequence Index`,
#                                         paste((20*(max_seq_index%/%20 + 1)):1))
#    counter                    <- 1
#    nrow_seq                   <- nrow(sequences)
#    for (i in (max_seq_index + 1):(20*(max_seq_index%/%20 + 1))) {
#      suppressWarnings(sequences[nrow_seq + counter, ] <-
#                         c(1, NA, NA, i, facet_factor[length(facet_factor)]))
#      counter                  <- counter + 1
#    }
#  }
#}

#plot   <- ggplot2::ggplot(data = sequences,
#                          ggplot2::aes(x = Period, y = `Sequence Index`)) +
#          ggplot2::geom_label(ggplot2::aes(label = Treatment, fill = Treatment),
#                              colour = "white", na.rm = T) +
#          ggplot2::ylab("Sequence") +
#          theme_xover() + ggplot2::theme(legend.position = "none") +
#          ggthemes::scale_fill_ptol()
#if (max_seq_index > 20) {
#  plot <- plot + ggplot2::facet_wrap(~facet_factor, scales = "free_y")
#}
#(plot)

