# library(cbioportalR)
# library(tidyverse)
#
#
#
# # check here next time
# p_oncogrid <- geom_oncogrid(genie_nsclc_data)
#
# p_gene_freq <- geom_gene_col(plot_data)
#
# p_tmb <- geom_tmb(plot_data)
#
# # Combine Plots -----------------------------------------------------------
#
# p_sp <- patchwork::plot_spacer() +
#   theme(
#     plot.margin = margin(0, 0, 0, 0),
#     axis.text.y = element_blank(),
#     axis.title.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title = element_blank(),
#     axis.line = element_blank(),
#     axis.ticks.length.y = unit(0, "pt")) +
#   guides(fill="none")
#
# p_tmb + p_sp + p_oncogrid + p_gene_freq+
#   patchwork::plot_layout(
#     nrow = 2,
#     widths = c(2, 1),
#     heights = c(.5, 2))
# p_oncogrid + p_gene_freq
#
