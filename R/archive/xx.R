# # data ------------
# library(gnomeR)
# library(tidyverse)
#
# gen <- data.frame(sample_id = letters[1:15],
#                   TP53 = sample(c(0,1), 15, replace = TRUE),
#                   TP53.Del = sample(c(0,1), 15, replace = TRUE),
#                   TP53.Amp = sample(c(0,1), 15, replace = TRUE),
#                   APC = sample(c(0,1), 15, replace = TRUE),
#                   APC.fus = sample(c(0,1), 15, replace = TRUE),
#                   PIK3CA = sample(c(0,1), 15, replace = TRUE),
#                   BRAF = sample(c(0,1), 15, replace = TRUE))
#
#
# new_df <- summarize_by_gene(gene_binary = gen)
# sort_patients(select(new_df, -sample_id))
#
#
# # ------------------------------------------------------------------------------
# key <- data.frame(
#   value = c(0:5),
#   alteration_label = c(
#     "No Alteration",
#     "Mutation",
#     "Fusion",
#     "Deletion",
#     "Amplification",
#     "Missing"
#   ),
#   color = c(
#     "#D4D6D4",
#     "#038054",
#     "orange",
#     "blue",
#     "darkred",
#     "grey"
#   ),
#   bar_height = c(
#     1,
#     .5,
#     .5,
#     1,
#     1,
#     1),
#   bar_width = c(
#     1,
#     1,
#     1,
#     .5,
#     .5,
#     1)
# )
#
# # ------------------------------------------------------------------------------
# alteration_df_long <- p
#
# alteration_df_long
#
#
# ggoncoprint <- function(alteration_df_long) {
#
#   alteration_df_long <- alteration_df_long %>%
#     mutate(name = fct_reorder(name, order)) %>%
#     group_by(name) %>%
#     arrange(value, desc = TRUE, .by_group = TRUE) %>%
#     #  distinct() %>%
#     mutate(value = fct_relevel(value, c("0", "1"))) %>%
#     mutate(order2 = case_when(
#       value == "3" ~ order + .25,
#       value == "4" ~ order - .25,
#       TRUE ~ as.numeric(order)))
#
#   br <- sort(unique(alteration_df_long$value))
#   key$alteration_label[which(key$value %in% br)]
#
#   alteration_df_long %>%
#     ggplot(., aes(x=order, y=name)) +
#     geom_tile(fill = "#D4D6D4") +
#
#     geom_tile(color = "black",
#               aes(x=order2, y=name,
#                   fill = value)) +
#             #  ,
#             #  height = bar_height_fun(alteration_df_long),
#             #  width = bar_width_fun(alteration_df_long$value), alpha = 1) +
#     scale_fill_manual(
#       values = key$color[1:2],
#                       breaks = br,
#                     #  labels = label_fun,
#                       limits = br) +
#
#     # need this just for now because cant get layering right on above
#     geom_tile(data = filter(alteration_df_long, value %in% c(1, 2)),
#               color = "white",
#               aes(x=order2, y=name,
#                   fill = as.factor(value)),
#               height = .5, alpha = 1) +
#     theme_classic() +
#     scale_x_continuous(breaks = 1:nrow(alteration_df_long), expand = c(0,0 )) +
#     scale_y_discrete(expand = c(0, 0))
#
#   #one
#   # theme(plot.margin = margin(0, 0, 0, 0),
#   #       legend.title = element_blank(),
#   #       axis.line.x = element_blank(),
#   #       axis.ticks.x = element_blank(),
#   #       axis.text.x = element_blank(),
#   #       axis.title.x = element_blank(),
#   #       axis.line.y = element_blank())
#
#  # one
# }
#
