
geom_gene_col <- function(gene_freq_df) {

  order_trunc %>%
    mutate(gene = forcats::fct_reorder(gene, n)) %>%
    ggplot(aes(x = gene, y = perc, fill = perc)) +
    geom_col() +
    theme_classic() +
    coord_flip() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "pt")) +

    scale_y_discrete(expand = c(0,0)) +

    guides(fill="none") +

    geom_text(aes(label = paste0(round(perc*100, 0), "%")),
              position = position_dodge(0.9),
              color = "white",
              hjust = 1,
              # color = ifelse(perc < 10, "black",  "white"),
              # vjust = 0.5, hjust = ifelse(perc < 10,
              #                             -0.2, 1.1),
              size = 2.5) +
    guides(fill="none")

}
