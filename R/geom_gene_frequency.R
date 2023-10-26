
geom_gene_col <- function(gene_binary) {


  gene_perc_df <- gene_binary %>%
    get_gene_order()

  gene_perc_df <- data.frame(gene = names(gene_perc_df),
                             perc = gene_perc_df)

  gene_perc_df %>%
    mutate(gene = factor(gene, levels = rev(gene)))%>%
    ggplot(aes(x = gene, y = perc)) +
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

    scale_y_continuous(expand = c(0,0), limits = c(0,1)) +

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
