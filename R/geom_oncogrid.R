

geom_oncogrid <- function(plot_data) {


  base_data <- create_base_data(plot_data)

  plot_data  %>%
    filter(!is.na(gene_order))%>%
    ggplot() +
    geom_tile(aes(x = order_id, y = gene_order), fill = "lightgrey", color = "white", linewidth = 2)+
    geom_tile(data = filter(plot_data, na_on_panel == 1), aes(x = order_id, y = gene_order), fill = 'lightgrey', color = 'white', linewidth = 2)+

    geom_tile(data = filter(plot_data, type_alt == "Amplification"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              size = 1) +

    geom_tile(data = filter(plot_data, type_alt == "Deletion"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              size = 1) +

    geom_tile(data = filter(plot_data,  type_alt == "Mutation"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              height = .5,
              width = 1,
              color = "white",
              size = .1) +

    geom_tile(data = filter(plot_data, type_alt == "Fusion"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              height = .5,
              width = 1,
              color = "white",
              size = .1)+
    scale_x_continuous(expand = c(0,0)) +
    # theme(legend.position = "left") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "pt"))


}
