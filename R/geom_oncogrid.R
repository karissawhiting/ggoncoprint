

geom_oncogrid <- function(plot_data) {

  base_data <- create_base_data(plot_data)

  base_data  %>%
    ggplot() +
    geom_tile(aes(x = order_id, y = gene), fill = "lightgrey", color = "white", linewidth = 2) +
    geom_tile(data = filter(plot_data, value == "Amplification"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              color = "white",
              size = 1) +

    geom_tile(data = filter(plot_data, value == "Deletion"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              color = "white",
              size = 1) +

    geom_tile(data = filter(plot_data, value == "Mutation"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              height = .5,
              width = 1,
              color = "white",
              size = .1) +

    geom_tile(data = filter(plot_data, value == "Fusion"),
              aes(order_id, gene, fill = alt),
              alpha = .7,
              height = .5,
              width = 1,
              color = "white",
              size = .1) +
    scale_x_continuous(expand = c(0,0)) +
    # theme(legend.position = "left") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "pt"))


}
