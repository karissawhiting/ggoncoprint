

geom_tmb <- function(plot_data) {

  tmb <- plot_data %>%
    filter(!is.na(value)) %>%
    group_by(sample_id) %>%
    mutate(total_alt = n()) %>%
    ungroup() %>%
    select(order_id, total_alt)

  tmb %>%
    ggplot(aes(x = order_id, y = total_alt, fill = total_alt)) +
    scale_fill_gradient(low = "palegreen", high = "palegreen4", na.value = NA) +
    geom_col() +
    theme_classic() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks.length.y = unit(0, "pt")) +
    guides(fill="none") +
    scale_x_continuous(expand = c(0,0))


}
