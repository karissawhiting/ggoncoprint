threshold <- .05

order <- get_gene_order(alteration_data)

order_perc <- order %>%
  mutate(perc = n/nrow(alteration_data))

order_trunc <- order_perc %>%
  filter(perc >= threshold)

alterations_data2 <- alteration_data %>%
  select(sample_id, contains(order_trunc$gene))

alterations_data2 <- sort_patients(alterations_data2)

plot_data <- prep_data_long(alterations_data2)

# reorder factor levels to be by gene frequency (this needs to go in one of the functions)
plot_data$gene <- factor(plot_data$gene, levels = rev(order_trunc$gene))

p_oncogrid <- geom_oncogrid(plot_data)

p_gene_freq <- geom_gene_col(order_trunc)

p_tmb <- geom_tmb(plot_data)

# Combine Plots -----------------------------------------------------------

p_sp <- plot_spacer() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks.length.y = unit(0, "pt")) +
  guides(fill="none")

p_tmb + p_sp + p_oncogrid + p_gene_freq+
  plot_layout(
    nrow = 2,
    widths = c(2, 1),
    heights = c(.5, 2))
p_oncogrid + p_gene_freq


