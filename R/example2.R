order <- get_gene_order(alteration_df)

if (!is.null(threshold)){
  order_perc <- order %>%
    mutate(perc = n/nrow(alteration_df))

  order <- order_perc %>%
    filter(perc >= threshold)}

order <- order %>%
  rbind(data.frame(gene = "ZFHX3",
                   n = 1,
                   perc = 0.01))

alterations_df2 <- alteration_df %>%
  select(sample_id, contains(c(order$gene)))

alterations_df2 <- sort_patients(alterations_df2)

map_dbl(alterations_df2 %>% select(-order_id), ~sum(is.na(.)))
colnames(alteration_df)


plot_df <- alterations_df2 %>%
  select(ends_with("id"), starts_with("ZFHX3"))%>%
  tidyr::pivot_longer(-c(order_id, sample_id)) %>%
  tidyr::separate_wider_delim(name, names = c("gene", "alt"),
                              too_few = "align_start", delim = ".")%>%
  mutate(
    alt =
      case_when(
        alt == "Amp" ~ "Amplification",
        alt == "Del" ~ "Deletion",
        alt == "fus" ~ "Fusion",
        is.na(alt) ~ "Mutation"
      ),
    type_alt = case_when(
      value == 1 ~ alt,
      value == 0 ~ NA_character_
    ))

plot_df <- plot_df %>%
  mutate(gene_order =
           factor(gene, levels = rev(order$gene)))%>%
  arrange(gene_order)%>%
  filter(!is.na(gene_order))

map_dbl(plot_df, ~sum(is.na(.)))


p_oncogrid <- geom_oncogrid(plot_df)

p_gene_freq <- geom_gene_col(order_trunc)

p_tmb <- geom_tmb(plot_df)

# Combine Plots -----------------------------------------------------------

p_sp <- patchwork::plot_spacer() +
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
  patchwork::plot_layout(
    nrow = 2,
    widths = c(2, 1),
    heights = c(.5, 2))
p_oncogrid + p_gene_freq

