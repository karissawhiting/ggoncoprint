order <- get_gene_order(alteration_df)

if (!is.null(threshold)){
  order_perc <- order %>%
    mutate(perc = n/nrow(alteration_df))

  order <- order_perc %>%
    filter(perc >= threshold)}


alterations_df2 <- alteration_df %>%
  select(contains(c(order$gene, "ZFHX3")

colnames(alteration_df)
