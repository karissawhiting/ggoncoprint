
.prep_data_long <- function(alteration_df, threshold = NULL) {

  if (!("panel_id" %in% colnames(alteration_df))){
    alteration_df <- alteration_df %>%
      mutate(panel_id = cbioportalR::get_panel_by_sample(sample_id = sample_id))
  }

  order <- get_gene_order(alteration_df)

  order_perc <- order %>%
    mutate(perc = n/nrow(alteration_df))

  order_trunc <- order_perc %>%
    filter(perc >= threshold)

  alterations_df2 <- alteration_df %>%
    select(sample_id, contains(order_trunc$gene))

  alterations_df2 <- sort_patients(alterations_df2)

  plot_df <- alterations_df2 %>%
    tidyr::pivot_longer(-c(order_id, sample_id)) %>%
    tidyr::separate_wider_delim(name, names = c("gene", "alt"),
                         too_few = "align_start", delim = ".") %>%
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



  plot_genes_df <- plot_df %>%
    group_by(sample_id, gene)%>%
    summarise(n = n())%>%
    mutate(panel = )

  plot_df <- plot_df %>%
    left_join(plot_genes_df)%>%
    select(-"max")%>%
    mutate(gene_order =
             factor(gene, levels = rev(order_trunc$gene)))%>%
    arrange(gene_order)%>%
    filter(!is.na(gene_order))


  return(plot_df)

}

#NA handing should maybe go in this function?
create_base_data <- function(long_alterations) {
  samples <- long_alterations %>%
    select(order_id) %>%
    distinct()

  genes <- long_alterations %>%
    select(gene) %>%
    distinct()


  base_df <- dplyr::cross_join(samples, genes) %>%
    mutate(ind = 1)

  return(base_df)
}
