
.prep_data_long <- function(alteration_df, threshold = NULL) {

  order <- get_gene_order(alteration_df)

  if (!is.null(threshold)){
    order_perc <- order %>%
      mutate(perc = n/nrow(alteration_df))

  order <- order_perc %>%
    filter(perc >= threshold)}


  alterations_df2 <- alteration_df %>%
    select(sample_id, contains(order$gene))

  alterations_df2 <- sort_patients(alterations_df2)

  plot_df <- alterations_df2 %>%
    tidyr::pivot_longer(-c(order_id, sample_id), values_drop_na = F) %>%
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

  plot_df <- plot_df %>%
    mutate(gene_order =
             factor(gene, levels = rev(order$gene)))%>%
    arrange(gene_order)%>%
    filter(!is.na(gene_order))

  #have to use this because my cbio won't connect
  if (!("panel_id" %in% colnames(plot_df))){
    set_cbioportal_db("msk")
    panels <- cbioportalR::get_panel_by_sample(sample_id = plot_df$sample_id)%>%
      rename(panel_id = genePanel,
             sample_id = sampleId)

    test <- panels$panel_id

    plot_df <- plot_df %>%
      left_join(panels %>% select(sample_id, panel_id))


    panels <- gnomeR::gene_panels[gnomeR::gene_panels$gene_panel %in%
                                    test,]

    plot_df <- plot_df %>%
      mutate(na_on_panel = case_when(
        gene %in% panels$genes_in_panel[panels$gene_panel == panel_id] ~ 1,
        !(gene %in% panels$genes_in_panel[panels$gene_panel == panel_id]) ~ 0
      ))
  }


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
