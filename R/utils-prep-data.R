
.extract_varclass <- function(gene_binary){


  x <- attr(gene_binary, "var_class")

  if (is.null(x)){
    cli::cli_abort(paste0("{.field ", deparse(substitute(gene_binary)), "}
                          does not have a {.code var_class}
                   attribute. Please recreate this object using
                   {.code gnomeR::create_gene_binary()}
                   with {.code save_var_class = TRUE} and try again."))
  } else {
    return(x)
  }

}


.prep_data_long <- function(gene_binary) {

  varclass <- .extract_varclass(gene_binary)%>%
    mutate(across(var_class:type, ~stringr::str_to_lower(.)))
  sorted_binary <- sort_genes(gene_binary)
  sorted_binary <- sort_patients(sorted_binary)

  plot_df <- sorted_binary %>%
    tidyr::pivot_longer(!sample_id, values_drop_na = F) %>%
    tidyr::separate_wider_delim(name, names = c("hugo_symbol", "alt"),
                         too_few = "align_start", delim = ".") %>%
    mutate(
      type =
        case_when(
          alt == "Amp" ~ "cna",
          alt == "Del" ~ "cna",
          alt == "fus" ~ "fusion",
          is.na(alt) ~ "mutation"
        ))%>%
    left_join(varclass)%>%
    mutate(var_class = case_when(
      var_class == "gain" ~ "amplification",
      var_class == "loss" ~ "deletion",
      TRUE ~ var_class
    ))


  # if (!("panel_id" %in% colnames(plot_df))){
  #
  #   panels <- cbioportalR::get_panel_by_sample(sample_id = plot_df$sample_id)%>%
  #     rename(panel_id = genePanel,
  #            sample_id = sampleId)
  #
  #   test <- panels$panel_id
  #
  #   plot_df <- plot_df %>%
  #     left_join(panels %>% select(sample_id, panel_id))
  #
  #
  #   panels <- gnomeR::gene_panels[gnomeR::gene_panels$gene_panel %in%
  #                                   test,]
  #
  #   plot_df <- plot_df %>%
  #     mutate(na_on_panel = case_when(
  #       gene %in% panels$genes_in_panel[panels$gene_panel == panel_id] ~ 1,
  #       !(gene %in% panels$genes_in_panel[panels$gene_panel == panel_id]) ~ 0
  #     ))
  # }

  return(plot_df)

}

#NA handing should maybe go in this function?
create_base_data <- function(gene_binary) {
  samples <- gene_binary%>%
    select(sample_id)%>%
    distinct()

  genes <- gene_binary %>%
    select(hugo_symbol) %>%
    distinct()

  base_df <- dplyr::cross_join(samples, genes) %>%
    mutate(ind = 1)

  return(base_df)
}
