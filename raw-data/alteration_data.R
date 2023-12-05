
library(gnomeR)
library(dplyr)

gene_binary <- gnomeR::create_gene_binary(mutation = gnomeR::mutations,
                   cna = gnomeR::cna,
                   fusion = gnomeR::sv,
                   specify_panel = "impact",
                   save_var_class = TRUE)

# Filter out genes with less than 1% prevalence
alteration_data <- gene_binary %>%
  gnomeR::subset_by_frequency(t = 0.01)

usethis::use_data(alteration_data, overwrite = TRUE)
