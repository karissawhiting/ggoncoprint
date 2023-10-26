library(gnomeR)
library(genieBPC)
library(dplyr)

nsclc <- pull_data_synapse("NSCLC", "v2.0-public")

mut <- nsclc$NSCLC_v2.0$mutations_extended %>%
  left_join(nsclc$NSCLC_v2.0$cpt %>%
              select(cpt_genie_sample_id, institution,
                     cpt_seq_assay_id)%>%
              rename(panel_id = cpt_seq_assay_id,
                     Tumor_Sample_Barcode = cpt_genie_sample_id))


cna <- nsclc$NSCLC_v2.0$cna%>%
  mutate(across(!Hugo_Symbol, ~ifelse(. == "NA", NA, as.numeric(.))))%>%
  pivot_cna_longer()

fus <- nsclc$NSCLC_v2.0$fusions %>%
  reformat_fusion()%>%
  mutate(variantClass = NA)

samp_panel <- mut %>%
  select(Tumor_Sample_Barcode, panel_id, institution)

samp_panel <- split(samp_panel, samp_panel$institution)%>%
  purrr::map(., function(x){x %>% distinct() %>% head(20)})%>%
  do.call(rbind, .)%>%
  select(-institution)%>%
  rename(sample_id = Tumor_Sample_Barcode)

samps <- samp_panel$sample_id

gene_binary <- gnomeR::create_gene_binary(mutation = mut,
                                          cna = cna,
                                          fusion = fus,
                                          samples = samps,
                                          specify_panel = samp_panel,
                                          save_var_class = TRUE)

# Filter out genes with less than 25% prevalence
gene <- gene_binary %>%
  summarize_by_gene()%>%
  subset_by_frequency(0.25)%>%
  names()%>%
  as.data.frame()%>%
  cross_join(., as.data.frame(c("", ".Amp", ".Del", ".fus")))%>%
  mutate(gene = paste0(`.`, `c("", ".Amp", ".Del", ".fus")`))

genie_nsclc_data <- gene_binary %>%
  select(any_of(gene$gene))

genie_nsclc_varclass <- genie_nsclc_data %>%
  attr(., "var_class")

usethis::use_data(genie_nsclc_data, overwrite = TRUE)
usethis::use_data(genie_nsclc_varclass, overwrite = T)


usethis::use_data(genie_nsclc_data, overwrite = TRUE)
