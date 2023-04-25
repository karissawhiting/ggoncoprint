
prep_data_long <- function(alteration_df) {

  plot_df <- alteration_df %>%
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
        )
    ) %>%
    mutate(value = case_when(
      value == 1 ~ alt,
      value == 0 ~ NA_character_
    ))

}

# NA handing should maybe go in this function?
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
