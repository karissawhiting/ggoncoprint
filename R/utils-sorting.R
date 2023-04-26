
#' Title
#'
#' @param alteration_df
#'
#' @return
#' @export
#'
#' @examples
sort_patients <- function(alteration_df) {

  alteration_df_raw <- alteration_df %>%
    gnomeR::rename_columns(.)

  alteration_df_raw <- alteration_df_raw %>%
    mutate(original_order = 1:nrow(.))

  alteration_df <- alteration_df_raw

  index_df <- alteration_df %>%
    select(original_order)

  if("sample_id" %in% colnames(alteration_df)) {

    index_df <- alteration_df %>%
      select(sample_id, original_order)

    alteration_df <- alteration_df %>%
      select(-c(sample_id))

  }

  ################################################# HERE
  scores <- apply(select(alteration_df, -c('original_order')), 1, calculate_patient_score)
  sample_order <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix

  alteration_df <- alteration_df[sample_order, ]

  alteration_df <-  alteration_df %>%
    left_join(index_df, alteration_df,
              by = "original_order") %>%
    select(-original_order) %>%
    mutate(order_id = 1:nrow(.)) %>%
    select(sample_id, order_id, everything())

  return(alteration_df)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
calculate_patient_score <- function(x) {
  x <- na.omit(x)
  score <- 0;
  for(i in 1:length(x)) {
    if(x[i]) {
      score <- score + 2^(length(x)-i)
    }
  }
  return(score)
}

#' Title
#'
#' @param alteration_df
#'
#' @return
#' @export
#'
#' @examples
get_gene_order <- function(alteration_df, threshold = 0.1) {

  alteration_df <- alteration_df %>%
    gnomeR::rename_columns(.)

  sum_bm <- alteration_df %>%
    gnomeR::summarize_by_gene()


  # order genes by frequency
  gene_freq <- colSums(select(sum_bm, -sample_id), na.rm = TRUE) %>%
    sort(., decreasing = TRUE) %>%
    tibble::enframe("gene", "n")

  return(gene_freq)
}

