
#' Sort patients by number of mutations
#'
#' Most mutations at the top to least at the bottom
#'
#' @param gene_binary
#'
#' @return
#' @export
#'
#' @examples
sort_patients <- function(gene_binary) {


  gene_binary2 <- gene_binary %>%
    gnomeR::rename_columns(.)

  if(!("sample_id" %in% names(gene_binary2))){
    cli::cli_abort("sample_id column is missing.")
  }

  alteration_df <- gene_binary2 %>%
    rowwise()%>%
    mutate(
           alt_num = sum(c_across(is.numeric), na.rm = T))%>%
    arrange(desc(alt_num))%>%
    select(-alt_num)

  return(alteration_df)
}


#' Title
#'
#' @param x
#'
#'  https://gist.github.com/dakl/5974ac1d78dac88c1c78
#' @return
#' @export
#'
#' @examples
calc_gene_scores <- function(gene) {
  col_values <- na.omit(gene)
  score <- 0

  score <- sum(purrr::imap_dbl(col_values, function(x, idx){
    if(x == 1){ #if mutated
      2^(length(col_values) - idx)
    } else {
      NA
    }
  }), na.rm = T)

  return(score)
}

#' Order genes based on mutational frequency
#'
#' @param gene_binary
#'
#' @return
#' @export
#'
#' @examples
sort_genes <- function(gene_binary) {

  alt_only <- gene_binary %>%
    select(-"sample_id")
  # Remove all NA columns ----------------------------------------------
  # all names but sample_id
  all_na_alt <- apply(alt_only,  2, function(x) {
    sum(is.na(x)) == nrow(gene_binary)
  })

  all_non_na_alt <- names(all_na_alt[!all_na_alt])
  alt_only <- select(alt_only, all_of(all_non_na_alt))

  gene_order <- purrr::map(alt_only, calc_gene_scores)%>%
    unlist()%>%
    sort(., decreasing = T)%>%
    names(.)

  sorted_binary <- gene_binary %>%
    select(sample_id, all_of(names(alt_only)))

  sorted_binary <- sorted_binary[, c("sample_id", gene_order)]

  return(sorted_binary)
}

