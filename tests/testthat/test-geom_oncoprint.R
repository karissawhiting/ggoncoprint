# table(gnomeR::which_impact_panel(hugo_symbol = rev(order_trunc$gene)) %>% count(IMPACT341, IMPACT410)
#       table(gnomeR::which_impact_panel(hugo_symbol = rev(order$gene)) %>% count(IMPACT341, IMPACT410, IMPACT468,
                                                                                IMPACT505))


#table(gnomeR::which_impact_panel(hugo_symbol = rev(order$gene)) %>% count(IMPACT505)

#examples of genes that aren't in 410 but are in 505

test_that("NAs work", {


  alteration_data <- cbind(alteration_data, data.frame(SESN3 = rep(1, nrow(alteration_data)),
                                                       ZNRF3 = rep(1, nrow(alteration_data))))


  plot_data <- .prep_data_long(alteration_data)

  p_oncogrid <- geom_oncogrid(plot_data)

  p_gene_freq <- geom_gene_col(order_trunc)

  p_tmb <- geom_tmb(plot_data)

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


}

)
