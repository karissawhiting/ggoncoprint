

geom_oncogrid <- function(plot_data) {

  plot_data <- .prep_data_long(genie_nsclc_data)

  base_data <- create_base_data(plot_data)

  observed <- plot_data %>%
    filter(value == 1)

  cna <- observed %>%
    filter(type == "cna")%>%
    rename(CNA = var_class)

  fus <- observed %>%
    filter(type == "fusion")%>%
    mutate(Fusion = ifelse(is.na(var_class),
                           "unspecified", var_class))


  na_on_panel <- plot_data %>%
    filter(is.na(value))%>%
    select(sample_id, hugo_symbol)%>%
    distinct()

  mut <- observed %>%
    filter(type == "mutation")

  order_mut <- mut %>% count(var_class) %>%
    arrange(desc(n))%>%
    pull(var_class)

  mut <- mut %>%
    mutate(var_class = factor(var_class,
                                 levels = order_mut))

  test <- rev(palette.colors(palette = "Okabe-Ito"))
  test <- c(test[2:(length(test)-1)], "#7570b3",
            "#984ea3")
  names(test) <- NULL

  base_data  %>%
    ggplot() +
    geom_tile(aes(x = sample_id, y = hugo_symbol), color = "white", linewidth = 2)+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "pt"))+
      geom_tile(data = mut,
              aes(sample_id, hugo_symbol, fill = var_class),
              height = 0.5)+
    geom_tile(data = cna,
              aes(sample_id, hugo_symbol,
                  color = CNA),
              width = 0.1)+
    scale_color_manual("CNA", values = c("red", "blue"))+
  geom_tile(data = na_on_panel, aes(x = sample_id, y = hugo_symbol),
            color = 'lightgrey', fill = "lightgrey")+
  geom_point(data = fus,
              aes(sample_id, hugo_symbol, shape = Fusion),
              size = 1)+
  theme(legend.position = "left")+
    scale_fill_manual(values = test)+
    labs(fill = "Mutation")
    #scale_color_manual("Fusion", values = c("purple"))
    # scale_fill_manual(values=c("red", "blue"))
    #scale_x_continuous(expand = c(0,0))+
    #labs(shape = "Split legend")
}
