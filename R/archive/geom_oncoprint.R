
# ------------------------------------------------------------------------------
key <- data.frame(
  value = c(0:5),
  alteration_label = c(
    "No Alteration",
    "Mutation",
    "Fusion",
    "Deletion",
    "Amplification",
    "Missing"
  ),
  color = c(
    "#D4D6D4",
    "#038054",
    "orange",
    "blue",
    "darkred",
    "grey"
  ),
  bar_height = c(
    1,
    .5,
    .5,
    1,
    1,
    1),
  bar_width = c(
    1,
    1,
    1,
    .5,
    .5,
    1)
)

# ------------------------------------------------------------------------------
alteration_df_long <- p

alteration_df_long


ggoncoprint <- function(alteration_df_long) {

  alteration_df_long <- alteration_df_long %>%
    mutate(name = fct_reorder(name, order)) %>%
    group_by(name) %>%
    arrange(value, desc = TRUE, .by_group = TRUE) %>%
    #  distinct() %>%
    mutate(value = fct_relevel(value, c("0", "1", "2", "3", "4"))) %>%
    mutate(order2 = case_when(
      value == "3" ~ order + .25,
      value == "4" ~ order - .25,
      TRUE ~ as.numeric(order)))

  br <- sort(unique(one$value))
  key$alteration_label[which(key$value %in% br)]

  one %>%
    ggplot(., aes(x=order, y=name)) +
    geom_tile(fill = "#D4D6D4") +

    geom_tile(color = "white",
              aes(x=order2, y=name,
                  fill = value),
              height = bar_height_fun(one$value),
              width = bar_width_fun(one$value), alpha = 1) +
    scale_fill_manual(values = color_fun(br),
                      breaks = br,
                      labels = label_fun,
                      limits = br) +

    # need this just for now because cant get layering right on above
    geom_tile(data = filter(one, value %in% c(1, 2)),
              color = "white",
              aes(x=order2, y=name,
                  fill = as.factor(value)),
              height = .5, alpha = 1) +
    theme_classic() +
    scale_x_continuous(breaks = 1:nrow(one), expand = c(0,0 )) +
    scale_y_discrete(expand = c(0, 0))

  #one
  # theme(plot.margin = margin(0, 0, 0, 0),
  #       legend.title = element_blank(),
  #       axis.line.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.line.y = element_blank())

  one

