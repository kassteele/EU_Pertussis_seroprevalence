#
# Reversed cumulative density curves PT
#

fig_RCDC_PT <- ggplot(
  data = eupert_complete_data %>%
    filter(
      # Filter records with Age_group = Total and Sex = Total
      # Remove records with IgG_PT_IU_ml equal to LLOQ/2
      Age_group == "Total" & Sex == "Total",
      IgG_PT_IU_ml != 0.5*0.85) %>%
    mutate(
      # Sort countries by median IgG_PT_IU_ml
      Country = Country %>% fct_reorder(.x = IgG_PT_IU_ml)),
  mapping = aes(
    x = IgG_PT_IU_ml,
    colour = Country)) +
  geom_line(
    stat = "ecdf",
    lwd = 0.25) +
  scale_x_continuous(
    limits = c(NA, 1000),
    breaks = 10^(-3:3),
    minor_breaks = outer(c(2, 5), 10^(-3:3)) %>% as.vector()) +
  scale_y_reverse(
    breaks = seq(from = 0, to = 1, by = 0.2),
    minor_breaks = seq(from = 0.1, to = 0.9, by = 0.2),
    labels = seq(from = 100, to = 0, by = -20)) +
  scale_color_discrete_sequential(
    palette = "Viridis") +
  coord_trans(
    x = "log10") +
  labs(
    x = "IgG-PT (IU/mL)",
    y = "Percentage of samples") +
  theme_bw()

# Plot
print(fig_RCDC_PT)
