#
# Visualize GMCs PT
#

# Create Sex (colours) by Age_group (facets) plot
fig_GMC_PT <- ggplot(
  data = eupert_gmc_data %>%
    filter(
      Antigen == "PT") %>%
    mutate(
      Country = Country %>% fct_rev(),
      Age_group = Age_group %>% fct_recode(`40-49 years` = "40-49", `50-59 years` = "50-59"),
      Sex = Sex %>% fct_relevel(c("Males", "Total", "Females")),
      p_value_Sex = p_value_Sex %>% formatC(format = "f", digits = 3) %>% str_remove(pattern = "  NA")),
  mapping = aes(
    x = Country,
    y = GMC,
    ymin = lower,
    ymax = upper,
    colour = Sex,
    label = p_value_Sex)) +
  geom_point(
    position = position_dodge(width = -0.7),
    size = 1) +
  geom_errorbar(
    position = position_dodge(width = -0.7),
    size = 0.5,
    width = 0) +
  geom_text(
    y = 24,
    size = 3,
    show.legend = FALSE) +
  scale_x_discrete(
    expand = expansion(add = c(0.75, 1.5))) +
  scale_y_continuous(
    limits = c(NA, 24)) +
  scale_colour_manual(
    values = c("#56B4E9", "#000000", "#E69F00"),
    labels = c("Males", "Total", "Females")) +
  coord_flip() +
  labs(
    x = "",
    y = "GMC (IU/mL)",
    colour = "",
    title = "IgG-PT") +
  annotate(
    geom = "text",
    x = 19,
    size = 3,
    y = 24,
    label = "p-value") +
  facet_wrap(
    facets = vars(Age_group),
    nrow = 3, ncol = 1) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5))

# Print
print(fig_GMC_PT)
