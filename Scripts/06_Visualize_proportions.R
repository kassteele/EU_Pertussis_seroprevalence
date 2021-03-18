#
# Visualize proportions of PT, Dt and TT
#

# Create eupert_prop_data
eupert_prop_data <- eupert_complete_data %>%
  filter(
    Age_group == "Total" & Sex == "Total") %>%
  mutate(
    IgG_PT = IgG_PT_IU_ml %>% cut(
      breaks = c(0, 0.85, 50, 100, Inf),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("IgG-PT<0.85 IU/mL", "0.85≥IgG-PT<50 IU/mL", "50≥IgG-PT<100 IU/mL", "IgG-PT≥100 IU/mL")) %>%
      fct_rev(),
    IgG_Dt = IgG_Dt_IU_ml %>% cut(
      breaks = c(0, 0.01, 0.1, Inf),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("IgG-Dt<0.01 IU/mL", "0.01≥IgG-Dt<0.1 IU/mL", "IgG-Dt≥0.1 IU/mL")) %>%
      fct_rev(),
    IgG_TT = IgG_TT_IU_ml %>% cut(
      breaks = c(0, 0.01, 0.1, Inf),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("IgG-TT<0.01 IU/mL", "0.01≥IgG-TT<0.1 IU/mL", "IgG-TT≥0.1 IU/mL")) %>%
      fct_rev()) %>%
  select(
    Country, IgG_PT, IgG_Dt, IgG_TT)

# Create empty list
fig_prop <- list()

# Theme is the same everywhere
theme_prop <- list(
  geom_bar(
    position = "fill"),
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.2),
    expand = expansion(mult = 0, add = 0),
    labels = scales::percent),
  labs(
    x = "",
    y = ""),
  theme_bw())

# Proportions of PT
fig_prop$PT <- ggplot(
  data = eupert_prop_data,
  mapping = aes(x = Country, fill = IgG_PT)) +
  scale_fill_discrete_sequential(
    name = "",
    palette = "Viridis",
    rev = FALSE) +
  theme_prop

# Proportions of Dt
fig_prop$Dt <- ggplot(
  data = eupert_prop_data,
  mapping = aes(x = Country, fill = IgG_Dt)) +
  scale_fill_discrete_sequential(
    name = "",
    palette = "Viridis") +
  theme_prop

# Proportions of TT
fig_prop$TT <- ggplot(
  data = eupert_prop_data,
  mapping = aes(x = Country, fill = IgG_TT)) +
  scale_fill_discrete_sequential(
    name = "",
    palette = "Viridis") +
  theme_prop

# Print
print(fig_prop$PT / fig_prop$Dt / fig_prop$TT)
