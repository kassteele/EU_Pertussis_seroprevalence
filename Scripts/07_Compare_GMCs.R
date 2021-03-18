#
# Model GMC's
#

eupert_gmc_data <- eupert_complete_data %>%
  select(Country:IgG_TT_IU_ml, -Sample_number) %>%
  pivot_longer(
    cols = starts_with("IgG"),
    names_to = "Antigen",
    values_to = "IgG_IU_ml") %>%
  mutate(
    Antigen = Antigen %>%
      str_remove("IgG_") %>%
      str_remove("_IU_ml")) %>%
  split(
    f = .$Antigen) %>%
  map_dfr(
    .id = "Antigen",
    .f = function(data) {

      # Model for Age_group x Sex, by Country
      gmc_fit_Age_group_Sex <- lm(
        formula = log(IgG_IU_ml) ~ Country * Age_group * Sex,
        data = data %>% filter(Age_group != "Total" & Sex != "Total"))
      # Model for Age_group, total Sex, by Country
      gmc_fit_Age_group <- lm(
        formula = log(IgG_IU_ml) ~ Country * Age_group,
        data = data %>% filter(Age_group != "Total" & Sex == "Total"))
      # Model for Sex, total Age_group, by Country
      gmc_fit_Sex <- lm(
        formula = log(IgG_IU_ml) ~ Country * Sex,
        data = data %>% filter(Age_group == "Total" & Sex != "Total"))
      # Model for total Age_group, total Sex, by Country
      gmc_fit <- lm(
        formula = log(IgG_IU_ml) ~ Country,
        data = data %>% filter(Age_group == "Total" & Sex == "Total"))

      # Get log-GMC's by lsmeans
      gmc_lsmeans_Age_group_Sex <- gmc_fit_Age_group_Sex %>% lsmeans(specs = ~ Country * Age_group * Sex)
      gmc_lsmeans_Age_group     <- gmc_fit_Age_group     %>% lsmeans(specs = ~ Country * Age_group)
      gmc_lsmeans_Sex           <- gmc_fit_Sex           %>% lsmeans(specs = ~ Country * Sex)
      gmc_lsmeans               <- gmc_fit               %>% lsmeans(specs = ~ Country)

      # Bind together in gmc_means
      gmc_means <- bind_rows(
        gmc_lsmeans_Age_group_Sex %>% as_tibble(),
        gmc_lsmeans_Age_group     %>% as_tibble(),
        gmc_lsmeans_Sex           %>% as_tibble(),
        gmc_lsmeans               %>% as_tibble()) %>%
        # NA's are group "Total"
        mutate(
          Age_group = Age_group %>% fct_explicit_na("Total"),
          Sex       = Sex    %>% fct_explicit_na("Total")) %>%
        # Make sure Country levels are complete
        mutate(
          Country = factor(Country, levels = data %>% pull(Country) %>% levels())) %>%
        # Complete all combinations and arange records
        complete(Country, Age_group, Sex) %>%
        arrange(Country, Age_group, Sex) %>%
        # Convert to GMC's
        mutate(
          GMC   = lsmean   %>% exp(),
          lower = lower.CL %>% exp(),
          upper = upper.CL %>% exp())

      # Create gmc_table
      gmc_table <- list(
        # The existing GMC table
        gmc_means %>%
          select(Country, Age_group, Sex, GMC, lower, upper),
        # Pairwise comparisons Age_group...
        bind_rows(
          # ...conditional on Country and Sex
          gmc_lsmeans_Age_group_Sex %>%
            pairs(by = c("Country", "Sex"), name = "Age_group", adjust = NULL) %>%
            as_tibble(),
          # ...conditional on Country (Sex = "Total")
          gmc_lsmeans_Age_group %>%
            pairs(by = "Country", name = "Age_group", adjust = NULL) %>%
            as_tibble() %>%
            add_column(Sex = factor("Total", levels = data %>% pull(Sex) %>% levels()))) %>%
          # Rename Age_group to Total, to join with these rows in gmc_means
          mutate(Age_group = factor("Total", levels = data %>% pull(Age_group) %>% levels())) %>%
          select(Country, Age_group, Sex, p_value_Age_group = p.value),
        # Pairwise comparisons Sex...
        bind_rows(
          # ...conditional on Country and Age_group
          gmc_lsmeans_Age_group_Sex %>%
            pairs(by = c("Country", "Age_group"), name = "Sex", adjust = NULL) %>%
            as_tibble(),
          # ...conditional on Country (Age_group = "Total")
          gmc_lsmeans_Sex %>%
            pairs(by = "Country", name = "Sex", adjust = NULL) %>%
            as_tibble() %>%
            add_column(Age_group = factor("Total", levels = data %>% pull(Age_group) %>% levels()))) %>%
          # Rename Sex to Total, to join with these rows in gmc_means
          mutate(Sex = factor("Total", levels = data %>% pull(Sex) %>% levels())) %>%
          select(Country, Age_group, Sex, p_value_Sex = p.value)) %>%
        # Join
        reduce(.f = left_join) %>%
        arrange(Country, Age_group, Sex)

      # Return table
      gmc_table
    })

# Print
print(eupert_gmc_data)
