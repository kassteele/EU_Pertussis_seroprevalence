#
# Test overall effect Country, Age_group and Sex on prevalences
#

table_overall_effects <- eupert_complete_data %>%
  select(-Sample_number, `PT≥100`:`TT<0.1`, -starts_with("IgG")) %>%
  # For Age_group known and Sex known. This excludes GR and NO
  filter(Age_group != "Total" & Sex != "Total") %>%
  # Put in long format for analysis
  pivot_longer(
    cols = -c(Country:Sex),
    names_to = "COP",
    values_to = "Outcome") %>%
  # For each COP
  split(f = .$COP) %>%
  map_dfr(
    .id = "COP",
    .f = function(data) {

      # For PT>=100, set sp and se in logit link function
      # If not, use default logit link function
      if (data$COP[1] == "PT≥100") {
        my_logit <- logit_sp_se(sp = 0.98, se = 0.78)
      } else {
        my_logit <- logit_sp_se(sp = 1, se = 1)
      }

      # Overall Age_group effect
      mod1 <- glm(
        formula = Outcome ~ Country * Age_group * Sex,
        family = binomial(link = my_logit),
        data = data)
      mod0 <- glm(
        formula = Outcome ~ Country * Sex,
        family = binomial(link = my_logit),
        data = data)
      p_value_Age_group <- anova(mod1, mod0, test = "Chisq")$`Pr(>Chi)`[2] %>% round(digits = 3)

      # Overall Sex effect
      mod1 <- glm(
        formula = Outcome ~ Country * Age_group * Sex,
        family = binomial(link = my_logit),
        data = data)
      mod0 <- glm(
        formula = Outcome ~ Country * Age_group,
        family = binomial(link = my_logit),
        data = data)
      p_value_Sex <- anova(mod1, mod0, test = "Chisq")$`Pr(>Chi)`[2] %>% round(digits = 3)

      # Overall Country effect
      mod1 <- glm(
        formula = Outcome ~ Country * Age_group * Sex,
        family = binomial(link = my_logit),
        data = data)
      mod0 <- glm(
        formula = Outcome ~ Age_group * Sex,
        family = binomial(link = my_logit),
        data = data)
      p_value_Country <- anova(mod1, mod0, test = "Chisq")$`Pr(>Chi)`[2] %>% round(digits = 3)

      # Put in table
      tibble(
        Age_group = p_value_Age_group,
        Sex       = p_value_Sex,
        Country   = p_value_Country)

    })
