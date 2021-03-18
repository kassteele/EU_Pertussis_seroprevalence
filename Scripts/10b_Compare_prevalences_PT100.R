#
# Prevalences PT >= 100
# By "exact" Monte Carlo simulation
#

eupert_prev_data[c("PT≥100")] <- map(
  .x = eupert_prev_data[c("PT≥100")],
  .f = function(data) {

    data <- eupert_prev_data$"PT≥100"

    # prev_data contains the prevalances with 95% CI
    prev_data <- bind_rows(
      # We can only do this if n > 0
      data %>%
        filter(n > 0) %>%
        # Because the function p_sim is not vectorized, we must use rowwise
        rowwise() %>%
        mutate(
          p_sim = list(p_sim(x, n, sp = 0.98, se = 0.78))) %>% # sp en se specific for PT >= 100
        ungroup() %>%
        mutate(
          Prevalence = p_sim %>% map_dbl(median)                  %>% "*"(100) %>% round(digits = 2),
          lower      = p_sim %>% map_dbl(quantile, probs = 0.025) %>% "*"(100) %>% round(digits = 2),
          upper      = p_sim %>% map_dbl(quantile, probs = 0.975) %>% "*"(100) %>% round(digits = 2)),
      # If n > 0, set prevalences to NA
      data %>%
        filter(n == 0)) %>%
      # Restore original arrangement
      arrange(
        Country, Age_group, Sex)

    # prev_comp adds the pairwise comparisons
    prev_comp <- list(
      # Calculated prevalences
      prev_data,
      # For differences in Age group, add p-value to Age group = Total
      prev_data %>%
        filter(
          Age_group == "Total") %>%
        mutate(
          p_value_Age_group = exact_2x2_test(
            data1 = prev_data %>% filter(Age_group == "40-49"),
            data2 = prev_data %>% filter(Age_group == "50-59"),
            sim = TRUE)),
      # For differences in Sex, add p-value to Sex = Total
      prev_data %>%
        filter(
          Sex == "Total") %>%
        mutate(
          p_value_Sex = exact_2x2_test(
            data1 = prev_data %>% filter(Sex == "Males"),
            data2 = prev_data %>% filter(Sex == "Females"),
            sim = TRUE))) %>%
      # Put it back together
      reduce(
        .f = left_join) %>%
      # Select columns to keep
      select(
        Country, Age_group, Sex, COP, x, n, Prevalence, lower, upper, p_value_Age_group, p_value_Sex)

    # Return prev_comp
    return(prev_comp)
  })
