#
# Prevalences TT<0.01, TT<0.1
# By exact Binomial methods
#

eupert_prev_data[c("TT<0.01", "TT<0.1")] <- map(
  .x = eupert_prev_data[c("TT<0.01", "TT<0.1")],
  .f = function(data) {

    # prev_data contains the prevalances with 95% CI
    prev_data <- bind_rows(
      # We can only do this if n > 0
      data %>%
        filter(n > 0) %>%
        # Because function binom.exact is not vectorized, we must use rowwise
        rowwise() %>%
        mutate(
          h = list(binom.exact(x = x, n = n, tsmethod = "minlike") %>% unclass())) %>%
        ungroup() %>%
        mutate(
          Prevalence = h %>% map_dbl(~ .x$estimate    %>% "*"(100) %>% round(digits = 2)),
          lower      = h %>% map_dbl(~ .x$conf.int[1] %>% "*"(100) %>% round(digits = 2)),
          upper      = h %>% map_dbl(~ .x$conf.int[2] %>% "*"(100) %>% round(digits = 2))),
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
            sim = FALSE)),
      # For differences in Sex, add p-value to Sex = Total
      prev_data %>%
        filter(
          Sex == "Total") %>%
        mutate(
          p_value_Sex = exact_2x2_test(
            data1 = prev_data %>% filter(Sex == "Males"),
            data2 = prev_data %>% filter(Sex == "Females"),
            sim = FALSE))) %>%
      # Put it back together
      reduce(
        .f = left_join) %>%
      # Select columns to keep
      select(
        Country, Age_group, Sex, COP, x, n, Prevalence, lower, upper, p_value_Age_group, p_value_Sex)

    # Return prev_comp
    return(prev_comp)
  })
