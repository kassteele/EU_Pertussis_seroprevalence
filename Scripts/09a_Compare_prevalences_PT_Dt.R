#
# Prevalences 50≥PT<100, PT≥50, Dt<0.01, Dt<0.1
# By Binomial GLM
#

eupert_prev_data[c("50≥PT<100", "PT≥50", "Dt<0.01", "Dt<0.1")] <- map(
  .x = eupert_prev_data[c("50≥PT<100", "PT≥50", "Dt<0.01", "Dt<0.1")],
  .f = function(data) {

    # Model for Age_group x Sex, by Country
    prev_fit_Age_group_Sex <- glm(
      formula = cbind(x, n - x) ~ Country * Age_group * Sex,
      family = binomial(link = "logit"),
      control = list(maxit = 100),
      data = data %>% filter(Age_group != "Total" & Sex != "Total"))
    # Model for Age_group, total Sex, by Country
    prev_fit_Age_group <- glm(
      formula = cbind(x, n - x) ~ Country * Age_group,
      family = binomial(link = "logit"),
      data = data %>% filter(Age_group != "Total" & Sex == "Total"))
    # Model for Sex, total Age_group, by Country
    prev_fit_Sex <- glm(
      formula = cbind(x, n - x) ~ Country * Sex,
      family = binomial(link = "logit"),
      data = data %>% filter(Age_group == "Total" & Sex != "Total"))
    # Model for total Age_group, total Sex, by Country
    prev_fit <- glm(
      formula = cbind(x, n - x) ~ Country,
      family = binomial(link = "logit"),
      data = data %>% filter(Age_group == "Total" & Sex == "Total"))

    # Get logit-prevalences by lsmeans
    prev_lsmeans_Age_group_Sex <- prev_fit_Age_group_Sex %>% lsmeans(specs = ~ Country * Age_group * Sex)
    prev_lsmeans_Age_group     <- prev_fit_Age_group     %>% lsmeans(specs = ~ Country * Age_group)
    prev_lsmeans_Sex           <- prev_fit_Sex           %>% lsmeans(specs = ~ Country * Sex)
    prev_lsmeans               <- prev_fit               %>% lsmeans(specs = ~ Country)

    # prev_data contains the prevalances with 95% CI
    prev_data <- bind_rows(
      prev_lsmeans_Age_group_Sex %>% as_tibble(),
      prev_lsmeans_Age_group     %>% as_tibble(),
      prev_lsmeans_Sex           %>% as_tibble(),
      prev_lsmeans               %>% as_tibble()) %>%
      # NA's are group "Total"
      mutate(
        Age_group = Age_group %>% fct_explicit_na("Total"),
        Sex       = Sex       %>% fct_explicit_na("Total")) %>%
      # Make sure Country levels are complete
      mutate(
        Country = factor(Country, levels = data %>% pull(Country) %>% levels())) %>%
      # Complete all combinations and arange records
      complete(Country, Age_group, Sex) %>%
      arrange(Country, Age_group, Sex) %>%
      # Convert to prevalences
      mutate(
        Prevalence = lsmean    %>% plogis() %>% "*"(100) %>% round(digits = 1),
        lower =      asymp.LCL %>% plogis() %>% "*"(100) %>% round(digits = 1),
        upper =      asymp.UCL %>% plogis() %>% "*"(100) %>% round(digits = 1))

    # prev_comp adds the pairwise comparisons
    prev_comp <- list(
      # Original data
      data,
      # Calculated prevalances
      prev_data %>%
        select(Country, Age_group, Sex, Prevalence, lower, upper),
      # Pairwise comparisons Age_group...
      bind_rows(
        # ...conditional on Country and Sex
        prev_lsmeans_Age_group_Sex %>%
          pairs(by = c("Country", "Sex"), name = "Age_group", adjust = NULL) %>%
          as_tibble(),
        # ...conditional on Country (Sex = "Total")
        prev_lsmeans_Age_group %>%
          pairs(by = "Country", name = "Age_group", adjust = NULL) %>%
          as_tibble() %>%
          add_column(Sex = factor("Total", levels = data %>% pull(Sex) %>% levels()))) %>%
        # Rename Age_group to Total, to join with these rows in prev_means
        mutate(Age_group = factor("Total", levels = data %>% pull(Age_group) %>% levels())) %>%
        select(Country, Age_group, Sex, p_value_Age_group = p.value),
      # Pairwise comparisons Sex...
      bind_rows(
        # ...conditional on Country and Age_group
        prev_lsmeans_Age_group_Sex %>%
          pairs(by = c("Country", "Age_group"), name = "Sex", adjust = NULL) %>%
          as_tibble(),
        # ...conditional on Country (Age_group = "Total")
        prev_lsmeans_Sex %>%
          pairs(by = "Country", name = "Sex", adjust = NULL) %>%
          as_tibble() %>%
          add_column(Age_group = factor("Total", levels = data %>% pull(Age_group) %>% levels()))) %>%
        # Rename Sex to Total, to join with these rows in prev_means
        mutate(Sex = factor("Total", levels = data %>% pull(Sex) %>% levels())) %>%
        select(Country, Age_group, Sex, p_value_Sex = p.value)) %>%
      # Join
      reduce(
        .f = left_join)

    # Return prev_comp
    return(prev_comp)
  })
