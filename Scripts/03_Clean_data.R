#
# Combine country data and clean contents
#

eupert_data <- bind_rows(
  # 1. Bind rows together. Columns are matched by name
  #    Any missing columns will automatically be filled with NA
  AT_data, BE_data, DK_data, FI_data, FR_data, UK_data, GR_data, HU_data, IE_data,
  LT_data, LV_data, NL_data, NO_data, PT_data, RO_data, SE_data, SI_data, SK_data) %>%
  # Rename
  rename(
    # Rename Gender to Sex
    Sex = Gender,
    # Rename Diph to Dt and Tet to TT
    IgG_Dt_IU_ml = IgG_Diph_IU_ml,
    IgG_TT_IU_ml = IgG_Tet_IU_ml) %>%
  # 3. Reorder columns
  select(
    Country, Sample_number, Age_group, Sex,
    IgG_PT_IU_ml, IgG_Dt_IU_ml, IgG_TT_IU_ml) %>%
  # 4. Clean data
  mutate(
    # Make Country a categorical variable
    Country = Country %>% factor(
      levels = c(
        "AT", "BE", "DK", "FI", "FR", "GR", "HU", "IE", "LV",
        "LT", "NL", "NO", "PT", "RO", "SK", "SI", "SE", "UK")),
    # Remove characters from Age_group, only keep 40-49 and 50-59
    # Replace NA by "Total"
    # Make it a categorical variable
    Age_group = Age_group %>%
      str_remove(pattern = "years") %>%
      str_remove(pattern = "y.o.") %>%
      str_remove(pattern = "A") %>%
      str_remove(pattern = "B") %>%
      str_squish() %>%
      str_remove(pattern = " ") %>%
      str_replace(pattern = "41-49", replacement = "40-49") %>%
      replace_na(replace = "Total") %>%
      factor(levels = c("40-49", "50-59", "Total")),
    # Unify Sex categories into Males and Females
    # No match (= Unknown) gets replaced by Total
    # Make it a categorical variable
    Sex = case_when(
      Sex %in% c("M", "Male",   "male")   ~ "Males",
      Sex %in% c("F", "Female", "female") ~ "Females",
      TRUE ~ "Total") %>%
      factor(
        levels = c("Males", "Females", "Total")),
    # Replace "no serum" and "no sample" with NA in IgG measurements
    # and make it a numerical variable
    IgG_PT_IU_ml = IgG_PT_IU_ml %>% na_if("no serum") %>% na_if("no sample") %>% as.numeric(),
    IgG_Dt_IU_ml = IgG_Dt_IU_ml %>% na_if("no serum") %>% na_if("no sample") %>% as.numeric(),
    IgG_TT_IU_ml = IgG_TT_IU_ml %>% na_if("no serum") %>% na_if("no sample") %>% as.numeric(),
    # Replace censored values with 0.5*LLOQ
    IgG_PT_IU_ml = if_else(IgG_PT_IU_ml == 0.85,  true = 0.5*IgG_PT_IU_ml, false = IgG_PT_IU_ml),
    IgG_Dt_IU_ml = if_else(IgG_Dt_IU_ml == 0.001, true = 0.5*IgG_Dt_IU_ml, false = IgG_Dt_IU_ml),
    IgG_TT_IU_ml = if_else(IgG_TT_IU_ml == 0.001, true = 0.5*IgG_TT_IU_ml, false = IgG_TT_IU_ml),
    # Correlates of protection
    `PT≥100`    = (IgG_PT_IU_ml >= 100)                     %>% as.integer(),
    `50≥PT<100` = (IgG_PT_IU_ml >= 50 & IgG_PT_IU_ml < 100) %>% as.integer(),
    `PT≥50`     = (IgG_PT_IU_ml >= 50)                      %>% as.integer(),
    `Dt<0.01`   = (IgG_Dt_IU_ml <  0.01)                    %>% as.integer(),
    `Dt<0.1`    = (IgG_Dt_IU_ml <  0.1)                     %>% as.integer(),
    `TT<0.01`   = (IgG_TT_IU_ml <  0.01)                    %>% as.integer(),
    `TT<0.1`    = (IgG_TT_IU_ml <  0.1)                     %>% as.integer()) %>%
  # Filter out missing antigens
  # Based on PT, but then applies to the rest as well!
  filter(
    !is.na(IgG_PT_IU_ml))
