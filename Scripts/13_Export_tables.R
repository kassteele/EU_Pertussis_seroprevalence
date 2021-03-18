#
# Export tables
#

# Table 2
eupert_prev_data %>%
  bind_rows() %>%
  write_delim(
    file = "Results/Tables/EUPert_Table2_prev_data.csv",
    delim = ";",
    na = "")

# Table 3
eupert_gmc_data %>%
  write_delim(
    file = "Results/Tables/EUPert_Table3_GMC_data.csv",
    delim = ";",
    na = "")

# Table S1
table_overall_effects %>%
  write_delim(
    file = "Results/Tables/EUPert_TableS1_overall_effects.csv",
    delim = ";",
    na = "")
