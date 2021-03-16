#
# Create aggregated data
#

# Step 1: aggregate eupert_complete_data for all groups
# We do this for all combinations of Country (18) x Age group (3) x Sex (3) = 162 records
# These contain numerator / denominator counts
eupert_prev_data <- eupert_complete_data %>%
  group_by(
    Country, Age_group, Sex, .drop = FALSE) %>%
  summarize(
    # Numerators
    `PT≥100`    = sum(`PT≥100`),
    `50≥PT<100` = sum(`50≥PT<100`),
    `PT≥50`     = sum(`PT≥50`),
    `Dt<0.01`   = sum(`Dt<0.01`),
    `Dt<0.1`    = sum(`Dt<0.1`),
    `TT<0.01`   = sum(`TT<0.01`),
    `TT<0.1`    = sum(`TT<0.1`),
    # Denominator is just the number of records
    n = n()) %>%
  ungroup()

# Step 2: reshape into long format and split by COP (7)
# This results in 7 x 162 records
eupert_prev_data <- eupert_prev_data %>%
  pivot_longer(
    cols = `PT≥100`:`TT<0.1`,
    names_to = "COP",
    values_to = "x") %>%
  relocate(COP, .after = "Sex") %>%
  # Split by COP for COP-specific calculations
  split(f = .$COP)

# Print
print(eupert_prev_data)
