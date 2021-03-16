#
# Complete eupert_data
#

# The "Total" group in Age_group and Sex has not been filled for countries that report data
# for only Age_group and Sex, .e.g. AT, BE and HU
#
# In order to "add" the Total group for these countries, we apply the following procedure:
# 1. Duplicate Age_group 40-49 and 50-59
# 2. Duplicate Sex Males and Females
# 3. Rename both duplicated groups to Age_group "Total" and Sex "Total"
#    These records are now the Total group
#
# This allows us to easily summmarize (aggregate) the records into all groups, including Total, in the next step
#
# >>> Be careful!!!
# Do NOT perform statistical analyses by Age_group and Sex
# on this complete data without filtering out the Total groups!

# Before table
with(eupert_data, table(Age_group, Sex, Country))[, , c(1, 2, 7, 6)] %>% print()

# You can see zeros in the "Total" group in Age_group and Sex
# We are going to duplicate the records, but relabel 40-49 and 50-59 in Age_group by Total,
#   and relabel Males and Females Sex by Total
eupert_complete_data <- bind_rows(
  # Original data
  eupert_data,
  # Without Total for Age_group
  eupert_data %>%
    filter(Age_group != "Total") %>%
    mutate(Age_group  = "Total" %>% factor(levels = c("40-49", "50-59", "Total"))),
  # Without Total for Sex
  eupert_data %>%
    filter(Sex != "Total") %>%
    mutate(Sex  = "Total" %>% factor(levels = c("Males", "Females", "Total"))),
  # Without Total for Age_group and Sex
  eupert_data %>%
    filter(Age_group != "Total" & Sex != "Total") %>%
    mutate(
      Age_group = "Total" %>% factor(levels = c("40-49", "50-59", "Total")),
      Sex       = "Total" %>% factor(levels = c("Males", "Females", "Total"))))

# After table
# You see that the zeroes in the margins are gone, and have been replaced by their totals
with(eupert_complete_data, table(Age_group, Sex, Country))[, , c(1, 2, 7, 6)] %>% print()

# Print
print(eupert_complete_data)
