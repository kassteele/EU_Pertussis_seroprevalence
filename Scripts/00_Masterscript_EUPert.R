#
# Masterscript EU Pertussis seroprevalence
#
# Circulation of pertussis and poor protection against diphtheria among middle-aged adults in 18 European countries
# Berbers et al., 2021.
# Nature Communications
#

#
# Initialization
#

# This sets up R packages and helper functions
source(file = "Scripts/01_Inititalize.R")

#
# Prepare data
#

# Import data
# This creates antigen datasets by country
source(file = "Scripts/02_Import_data.R")

# Clean data
# This creates eupert_data, a join of all antigen datasets
source(file = "Scripts/03_Clean_data.R")

# Complete data
# This creates eupert_complete_data, a completed version of eupert_data
# This is a dataset where a "Total" group in Age_group and Sex is created
source(file = "Scripts/04_Create_complete_data.R")

#
# Raw data
#

# Visualize RCDC of PT
# This creates fig_RCDC_PT
source(file = "Scripts/05_Visualize_RCDC_PT.R")

#
# GMCs
#

# Create GMC table
# This creates eupert_gmc_data
# It contains the GMCs, their corresponding 95% CIs and mutual comparisons
source(file = "Scripts/06_Compare_GMCs.R")

# Visualize eupert_gmc_data PT
# This creates fig_GMC_PT
source(file = "Scripts/07_Visualize_GMCs_PT.R")

#
# Seroprevalences
#

# Set-up prevalence data
# This creates eupert_prev_data
# At this stage, it only contains numerator / denominator counts
source(file = "Scripts/08_Create_prevalence_data.R")

# Add prevalances and mutual comparisons
# It contains the prevalences, their corresponding 95% CIs and mutual comparisons
# Prevalences are calculated by:
# - Binomial generalized linear modelling (All PT, except PT >= 100 and TT)
# - Exact Binomial methods (PT >= 100 and TT, PT >= 100 by Monte Carlo simulation)
source(file = "Scripts/09a_Compare_prevalences_PT_Dt.R")
source(file = "Scripts/09b_Compare_prevalences_PT100.R")
source(file = "Scripts/09c_Compare_prevalences_TT.R")
# Print
print(eupert_prev_data)

# Visualize eupert_prev_data
# This creates fig_prev
source(file = "Scripts/10_Visualize_prevalences.R")

# Test overall effect of Country, Age_group and Sex on seroprevalence
# This creates table_overall_effects
source(file = "Scripts/11_Test_overall_effects.R")

#
# Export tables and figures
#

source(file = "Scripts/12_Export_tables.R")
source(file = "Scripts/13_Export_figures.R")

#
# Extra analyses
#

# Sero incidence calculations PT
source(file = "Scripts/14_Seroincidence_PT.R")
