#
# Import antigen data by country
#

# The Excelfile consists of 18 tabs, one for each country
# The layout is different for each tab
# Each country is therefore imported separately
#
# Things that are done during and right after import:
# 1. Select only columns that are needed
# 2. Each column is imported as text (for easier data cleaning)
# 3. Fix names: replace " " and "/" by "_"
# 4. Rename column names to make them the same for all countries
# 4. Add ISO Alpha-2 country codes, see https://www.nationsonline.org/oneworld/country_code_list.htm

# Import data
AT_data <- import_fun(sheet =  1, cols = "B:G") %>% mutate(Country = "AT")
SI_data <- import_fun(sheet =  2, cols = "B:G") %>% mutate(Country = "SI")
HU_data <- import_fun(sheet =  3, cols = "B:G") %>% mutate(Country = "HU")
PT_data <- import_fun(sheet =  4, cols = "B:G") %>% mutate(Country = "PT")
LT_data <- import_fun(sheet =  5, cols = "B:G") %>% mutate(Country = "LT")
FI_data <- import_fun(sheet =  6, cols = "D:I") %>% mutate(Country = "FI")
BE_data <- import_fun(sheet =  7, cols = "B:G") %>% mutate(Country = "BE")
IE_data <- import_fun(sheet =  8, cols = "B:H") %>% mutate(Country = "IE") %>%
  rename(Sample_number = Sample__number, Age_group = Age_at_sampling)
SK_data <- import_fun(sheet =  9, cols = "C:I") %>% mutate(Country = "SK")
DK_data <- import_fun(sheet = 10, cols = "B:H") %>% mutate(Country = "DK") %>%
  rename(Sample_number = Tube_number)
NO_data <- import_fun(sheet = 11, cols = "B:F") %>% mutate(Country = "NO")
NL_data <- import_fun(sheet = 12, cols = "B:H") %>% mutate(Country = "NL") %>%
  rename(Sample_number = SampleCode)
RO_data <- import_fun(sheet = 13, cols = "D:J") %>% mutate(Country = "RO") %>%
  rename(Sample_number = Sample_code)
LV_data <- import_fun(sheet = 14, cols = "D:I") %>% mutate(Country = "LV") %>%
  rename(Sample_number = Sample_code)
SE_data <- import_fun(sheet = 15, cols = "B:H") %>% mutate(Country = "SE")
UK_data <- import_fun(sheet = 16, cols = "C:I") %>% mutate(Country = "UK") %>%
  rename(Sample_number = Sample_code)
FR_data <- import_fun(sheet = 17, cols = "B:H") %>% mutate(Country = "FR")
GR_data <- import_fun(sheet = 18, cols = "B:G") %>% mutate(Country = "GR")
