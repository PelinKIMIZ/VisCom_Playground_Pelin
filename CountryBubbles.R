# Variable Names in country_panel_wdi_who.csv
#
# See Details on corresponding lecture slides
#
# country_name / country_code / year
# region (categorical)
# income_group (categorical)
# total_population — SP.POP.TOTL
# gdp_total_usd — NY.GDP.MKTP.CD
# gdp_per_capita_usd — NY.GDP.PCAP.CD
# life_expectancy_years — SP.DYN.LE00.IN
# secondary_enrollment_gross_pct — SE.SEC.ENRR
# unemployment_rate_pct — SL.UEM.TOTL.ZS
# inflation_cpi_pct — FP.CPI.TOTL.ZG
# fuel_exports_pct_merch_exports — TX.VAL.FUEL.ZS.UN
# fuel_imports_pct_merch_imports — TM.VAL.FUEL.ZS.UN
# voice_accountability_index — GOV_WGI_VA.EST
# obesity_prevalence_pct — NCD_BMI_30C (WHO GHO)
#
# See data compilation script in data/ with download using WDI and WHO APIs (wrapped in R packages)
# With research on WDI indicators, you can extend the dataset

# Package and Data
library(tidyverse)
df <- read_csv("data/country_panel_wdi_who.csv")

# Hans Rosling's Version (with different regions)
df |>
  filter(year == 2018) |>
  ggplot(aes(
    x = gdp_per_capita_usd,
    y = life_expectancy_years,
    size = total_population,
    color = region
  )) +
  geom_point() +
  scale_x_log10() +
  scale_size_area(max_size = 15) +
  theme_minimal()

# Paulsen's version
df |>
  filter(year == 2018) |>
  ggplot(aes(
    x = gdp_per_capita_usd,
    y = life_expectancy_years,
    size = total_population,
    color = region
  )) +
  geom_point() +
  scale_size_area(max_size = 15) +
  theme_minimal()

# Facetted years
df |>
  filter(year %in% c(1988, 1998, 2018)) |>
  ggplot(aes(
    x = gdp_per_capita_usd,
    y = life_expectancy_years,
    size = total_population,
    color = region
  )) +
  geom_point() +
  scale_x_log10() +
  scale_size_area(max_size = 15) +
  facet_wrap(~year) +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  guides(
    size = "none",
    color = guide_legend(ncol = 2, override.aes = list(size = 4), title = NULL)
  )


