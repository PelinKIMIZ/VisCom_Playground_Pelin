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

# Own Work

df |> summary()

df |> 
  filter(year >= 2010) |> 
  group_by(year) |> 
  summarise(
    n_countries = n(),
    obesity_valid = sum(!is.na(obesity_prevalence_pct)),
    voice_valid = sum(!is.na(voice_accountability_index)),
    gdp_valid = sum(!is.na(gdp_per_capita_usd))
  )

# Research Question: Does Democratic Voice & Accountability Extend Human Life?

## Filter for year 2019 and strictly drop NAs and regional aggregates
plot_data <- df |>
  filter(year == 2019) |>
  filter(
    !is.na(voice_accountability_index),
    !is.na(life_expectancy_years),
    !is.na(total_population),
    total_population > 0,
    !is.na(region)
  )

## Select key countries to annotate
highlight_countries <- c("Norway", "Japan", "United States", "China", "Turkey", "Nigeria")

labels_data <- plot_data |>
  filter(country_name %in% highlight_countries)

## Create bubble plot
p <- ggplot(plot_data, aes(
  x = voice_accountability_index,
  y = life_expectancy_years,
  size = total_population,
  color = region
)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 14, guide = "none") +
  
  ## Add country labels
  geom_text(
    data = labels_data,
    aes(label = country_name),
    color = "black",
    size = 3.2,
    nudge_y = 1.3,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  
  ## Labels and captions
  labs(
    title = "Voice, Freedom, and Longevity: Does Democracy Extend Life?",
    subtitle = "Voice & Accountability Index vs. Life Expectancy across Regions (2019)",
    x = "Voice and Accountability Index (-2.5 = Authoritarian, +2.5 = Democratic)",
    y = "Life Expectancy at Birth (Years)",
    color = "Region",
    caption = "Visualization: Pelin Kimiz | Data: World Bank & WHO"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

## Render plot
p

## Save output figure
ggsave("democracy_vs_life_expectancy.png", plot = p, width = 9, height = 6, dpi = 300)


# Research Question: The Resource Curse: Does Fossil Fuel Reliance Lead to Higher Unemployment?

## Filter for year 2019, drop missing values, and remove duplicate country entries
plot_data <- df |>
  filter(year == 2019) |>
  filter(
    !is.na(fuel_exports_pct_merch_exports),
    !is.na(unemployment_rate_pct),
    !is.na(gdp_total_usd),
    gdp_total_usd > 0,
    !is.na(income_group)
  ) |>
  distinct(country_name, .keep_all = TRUE)

## Strategic country sample with customized offsets to prevent overlapping
labels_data <- tibble(
  country_name = c("Norway", "Kuwait", "Nigeria", "Algeria", "Germany", "United States"),
  offset_y     = c(1.3,     -1.5,      1.8,       1.6,       1.2,      -1.8)
) |>
  inner_join(plot_data, by = "country_name")

## Construct bubble plot
p <- ggplot(plot_data, aes(
  x = fuel_exports_pct_merch_exports,
  y = unemployment_rate_pct,
  size = gdp_total_usd,
  color = income_group
)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 14, guide = "none") +
  
  ## Clean single-instance country annotations
  geom_text(
    data = labels_data,
    aes(y = unemployment_rate_pct + offset_y, label = country_name),
    color = "grey15",
    fontface = "bold",
    size = 3.3,
    show.legend = FALSE
  ) +
  
  ## Titles, axis labels, and caption attribution
  labs(
    title = "The Resource Curse: Does Oil Wealth Solve Unemployment?",
    subtitle = "Fuel Exports (% of merchandise exports) vs. Unemployment Rate (2019)",
    x = "Fuel Exports (% of Merchandise Exports)",
    y = "Total Unemployment Rate (% of Labor Force)",
    color = "Income Group",
    caption = "Visualization: Pelin Kimiz | Data: World Bank (WDI)"
  ) +
  
  ## Theme layout
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

## Display plot
p

## Save output figure
ggsave(
  filename = "resource_curse_unemployment.png",
  plot = p,
  width = 10,
  height = 6.2,
  dpi = 300
)