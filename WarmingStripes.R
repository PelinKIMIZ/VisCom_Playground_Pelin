warming_stripe_colors <- c(
  "#67000d",
  "#a50f15",
  "#cb181d",
  "#ef3b2c",
  "#fb6a4a",
  "#fc9272",
  "#fcbba1",
  "#fee0d2",
  "#FFFFFF",
  "#deebf7",
  "#c6dbef",
  "#9ecae1",
  "#6baed6",
  "#4292c6",
  "#2171b5",
  "#08519c",
  "#08306b"
)
temperature <- read_csv(
  "data/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv"
)
temperature |>
  ggplot(aes(x = Time, y = `Anomaly (deg C)`, color = `Anomaly (deg C)`)) +
  geom_point(size = 5) +
  scale_color_gradientn(colors = rev(warming_stripe_colors)) +
  theme_minimal() +
  theme(legend.position = "none")
temperature |>
  ggplot(aes(x = Time, y = 1, fill = `Anomaly (deg C)`)) +
  geom_tile() +
  scale_fill_gradientn(colors = (warming_stripe_colors)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none")
