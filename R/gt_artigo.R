library(tidyverse)
library(gt)
library(gtExtras)
library(WDI)

countries <-  c("BR","USA","AR",
                "AT","AU","BG",
                "CL","CH","NO",
                "DE","DK","FR",
                "ES","FI","GB",
                "IE","IN","IT",
                "LU","MX")
gdp_per_cap <-
  WDI(
      country = countries,
      indicator = c("gdp_per_cap" = "NY.GDP.PCAP.KD"),
      start = 1980,
      end = 2020
  ) %>%
  as_tibble()

gdp_data <- gdp_per_cap %>%
  group_by(iso2c) %>%
  arrange(year) %>%
  mutate(gdp_data = list(gdp_per_cap)) %>%
  filter(year %in% c(1980, 2020)) %>%
  pivot_wider(names_from = "year", values_from = "gdp_per_cap") %>%
  mutate(perc_change = (`2020` / `1980` - 1) * 100) %>%
  relocate(gdp_data, .after = everything()) %>%
  relocate(`1980`, .before = `2020`) %>%
  select(-iso3c) %>%
  ungroup() %>%
  arrange(desc(perc_change))



gdp_data %>%
  gt(rowname_col = "iso2c") %>%
  gt_plt_sparkline(
    column = "gdp_data",
    type = "shaded",
    palette = c("#067d51", rep("transparent", 3), "#17bdb4"),
    label = FALSE,
    same_limit = FALSE
  ) %>%
  cols_label(
    country = "**Country**",
    iso2c = "",
    `2020` = "2020",
    `1980` = "1980",
    perc_change = "**% Change**",
    gdp_data = "**Trend**",
    .fn = md
  ) %>%
  fmt_flag(
    columns = "iso2c"
  ) %>%
  cols_merge(
    columns = c("iso2c", "country"),
    pattern = "{1}{2}"
  ) %>%
  fmt_currency(
    columns = c(`1980`,`2020`),
    currency = "USD",
    decimals = 0,
    drop_trailing_dec_mark = TRUE
  ) %>%
  fmt_percent(
    columns = "perc_change",
    scale_values = FALSE
  ) %>%
  data_color(
    columns = "perc_change",
    palette = "rcartocolor::Emrld",
    alpha = 0.75
  ) %>%
  tab_header(
    title = "GDP Per Capita Evolution in 20 countries",
    subtitle = "From 1980 to 2020"
  ) %>%
  tab_source_note("Source: World Development Indicators (WDI)") %>%
  tab_options(
    heading.align = "center",
    heading.border.bottom.color = "transparent",
    data_row.padding = px(5),
    table_body.vlines.color = "transparent",
    column_labels.border.bottom.color = "transparent",
    column_labels.background.color = "#17bdb4",
    column_labels.font.size = px(12),
    table_body.hlines.width = px(0),
    source_notes.font.size = px(8)
  )















