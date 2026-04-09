# ============================================================
# Assignment 4 – Degrees and Democracies
# Updated analysis: CCPI 2025, EIU Democracy Index 2024,
# ND-GAIN 2023, GDP per capita 2024
# ============================================================

# --- 0. Load packages ---------------------------------------
library(tidyverse)
library(stargazer)

# ============================================================
# 1. LOAD DATA
# ============================================================

# -- 1a. CCPI 2025 -------------------------------------------
ccpi <- read_csv("CCPI_2024.csv") %>%
  rename(country = Country, ccpi = `CCPI Score`) %>%
  mutate(country = str_trim(country)) %>%
  # Remove EU row - not a country
  filter(country != "European Union (27)") %>%
  # Standardise names to match Democracy Index
  mutate(country = case_when(
    country == "Czech Republic"         ~ "Czechia",
    country == "Turkiye"                ~ "Turkey",
    country == "Chinese Taipei"         ~ "Taiwan",
    country == "Republic of Korea"      ~ "South Korea",
    country == "Russian Federation"     ~ "Russia",
    country == "Islamic Republic of Iran" ~ "Iran",
    TRUE                                ~ country
  )) %>%
  mutate(ccpi = as.numeric(ccpi))

# -- 1b. Democracy Index 2024 (includes region column) -------
dem <- read_csv("democracy_index_2024.csv") %>%
  rename(
    country   = Country,
    dem_index = `Democracy Index`,
    region    = `World region according to OWID`
  ) %>%
  mutate(
    country   = str_trim(country),
    dem_index = as.numeric(dem_index),
    # Consolidate Americas into one region to match previous analysis
    region = case_when(
      region == "South America"  ~ "Americas",
      region == "North America"  ~ "Americas",
      TRUE                       ~ region
    )
  ) %>%
  select(country, dem_index, region)

# -- 1c. GDP per capita (World Bank 2024) --------------------
gdp_raw <- read_csv("GDP_2024_WorldBank.csv", skip = 4)

gdp <- gdp_raw %>%
  rename(country_wb = `Country Name`, gdp_pc = `2024`) %>%
  select(country_wb, gdp_pc) %>%
  mutate(gdp_pc = as.numeric(gdp_pc)) %>%
  mutate(country = case_when(
    country_wb == "Viet Nam"             ~ "Vietnam",
    country_wb == "Egypt, Arab Rep."     ~ "Egypt",
    country_wb == "Czechia"              ~ "Czechia",
    country_wb == "Turkiye"              ~ "Turkey",
    country_wb == "Korea, Rep."          ~ "South Korea",
    country_wb == "Iran, Islamic Rep."   ~ "Iran",
    country_wb == "Russian Federation"   ~ "Russia",
    country_wb == "United Arab Emirates" ~ "United Arab Emirates",
    country_wb == "United Kingdom"       ~ "United Kingdom",
    country_wb == "United States"        ~ "United States",
    TRUE                                 ~ country_wb
  )) %>%
  select(country, gdp_pc)

# -- 1d. ND-GAIN vulnerability (2023) ------------------------
gain_raw <- read_csv("vulnerability.csv")

gain <- gain_raw %>%
  rename(country_gain = Name) %>%
  select(country_gain, vulnerability = `2023`) %>%
  mutate(vulnerability = as.numeric(vulnerability)) %>%
  mutate(country = case_when(
    country_gain == "Taiwan"                   ~ "Taiwan",
    country_gain == "South Korea"              ~ "South Korea",
    country_gain == "Russia"                   ~ "Russia",
    country_gain == "Iran"                     ~ "Iran",
    country_gain == "United States of America" ~ "United States",
    country_gain == "United Kingdom"           ~ "United Kingdom",
    TRUE                                       ~ country_gain
  )) %>%
  select(country, vulnerability)

# ============================================================
# 2. MERGE ALL DATASETS
# ============================================================

df <- ccpi %>%
  left_join(dem,  by = "country") %>%
  left_join(gdp,  by = "country") %>%
  left_join(gain, by = "country")

# Check merge coverage
cat("=== Merge summary ===\n")
cat("Total CCPI countries:  ", nrow(df), "\n")
cat("Missing dem_index:     ", sum(is.na(df$dem_index)), "\n")
cat("Missing GDP:           ", sum(is.na(df$gdp_pc)), "\n")
cat("Missing vulnerability: ", sum(is.na(df$vulnerability)), "\n")
cat("Missing region:        ", sum(is.na(df$region)), "\n")

cat("\nCountries missing dem_index:\n");  print(df$country[is.na(df$dem_index)])
cat("\nCountries missing GDP:\n");        print(df$country[is.na(df$gdp_pc)])
cat("\nCountries missing vulnerability:\n"); print(df$country[is.na(df$vulnerability)])
cat("\nCountries missing region:\n");     print(df$country[is.na(df$region)])


# Fix GDP for Slovakia and Taiwan
gdp <- gdp_raw %>%
  rename(country_wb = `Country Name`, gdp_pc = `2024`) %>%
  select(country_wb, gdp_pc) %>%
  mutate(gdp_pc = as.numeric(gdp_pc)) %>%
  mutate(country = case_when(
    country_wb == "Viet Nam"             ~ "Vietnam",
    country_wb == "Egypt, Arab Rep."     ~ "Egypt",
    country_wb == "Czechia"              ~ "Czechia",
    country_wb == "Turkiye"              ~ "Turkey",
    country_wb == "Korea, Rep."          ~ "South Korea",
    country_wb == "Iran, Islamic Rep."   ~ "Iran",
    country_wb == "Russian Federation"   ~ "Russia",
    country_wb == "Slovak Republic"      ~ "Slovakia",
    country_wb == "Taiwan, Province of China" ~ "Taiwan",
    country_wb == "United Arab Emirates" ~ "United Arab Emirates",
    country_wb == "United Kingdom"       ~ "United Kingdom",
    country_wb == "United States"        ~ "United States",
    TRUE                                 ~ country_wb
  )) %>%
  select(country, gdp_pc)

# Fix vulnerability for the 6 missing countries
gain <- gain_raw %>%
  rename(country_gain = Name) %>%
  select(country_gain, vulnerability = `2023`) %>%
  mutate(vulnerability = as.numeric(vulnerability)) %>%
  mutate(country = case_when(
    country_gain == "Viet Nam"                 ~ "Vietnam",
    country_gain == "Czech Republic"           ~ "Czechia",
    country_gain == "Taiwan"                   ~ "Taiwan",
    country_gain == "Republic of Korea"        ~ "South Korea",
    country_gain == "Russian Federation"       ~ "Russia",
    country_gain == "Iran (Islamic Republic)"  ~ "Iran",
    country_gain == "United States of America" ~ "United States",
    country_gain == "United Kingdom"           ~ "United Kingdom",
    TRUE                                       ~ country_gain
  )) %>%
  select(country, vulnerability)

# Fix GDP
gdp <- gdp_raw %>%
  rename(country_wb = `Country Name`, gdp_pc = `2024`) %>%
  select(country_wb, gdp_pc) %>%
  mutate(gdp_pc = as.numeric(gdp_pc)) %>%
  mutate(country = case_when(
    country_wb == "Viet Nam"             ~ "Vietnam",
    country_wb == "Egypt, Arab Rep."     ~ "Egypt",
    country_wb == "Czechia"              ~ "Czechia",
    country_wb == "Turkiye"              ~ "Turkey",
    country_wb == "Korea, Rep."          ~ "South Korea",
    country_wb == "Iran, Islamic Rep."   ~ "Iran",
    country_wb == "Russian Federation"   ~ "Russia",
    country_wb == "Slovak Republic"      ~ "Slovakia",
    country_wb == "United Arab Emirates" ~ "United Arab Emirates",
    country_wb == "United Kingdom"       ~ "United Kingdom",
    country_wb == "United States"        ~ "United States",
    TRUE                                 ~ country_wb
  )) %>%
  select(country, gdp_pc)

# Fix vulnerability
gain <- gain_raw %>%
  rename(country_gain = Name) %>%
  select(country_gain, vulnerability = `2023`) %>%
  mutate(vulnerability = as.numeric(vulnerability)) %>%
  mutate(country = case_when(
    country_gain == "Viet Nam"                    ~ "Vietnam",
    country_gain == "Czech Republic"              ~ "Czechia",
    country_gain == "Korea, Republic of"          ~ "South Korea",
    country_gain == "Iran, Islamic Republic of"   ~ "Iran",
    country_gain == "United States of America"    ~ "United States",
    country_gain == "United Kingdom"              ~ "United Kingdom",
    TRUE                                          ~ country_gain
  )) %>%
  select(country, vulnerability)


df <- ccpi %>%
  left_join(dem,  by = "country") %>%
  left_join(gdp,  by = "country") %>%
  left_join(gain, by = "country")

cat("Missing GDP:           ", sum(is.na(df$gdp_pc)), "\n")
cat("Missing vulnerability: ", sum(is.na(df$vulnerability)), "\n")
print(df$country[is.na(df$gdp_pc)])
print(df$country[is.na(df$vulnerability)])


# Drop rows with any missing values for regression
df_clean <- df %>% drop_na(ccpi, dem_index, gdp_pc, vulnerability, region)
cat("\nFinal N for multivariate models:", nrow(df_clean), "\n")

# Set Africa as reference region
df_clean <- df_clean %>%
  mutate(region = relevel(factor(region), ref = "Africa"))

# ============================================================
# 3. REGRESSION MODELS
# ============================================================

m1 <- lm(ccpi ~ dem_index, data = df_clean)
m2 <- lm(ccpi ~ dem_index + gdp_pc + vulnerability, data = df_clean)
m3 <- lm(ccpi ~ dem_index + gdp_pc + vulnerability + region, data = df_clean)

cat("\n=== Model 1: Bivariate ===\n");               print(summary(m1))
cat("\n=== Model 2: Multivariate (no region) ===\n"); print(summary(m2))
cat("\n=== Model 3: Multivariate (with region) ===\n"); print(summary(m3))

# ============================================================
# 4. REGRESSION TABLE (stargazer)
# ============================================================

stargazer(m1, m2, m3,
  type             = "text",
  title            = "Democracy and Climate Performance",
  dep.var.labels   = "CCPI Score (0-100)",
  covariate.labels = c("Democracy Index", "GDP per capita",
                       "Vulnerability (ND-GAIN)",
                       "Americas", "Asia", "Europe", "Oceania"),
  omit.stat        = c("ser", "f"),
  out              = "regression_table.txt"
)

browseURL("model_summary.html")
getwd()

# ============================================================
# 5. SCATTERPLOT
# ============================================================

##------all country labels-----

install.packages("ggrepel")
library(ggrepel)

ggplot(df_clean, aes(x = dem_index, y = ccpi)) +
  geom_point(color = "#2E86AB", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#E84855", se = TRUE, linewidth = 1) +
  geom_text_repel(
    aes(label = country),
    size = 2.5,
    fontface = "italic",
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2
  ) +
  labs(
    title    = "Democracy and Climate Performance",
    subtitle = paste0("CCPI 2025 vs. EIU Democracy Index 2024 (N = ", nrow(df_clean), ")"),
    x        = "Democracy Index (0–10)",
    y        = "CCPI Score (0–100)",
    caption  = "Sources: CCPI 2025; EIU Democracy Index 2024; GDP per capita 2024; ND-GAIN 2023"
  ) +
  annotate("text", x = 0.5, y = 90,
           label = paste0("β = ", round(coef(m1)[2], 2),
                          ", r = ", round(cor(df_clean$dem_index, df_clean$ccpi), 2),
                          ", R² = ", round(summary(m1)$r.squared, 2),
                          ", p = ", round(summary(m1)$coefficients[2, 4], 4)),
           hjust = 0, size = 3.5, color = "#E84855"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("scatterplot_ccpi_democracy.png", width = 10, height = 8, dpi = 300)


##------most country labels-----

ggplot(df_clean, aes(x = dem_index, y = ccpi)) +
  geom_point(color = "#2E86AB", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#E84855", se = TRUE, linewidth = 1) +
  geom_text(
    data = df_clean %>% filter(country %in% c(
      # Top democratic performers
      "Denmark", "Norway", "Netherlands", "United Kingdom",
      # Democratic underperformers
      "United States", "Canada", "Australia",
      # Autocratic outliers (high CCPI despite low democracy)
      "Morocco", "Pakistan", "Vietnam", "India",
      # Autocratic underperformers
      "Saudi Arabia", "Russia", "Iran", "China",
      # Middle-ground interesting cases
      "Germany", "Brazil", "South Africa"
    )),
    aes(label = country),
    nudge_y = 1.5, size = 2.8, fontface = "italic"
  ) +
  labs(
    title    = "Democracy and Climate Performance",
    subtitle = paste0("CCPI 2025 vs. EIU Democracy Index 2024 (N = ", nrow(df_clean), ")"),
    x        = "Democracy Index (0–10)",
    y        = "CCPI Score (0–100)",
    caption  = "Sources: CCPI 2025; EIU Democracy Index 2024; GDP per capita 2024; ND-GAIN 2023"
  ) +
  annotate("text", x = 0.5, y = 90,
           label = paste0("β = ", round(coef(m1)[2], 2),
                          ", r = ", round(cor(df_clean$dem_index, df_clean$ccpi), 2),
                          ", R² = ", round(summary(m1)$r.squared, 2),
                          ", p = ", round(summary(m1)$coefficients[2, 4], 4)),
           hjust = 0, size = 3.5, color = "#E84855"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("scatterplot_ccpi_democracy.png", width = 8, height = 6, dpi = 300)



##------main 6 country labels-----


ggplot(df_clean, aes(x = dem_index, y = ccpi)) +
  geom_point(color = "#2E86AB", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#E84855", se = TRUE, linewidth = 1) +
  geom_text(
    data = df_clean %>% filter(country %in% c(
      "Denmark", "Morocco", "United States", "China",
      "Norway", "Russia", "Saudi Arabia", "Pakistan"
    )),
    aes(label = country),
    nudge_y = 1.5, size = 3, fontface = "italic"
  ) +
  labs(
    title    = "Democracy and Climate Performance",
    subtitle = paste0("CCPI 2025 vs. EIU Democracy Index 2024 (N = ", nrow(df_clean), ")"),
    x        = "Democracy Index (0–10)",
    y        = "CCPI Score (0–100)",
    caption  = "Sources: CCPI 2025; EIU Democracy Index 2024; GDP per capita 2024; ND-GAIN 2023"
  ) +
  annotate("text", x = 0.5, y = 90,
           label = paste0("β = ", round(coef(m1)[2], 2),
                          ", r = ", round(cor(df_clean$dem_index, df_clean$ccpi), 2),
                          ", R² = ", round(summary(m1)$r.squared, 2),
                          ", p = ", round(summary(m1)$coefficients[2, 4], 4)),
           hjust = 0, size = 3.5, color = "#E84855"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("scatterplot_ccpi_democracy.png", width = 8, height = 6, dpi = 300)
cat("\nDone! Files saved: regression_table.txt and scatterplot_ccpi_democracy.png\n")

