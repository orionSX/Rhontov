raw_data <- read_excel("D:/GitHub/Rhontov/dataset_lab7/DATA.xlsx")

library(tidyr)
library(dplyr)
# Filter for Serbia and pivot years
serbia_clean <- raw_data %>%
  select(-`Country Code`, -`Indicator Code`) %>%  # Remove unnecessary columns
  pivot_longer(
    cols = -c(`Country Name`, `Indicator Name`),  # All columns except these
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(year)) %>%  # Convert year to numeric
  filter(!is.na(value))  # Remove NA values

# Reshape to have indicators as columns
serbia_wide <- serbia_clean %>%
  pivot_wider(
    names_from = `Indicator Name`,
    values_from = value
  ) %>%
  select(-`Country Name`) %>%  # Now redundant
  arrange(year)  # Sort chronologically

library(ggplot2)

ggplot(serbia_wide, aes(x = year, y = 'GDP growth (annual %)')) +
  geom_line(color = "#005BB6", linewidth = 1) +  # Serbian flag blue
  geom_point(color = "#E4203A") +  # Serbian flag red
  labs(
    title = "Serbia: GDP Growth Trend ",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  theme_minimal()