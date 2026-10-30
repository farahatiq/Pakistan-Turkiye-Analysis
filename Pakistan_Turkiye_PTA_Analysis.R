
# =====================================================
# PAKISTAN–TÜRKIYE PTA: COMPARATIVE TARIFF ANALYSIS
# =====================================================

# 1. Install and load packages
packages <- c("readxl", "dplyr", "tidyr", "writexl")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# 2. Import Data.xlsx
data <- read_excel(file.choose())

# 3. Rename columns
names(data) <- c(
  "HS6", "Description",
  "Pakistan", "Egypt", "Morocco",
  "Uzbekistan", "Azerbaijan",
  "Exports_Turkey", "Exports_World"
)

# 4. Clean data
data <- data %>%
  mutate(
    HS6 = sprintf("%06d", as.integer(HS6)),
    Chapter = substr(HS6, 1, 2)
  )

numeric_cols <- c(
  "Pakistan", "Egypt", "Morocco",
  "Uzbekistan", "Azerbaijan",
  "Exports_Turkey", "Exports_World"
)

data[numeric_cols] <- lapply(
  data[numeric_cols],
  function(x) suppressWarnings(as.numeric(x))
)

countries <- c(
  "Pakistan", "Egypt", "Morocco",
  "Uzbekistan", "Azerbaijan"
)

comparators <- countries[-1]

# 5. Overall tariff analysis
tariff_summary <- bind_rows(
  lapply(countries, function(country) {
    
    tariff <- data[[country]]
    valid <- tariff[!is.na(tariff)]
    
    data.frame(
      Country = country,
      Agreement = ifelse(
        country %in% c("Egypt", "Morocco"),
        "FTA", "PTA"
      ),
      Total_HS6 = nrow(data),
      Available_HS6 = length(valid),
      Missing_HS6 = sum(is.na(tariff)),
      Average_Tariff = round(mean(valid), 2),
      Median_Tariff = round(median(valid), 2),
      Duty_Free_Lines = sum(valid == 0),
      Duty_Free_Share = round(
        mean(valid == 0) * 100, 2
      ),
      Positive_Tariff_Lines = sum(valid > 0)
    )
  })
)

# 6. Weighted mean function
wmean <- function(rate, weight) {
  
  valid <- !is.na(rate) &
    !is.na(weight) &
    weight >= 0
  
  if (!any(valid) || sum(weight[valid]) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(rate[valid], weight[valid])
}

# 7. Export-weighted tariff rates
weighted_tariffs <- bind_rows(
  lapply(countries, function(country) {
    
    df <- data %>%
      filter(!is.na(.data[[country]]))
    
    data.frame(
      Country = country,
      
      Average_Tariff = round(
        mean(df[[country]]), 2
      ),
      
      Weighted_Turkey = round(
        wmean(df[[country]], df$Exports_Turkey), 2
      ),
      
      Weighted_World = round(
        wmean(df[[country]], df$Exports_World), 2
      ),
      
      Duty_Free_Turkey = round(
        100 * wmean(
          as.numeric(df[[country]] == 0),
          df$Exports_Turkey
        ), 2
      ),
      
      Duty_Free_World = round(
        100 * wmean(
          as.numeric(df[[country]] == 0),
          df$Exports_World
        ), 2
      )
    )
  })
)

# 8. Bilateral tariff disadvantage analysis
compare_tariffs <- function(country) {
  
  df <- data %>%
    filter(
      !is.na(Pakistan),
      !is.na(.data[[country]])
    ) %>%
    mutate(
      Gap = Pakistan - .data[[country]]
    )
  
  tr <- df %>%
    filter(
      !is.na(Exports_Turkey),
      Exports_Turkey >= 0
    )
  
  world <- df %>%
    filter(
      !is.na(Exports_World),
      Exports_World >= 0
    )
  
  gap_turkey <- wmean(
    tr$Gap, tr$Exports_Turkey
  )
  
  gap_world <- wmean(
    world$Gap, world$Exports_World
  )
  
  higher_share <- wmean(
    as.numeric(tr$Gap > 0),
    tr$Exports_Turkey
  ) * 100
  
  data.frame(
    Comparator = country,
    Agreement = ifelse(
      country %in% c("Egypt", "Morocco"),
      "FTA", "PTA"
    ),
    Comparable_HS6 = nrow(df),
    Pakistan_Higher = sum(df$Gap > 0),
    Pakistan_Lower = sum(df$Gap < 0),
    Equal_Tariffs = sum(df$Gap == 0),
    
    Gap_Turkey_pp = round(gap_turkey, 2),
    Gap_World_pp = round(gap_world, 2),
    Higher_Export_Share = round(higher_share, 2)
  )
}

comparison <- bind_rows(
  lapply(comparators, compare_tariffs)
)

# 9. Sector-wise average tariffs (HS-2)
sector_analysis <- data %>%
  group_by(Chapter) %>%
  summarise(
    across(
      all_of(countries),
      ~ if (all(is.na(.x))) NA_real_
      else round(mean(.x, na.rm = TRUE), 2)
    ),
    HS6_Lines = n(),
    .groups = "drop"
  ) %>%
  arrange(Chapter)

# 10. HS-6 product-level tariff comparison
product_comparison <- data %>%
  mutate(
    Gap_Egypt = Pakistan - Egypt,
    Gap_Morocco = Pakistan - Morocco,
    Gap_Uzbekistan = Pakistan - Uzbekistan,
    Gap_Azerbaijan = Pakistan - Azerbaijan
  )

# 11. Export products facing higher tariffs
export_disadvantage <- product_comparison %>%
  filter(
    !is.na(Exports_Turkey),
    Exports_Turkey > 0
  ) %>%
  arrange(desc(Exports_Turkey))

# 12. Potential export products
# Products exported globally but not currently to Türkiye
export_potential <- product_comparison %>%
  filter(
    !is.na(Exports_World),
    Exports_World > 0,
    !is.na(Exports_Turkey),
    Exports_Turkey == 0
  ) %>%
  arrange(desc(Exports_World))

# 13. Display results
print(tariff_summary)
print(weighted_tariffs)
print(comparison)
print(sector_analysis)

# 14. Export all tables to Excel
write_xlsx(
  list(
    "Overall_Tariffs" = tariff_summary,
    "Weighted_Tariffs" = weighted_tariffs,
    "Tariff_Disadvantage" = comparison,
    "Sector_Analysis" = sector_analysis,
    "HS6_Comparison" = product_comparison,
    "Existing_Exports" = export_disadvantage,
    "Potential_Exports" = export_potential
  ),
  "Pakistan_Turkiye_PTA_Analysis.xlsx"
)

# 15. View results in RStudio
View(tariff_summary)
View(weighted_tariffs)
View(comparison)
View(sector_analysis)

# Show saved file location
getwd()
