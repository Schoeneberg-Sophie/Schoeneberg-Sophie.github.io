library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

us_flight_data <- read_csv("us_flight_data.csv")
us_weather_data <- read_csv("us_weather_data.csv")

# filter for date (February 2026)
jfk_weather <- us_weather_data %>%
  filter(station_id == "USW00094789", date >= 20260201, date <= 20260228) %>%
  mutate(DAY_OF_MONTH = date %% 100)

feb_flights <- us_flight_data %>%
  filter(YEAR == 2026, MONTH == 2)

weather_wide <- jfk_weather %>%
  pivot_wider(id_cols = DAY_OF_MONTH, names_from = element, values_from = value)

# daily mean departure delays from JFK
# I aggregate by day to avoid copies of the same data entries, though the 
# trade-off here is sample size (N=28 for every day)
# analysis for 6-months and multiple airports confirm the correlation results
jfk_dep <- feb_flights %>%
  filter(ORIGIN == "JFK") %>%
  group_by(DAY_OF_MONTH) %>%
  # here the median is used to avoid distortion of data due to outliers (think a
  # single delay of 3 hours)
  summarise(mean_dep_delay = median(DEP_DELAY, na.rm = TRUE))

# daily mean arrival delays to JFK
jfk_arr <- feb_flights %>%
  filter(DEST == "JFK") %>%
  group_by(DAY_OF_MONTH) %>%
  summarise(mean_arr_delay = median(ARR_DELAY, na.rm = TRUE))

jfk_analysis <- weather_wide %>%
  inner_join(jfk_dep, by = "DAY_OF_MONTH") %>%
  inner_join(jfk_arr, by = "DAY_OF_MONTH")

# cumulative indicators
jfk_interactions <- jfk_analysis %>%
  mutate(
    # Wind + Rain (Combined wet and windy conditions)
    wind_plus_rain = AWND + PRCP,
    
    # Rain + TMin (Wet and cold conditions)
    rain_plus_tmin = PRCP + TMIN,
    
    # Snow + TMin (Deep winter / sub-zero snowy conditions)
    snow_plus_tmin = SNOW + TMIN,
    
    # Multi-hazard composite: Rain + Snow + TMin (Severe winter storm index)
    rain_snow_tmin = PRCP + SNOW + TMIN
  )

# narrow down to just the variables going into the correlation matrix
selected_vars_flight <- jfk_interactions %>%
  select(any_of(c("mean_dep_delay", "mean_arr_delay", "PRCP", "SNOW", "TMIN", "TMAX", "AWND", 
                  "wind_plus_rain", "rain_plus_tmin", "snow_plus_tmin", "rain_snow_tmin")))

# calculation of Pearson's r across all variables (weather-vs-weather,
# delay-vs-delay, and weather-vs-delay all included)
comprehensive_cor_matrix <- cor(selected_vars_flight, use = "pairwise.complete.obs")

# keep only the upper triangle + diagonal so each pair (e.g. PRCP vs SNOW)
# appears once instead of twice (mirrored) in the melted/plotted output
cor_matrix_triangle <- comprehensive_cor_matrix
cor_matrix_triangle[lower.tri(cor_matrix_triangle)] <- NA

cor_melted_full <- melt(cor_matrix_triangle, na.rm = TRUE)
names(cor_melted_full) <- c("Var1", "Var2", "value")

print(cor_melted_full)