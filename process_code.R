library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(lubridate)
library(patchwork)
library(purrr)
library(arrow)
library(writexl)

#rename the data columns for more convinient analysis
trip_2019 <- read_csv("D:/Learning/data_analyst/Google_data_analytics/Case_study_Cyclistic/Datasets/trip_info_2019_duration_fix.csv")
trip_2019 <- trip_2019 %>% 
  rename(
    bike_id = bikeid,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    user_type = usertype,
    birth = birthyear,
    duration = duration_calc
  )

#filter na value and create demographic dataframe
trip_2019_demo <- trip_2019 %>% 
  filter(!is.na(gender), !is.na(birth)) 

#rename the dataset columns for 2020 data and exclude the ridable_type due to no variance
trip_2020 <- read_csv("D:/Learning/data_analyst/Google_data_analytics/Case_study_Cyclistic/Datasets/trip_info_2020_timestamp_fix.csv")
trip_2020 <- trip_2020 %>% 
  select(-rideable_type) %>% 
  rename(
    trip_id = ride_id,
    start_time = started_at,
    end_time = ended_at,
    user_type = member_casual
  )

add_general_col <- function(df){ 
  df %>% 
    unite( #Add route ID column
      col = route_id,
      start_station_id,
      end_station_id,
      sep = "_",
      remove = FALSE
    ) %>% 
    rename( #add duration in seconds columns
      duration_sec = duration
    ) %>% 
    mutate(
      weekday = wday(start_time, label = TRUE), #Weekday column
      duration_min = as.numeric(difftime(end_time, start_time, units = "min")), #exchange to duration in minute
      start_hour = hour(start_time), #hour where the ride start
      is_weekend = weekday == "Sat" | weekday == "Sun", #validate if the ride happened in weekend
      round_trip = start_station_id == end_station_id #validate if the ride is a round trip
    )
}

df_list <- list(
  trip_2019 <- trip_2019,
  trip_2019_demo = trip_2019_demo,
  trip_2020 = trip_2020
)

df_list <- map(df_list, add_general_col)

trip_2019 <- df_list$trip_2019
trip_2019_demo <- df_list$trip_2019_demo
trip_2020 <- df_list$trip_2020

#add age column
trip_2019 <- trip_2019 %>% 
  mutate(
    user_type = case_when(
      user_type == "Subscriber" ~ "member",
      user_type == "Customer" ~ "casual",
      TRUE ~ NA_character_
    )
  )

trip_2019_demo <- trip_2019_demo %>% 
  mutate(
    age = 2019 - birth,
    user_type = case_when(
      user_type == "Subscriber" ~ "member",
      user_type == "Customer" ~ "casual",
      TRUE ~ NA_character_
    )
  )


#Check for hypothesis 1
p1 <- trip_2019 %>%
  count(user_type, start_hour, weekday) %>%
  ggplot(aes(
    x = start_hour,
    y = n
  )) +
  geom_col()

p2 <- trip_2020 %>%
  count(user_type, start_hour) %>%
  ggplot(aes(
    x = start_hour,
    y = n
  )) +
  geom_col()

p1/p2
 
#Build threshold data frame
threshold <- trip_2019 %>%
  count(start_hour) %>%
  complete(start_hour = 0:23, fill = list(n = 0)) %>%
  arrange(desc(n)) %>% 
  mutate(
    pct_to_max = if_else(
      row_number() == 1,
      100,
      round((n/n[1])*100, 2)
    ), 
    delta_to_above = if_else(
      row_number() == 1,
      0,
      round((1-n/lag(n))*100, 2)
    )
  )

#count for proportion in each time slot compare between two types of riders
#2019 data
print("2019 data")
peak_mem <- trip_2019 %>%
  summarize(n_peak = sum(start_hour >= 7 & start_hour <= 9 & user_type == "member") + sum(start_hour >= 15 & start_hour <= 18 & user_type == "member")) %>% 
  pull(n_peak)
all_mem <- trip_2019 %>% 
  summarize(n_peak = sum(user_type == "member")) %>% 
  pull(n_peak)

pct_mem <- peak_mem/all_mem*100
cat("peak hour member percentage: ",  round(pct_mem, 2), "% \n")

peak_cas <- trip_2019 %>%
  summarize(n_peak = sum(start_hour >= 7 & start_hour <= 9 & user_type == "casual") + sum(start_hour >= 15 & start_hour <= 18 & user_type == "casual")) %>% 
  pull(n_peak)
all_cas <- trip_2019 %>% 
  summarize(n_peak = sum(user_type == "casual")) %>% 
  pull(n_peak)
 
pct_cas <- peak_cas/all_cas*100
cat("peak hour casual percentage: ",  round(pct_cas, 2), "% \n")

#2020 data
print("2020 data")
peak_mem <- trip_2020 %>%
  summarize(n_peak = sum(start_hour >= 7 & start_hour <= 9 & user_type == "member") + sum(start_hour >= 15 & start_hour <= 18 & user_type == "member")) %>% 
  pull(n_peak)
all_mem <- trip_2020 %>% 
  summarize(n_peak = sum(user_type == "member")) %>% 
  pull(n_peak)

pct_mem <- peak_mem/all_mem*100
cat("peak hour member percentage: ",  round(pct_mem, 2), "% \n")

peak_cas <- trip_2020 %>%
  summarize(n_peak = sum(start_hour >= 7 & start_hour <= 9 & user_type == "casual") + sum(start_hour >= 15 & start_hour <= 18 & user_type == "casual")) %>% 
  pull(n_peak)
all_cas <- trip_2020 %>% 
  summarize(n_peak = sum(user_type == "casual")) %>% 
  pull(n_peak)

pct_cas <- peak_cas/all_cas*100
cat("peak hour casual percentage: ",  round(pct_cas, 2), "% \n")

#filter the sample was recorded in peak hour
add_is_peak <- function(df){
  df %>% mutate(
    is_peak = if_else(
      start_hour >= 7 & start_hour <= 9 | start_hour >= 15 & start_hour <= 18, 
      TRUE,
      FALSE
    )
  )
}

#Apply function for multiple data frames
df_list <- list(
  trip_2019 = trip_2019,
  trip_2019_demo = trip_2019_demo,
  trip_2020 = trip_2020
)

df_list <- map(df_list, add_is_peak)

trip_2019 <- df_list$trip_2019
trip_2019_demo <- df_list$trip_2019_demo
trip_2020 <- df_list$trip_2020

#set up the tible for IQR validation
add_iqr <- function(df){
  df %>% 
    group_by(user_type) %>%
    summarise(
      q1 = quantile(duration_sec, 0.25, na.rm = TRUE),
      med = median(duration_sec, na.rm = TRUE),
      q3 = quantile(duration_sec, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr
    )
}

df_list <- list(
  trip_2019 = trip_2019 %>% filter(is_peak == TRUE), 
  trip_2020 = trip_2020 %>% filter(is_peak == TRUE)
)

df_list <- map(df_list, add_iqr)
iqr_peak_stat_2019 <- df_list$trip_2019
iqr_peak_stat_2020 <- df_list$trip_2020

#Define the percentage of outliers
pct_vld_2019 <- trip_2019 %>% 
  filter(is_peak == TRUE) %>% 
  left_join(iqr_peak_stat_2019, by = "user_type") %>% 
  mutate(
    is_outlier = duration_sec < lower | duration_sec > upper
  ) %>% 
  group_by(user_type) %>% 
  summarize(
    total = n(),
    outliers = sum(is_outlier),
    pct = outliers/total*100
  )

pct_vld_2020 <- trip_2020 %>% 
  filter(is_peak == TRUE) %>% 
  left_join(iqr_peak_stat_2020, by = "user_type") %>% 
  mutate(
    is_outlier = duration_sec < lower | duration_sec > upper
  ) %>% 
  group_by(user_type) %>% 
  summarize(
    total = n(),
    outliers = sum(is_outlier),
    pct = outliers/total*100
  )

#Determine where the tails are
#Draw 2019 histogram
trip_2019 %>% 
  filter(is_peak == TRUE) %>% 
  ggplot(aes(x = duration_sec)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  facet_wrap(~user_type)

#Check for the distribution of the tails
trip_2019 %>% 
  filter(is_peak == TRUE) %>% 
  left_join(iqr_peak_stat_2019, by = "user_type") %>% 
  mutate(
    is_outlier_left = duration_sec < lower,
    is_outlier_right = duration_sec > upper
  ) %>% 
  group_by(user_type) %>% 
  summarize(
    total = n(),
    outlier_left = sum(is_outlier_left),
    outlier_right = sum(is_outlier_right),
    pct_left = outlier_left/total*100,
    pct_right = outlier_right/total*100
  )

#Draw 2020 histogram
trip_2020 %>% 
  filter(is_peak == TRUE) %>% 
  ggplot(aes(x = duration_sec)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  facet_wrap(~user_type)

#Check for distribution of the tails
trip_2020 %>% 
  filter(is_peak == TRUE) %>% 
  left_join(iqr_peak_stat_2020, by = "user_type") %>% 
  mutate(
    is_outlier_left = duration_sec < lower,
    is_outlier_right = duration_sec > upper
  ) %>% 
  group_by(user_type) %>% 
  summarize(
    total = n(),
    outlier_left = sum(is_outlier_left),
    outlier_right = sum(is_outlier_right),
    pct_left = outlier_left/total*100,
    pct_right = outlier_right/total*100
  )

#Check IQR out of the peak_hour
df_list <- list(
  trip_2019 = trip_2019 %>% filter(is_peak == FALSE), 
  trip_2020 = trip_2020 %>% filter(is_peak == FALSE)
)

df_list <- map(df_list, add_iqr)
iqr_peak_stat_2019 <- df_list$trip_2019
iqr_peak_stat_2020 <- df_list$trip_2020

#Check for round trip portion in 2019 data base on user_type
trip_2019 %>% 
  count(user_type, round_trip) %>% 
  group_by(user_type) %>% 
  mutate(
    pct_round_trip = n/sum(n)*100
  )

#Check for round trip portion in 2020 data base on user_type
trip_2020 %>% 
  count(user_type, round_trip) %>% 
  group_by(user_type) %>% 
  mutate(
    pct_round_trip = n/sum(n)*100
  )

#Check for statistic index of the data
df_list <- list(
  trip_2019 = trip_2019,
  trip_2020 = trip_2020
)

df_list <- map(df_list, add_iqr)
 
iqr_all_stat_2019 <- df_list$trip_2019
iqr_all_stat_2020 <- df_list$trip_2020

#Checking 2019 data to see the age distribution
#column chart base on age group
trip_2019_demo %>% 
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 18, 25, 35, 45, 55, 65, 120),
      right = FALSE
    )
  ) %>% 
  count(age_group, user_type) %>% 
  group_by(age_group) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(x = age_group, y = pct, fill = user_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)

#Calculate the member portion in each age group
trip_2019_demo %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 18, 25, 35, 45, 55, 65, 120),
      right = FALSE
    )
  ) %>% 
  count(age_group, user_type) %>% 
  group_by(age_group) %>% 
  mutate(
    pct = round(n/sum(n)*100, 2)
  ) %>% 
  filter(user_type == "member")

#Calculate IQR between age group
iqr_age_group <- trip_2019_demo %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 18, 25, 35, 45, 55, 65, 120),
      right = FALSE
    )
  ) %>% 
  group_by(age_group) %>%
  summarise(
    q1 = quantile(duration_sec, 0.25, na.rm = TRUE),
    med = median(duration_sec, na.rm = TRUE),
    q3 = quantile(duration_sec, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    outlier_pct = mean(
      duration_sec < (q1 - 1.5 * iqr) | duration_sec > (q3 + 1.5 * iqr)
    )*100
  )

#Checking for portion of each type user ride in weekend
trip_2019 %>% 
  count(is_weekend, user_type) %>% 
  group_by(user_type) %>% 
  mutate(
    pct = n/sum(n)
  )

trip_2020 %>% 
  count(is_weekend, user_type) %>% 
  group_by(user_type) %>% 
  mutate(
    pct = n/sum(n)
  )

#Check for distribution of user_type per route
member_route_2019 <- trip_2019 %>% 
  filter(user_type == "member") %>% 
  count(route_id) %>% 
  mutate(
    pct = n/sum(n)*100
  )

casual_route_2019 <- trip_2019 %>% 
  filter(user_type == "casual") %>% 
  count(route_id) %>% 
  mutate(
    pct = n/sum(n)*100
  )

p1 <- ggplot() +
  geom_histogram(
    data = member_route_2019,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) + 
  labs(
    title = "Distribution of member route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p2 <- ggplot() +
  geom_histogram(
    data = casual_route_2019,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) +
  labs(
    title = "Distribution of casual route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p1/p2

#Validate IQR index
iqr_route_casual_2019 <- casual_route_2019 %>% 
  summarize(
    user_type = "casual",
    med = median(pct),
    q1 = quantile(pct, 0.25, na.rm = 0),
    q3 = quantile(pct, 0.75, na.rm = 0),
    iqr = q3 - q1,
    max_route = max(pct)
  )

iqr_route_member_2019 <- member_route_2019 %>% 
  summarize(
    user_type = "member",
    med = median(pct),
    q1 = quantile(pct, 0.25, na.rm = 0),
    q3 = quantile(pct, 0.75, na.rm = 0),
    iqr = q3 - q1,
    max_route = max(pct)
  )

iqr_route_2019 <- bind_rows(
  iqr_route_member_2019,
  iqr_route_casual_2019
)

iqr_route_2019

#Check for distribution of user_type per route
member_route_2020 <- trip_2020 %>% 
  filter(user_type == "member") %>% 
  count(route_id) %>% 
  mutate(
    pct = n/sum(n)*100
  )

casual_route_2020 <- trip_2020 %>% 
  filter(user_type == "casual") %>% 
  count(route_id) %>% 
  mutate(
    pct = n/sum(n)*100
  )

p1 <- ggplot() +
  geom_histogram(
    data = member_route_2020,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) + 
  labs(
    title = "Distribution of member route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p2 <- ggplot() +
  geom_histogram(
    data = casual_route_2020,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) +
  labs(
    title = "Distribution of casual route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p1/p2

#Validate IQR index
iqr_route_casual_2020 <- casual_route_2020 %>% 
  summarize(
    user_type = "casual",
    med = median(pct),
    q1 = quantile(pct, 0.25, na.rm = 0),
    q3 = quantile(pct, 0.75, na.rm = 0),
    iqr = q3 - q1,
    max_route = max(pct)
  )

iqr_route_member_2020 <- member_route_2020 %>% 
  summarize(
    user_type = "member",
    med = median(pct),
    q1 = quantile(pct, 0.25, na.rm = 0),
    q3 = quantile(pct, 0.75, na.rm = 0),
    iqr = q3 - q1,
    max_route = max(pct)
  )

iqr_route_2020 <- bind_rows(
  iqr_route_member_2020,
  iqr_route_casual_2020
)

iqr_route_2020

prep_for_powerbi <- function(df) {
  df %>%
    mutate(
      across(where(is.factor), as.character),
      across(where(is.logical), as.integer),
      across(where(~ inherits(.x, "Date")), ~ format(.x, "%Y-%m-%d")),
      across(where(~ inherits(.x, "POSIXct")), ~ format(.x, "%Y-%m-%d %H:%M:%S"))
    ) %>%
    janitor::clean_names()
}

iwalk(
  dfs,
  ~ write_csv(
    prep_for_powerbi(.x),
    paste0("bi/", .y, ".csv")
  )
)

dfs = list(
  member_route = member_route,
  casual_route = casual_route,
  entire_data,
  casual_route_2019 = casual_route_2019,
  member_route_2019 = member_route_2019,
  casual_route_2020 = casual_route_2020,
  member_route_2020 = member_route_2020,
  pct_vld_2019 = pct_vld_2019,
  pct_vld_2020 = pct_vld_2020,
  trip_2019 = trip_2019,
  trip_2019_demo = trip_2019_demo,
  trip_2020 = trip_2020
)

info_2019 <- trip_2019 %>% mutate(year = 2019, trip_id = as.character(trip_id))
info_2020 <- trip_2020 %>% mutate(year = 2020, trip_id = as.character(trip_id))

common_data <- intersect(names(info_2019), names(info_2020))

entire_data <- bind_rows(
  info_2019 %>% select(all_of(common_data)),
  info_2020 %>% select(all_of(common_data))
)

member_route <- bind_rows(
  member_route_2019,
  member_route_2020
)

casual_route <- bind_rows(
  casual_route_2019,
  casual_route_2020
)

p1 <- ggplot() +
  geom_histogram(
    data = member_route,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) + 
  labs(
    title = "Distribution of member route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p2 <- ggplot() +
  geom_histogram(
    data = casual_route,
    aes(x = pct),
    alpha = 0.6,
    bins = 50,
  ) +
  labs(
    title = "Distribution of casual route portion",
    x = "Percentage of trips per route (%)",
    y = "Number of routes"
  )

p1/p2

entire_data <- entire_data %>%
  mutate(
    duration_log10    = log10(duration_sec + 1)
  )

route_summary <- entire_data %>% 
  group_by(user_type, route_id) %>% 
  summarise(
    trip_count = n(),
    .groups = "drop"
  )

route_ranked <- route_summary %>% 
  group_by(user_type) %>% 
  arrange(desc(trip_count)) %>% 
  mutate(
    route_rank = row_number(),
    trip_pct = trip_count / sum(trip_count),
    cum_trip_pct = cumsum(trip_pct)
  ) %>% 
  ungroup()

route_ranked <- route_ranked %>% 
  select(user_type, route_rank, cum_trip_pct)

dfs = list(
  route_ranked = route_ranked
)

trip_2019_demo <- trip_2019_demo %>% 
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 18, 25, 35, 45, 55, 65, 120),
      right = FALSE
    ),
    age_rank = case_when(
      age_group == "[0,18)" ~ 1,
      age_group == "[18,25)" ~ 2,
      age_group == "[25,35)" ~ 3,
      age_group == "[35,45)" ~ 4,
      age_group == "[45,55)" ~ 5,
      age_group == "[55,65)" ~ 6,
      age_group == "[65,120)" ~ 7
    ),
    duration_log10 = log10(duration_sec +1)
  ) %>% 
  select(trip_id, user_type, age_group, age_rank, duration_sec, duration_log10)

dfs = list(
  trip_2019_demo = trip_2019_demo
)

kpi_50pct <- route_ranked %>% 
  filter(cum_trip_pct >= 0.5) %>% 
  group_by(user_type) %>% 
  summarise(
    routes_for_50pct = min(route_rank),
    .groups = "drop"
  )

kpi_top_routes <- route_ranked %>% 
  filter(route_rank <= 10) %>% 
  group_by(user_type) %>% 
  summarise(
    pct_trip_top10 = sum(trip_pct),
    .groups = "drop"
  )

kpi_summary <- left_join(kpi_50pct, kpi_top_routes, by = "user_type")

dfs = list(
  kpi_summary = kpi_summary
)

add_iqr <- function(df){
  df %>% 
    group_by(user_type) %>%
    summarise(
      q1 = quantile(duration_sec, 0.25, na.rm = TRUE),
      med = median(duration_sec, na.rm = TRUE),
      q3 = quantile(duration_sec, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr
    )
}

df_list <- list(
  entire_data = entire_data
)

df_list <- map(df_list, add_iqr)

iqr_entire_data <- df_list$entire_data

dfs = list(
  iqr_entire_data = iqr_entire_data
)
