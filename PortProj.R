#______________PREPARE DATA____________

#set working directory
setwd("C:/Users/Rachel/Documents/Grad School/MIS581/Portfolio Project")

#Import libraries
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(zoo)
library(tidyverse)
library(lubridate)

#Load data
visit_data<-read.csv("~/Grad School/MIS581/Portfolio Project/air_visit_data.csv")
date_data<-read.csv("~/Grad School/MIS581/Portfolio Project/date_info.csv")

#Combine visit_data with date_data
combined_data <- merge(visit_data, date_data, by.x = "visit_date", by.y = "calendar_date")
combined_data$visit_date <- as.Date(combined_data$visit_date)
combined_data$holiday_flg <- as.factor(combined_data$holiday_flg)

#Aggregate data by date
aggregated_data <- combined_data %>%
  group_by(visit_date) %>%
  summarise(total_visitors = sum(visitors, na.rm = TRUE), 
            holiday_flg = first(holiday_flg))

#Fill missing date with NA
all_dates <- seq.Date(min(aggregated_data$visit_date), max(aggregated_data$visit_date), by = "day")
aggregated_data <- full_join(data.frame(visit_date = all_dates), aggregated_data, by = "visit_date")

#Interpolate Missing Values
aggregated_data$total_visitors <- zoo::na.approx(aggregated_data$total_visitors, rule = 2)

#Aggregate data by month
monthly_data <- aggregated_data %>%
  mutate(month = floor_date(visit_date, "month")) %>%
  group_by(month)%>%
  summarise(total_visitors = sum(total_visitors, na.rm = TRUE))

#_________EXPLORATORY ANALYSIS___________#

#Plot Time Series
ggplot(monthly_data, aes(x = month, y = total_visitors)) +
  geom_line() +
  labs(title = "Monthly Visitors Aggregated Across All Restaurants",
       x = "Month",
       y = "Total Visitors") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

summary(monthly_data)
summary(aggregated_data)

#__________TIME SERIES ANALYSIS___________#

#Create Time Series
ts_data <- ts(monthly_data$total_visitors,
              frequency = 12, start = c(2016, 1))

#Fourier terms for seasonality
K <- 4
fourier_terms <- fourier(ts_data, K = K)
fit <- auto.arima(ts_data, xreg = fourier_terms)
summary(fit)

#Forecast future visitors with Fourier terms
forecasted <- forecast(fit, xreg = fourier(ts_data, K = K, h = 12))
forecast_df <- data.frame(
  Date = as.Date(time(forecasted$mean)),
  Forecast = as.numeric(forecasted$mean),
  Lower = as.numeric(forecasted$lower[,2]),
  Upper = as.numeric(forecasted$upper[,2])
)

historical_df <- data.frame(
  Date = as.Date(time(ts_data)),
  Visitors = as.numeric(ts_data)
)

ggplot() +
  geom_line(data = historical_df, aes(x = Date, y = Visitors, color = "Historical Data "), size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast, color = "Forecast"), size = 1) +
  labs(title = "Forecast of Total Visitors with Fourier Terms",
       x = "Date",
       y = "Total Visitors") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "3 months") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_blank(),
    legend.position = "bottom") +
  scale_color_manual(values = c("Forecast" = "red"))

#ANOVA Total Visitors
anova_result <- aov(total_visitors ~ holiday_flg, data = aggregated_data)
summary(anova_result)

holiday_counts <- aggregated_data %>%
  group_by(holiday_flg) %>%
  summarise(count = n())
ggplot(holiday_counts, aes(x = holiday_flg, y = count, fill = holiday_flg)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(title = "Number of Holidays vs. Non-Holidays",
       x = "Holiday",
       y = "Count") +
  scale_x_discrete(labels = c("Non-Holiday", "Holiday")) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
