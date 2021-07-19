install.packages("tidyverse")
install.packages("forcats")
install.packages("lubridate")
install.packages("skimr")
install.packages("mgcv")
install.packages("gratia")

library(tidyverse)
library(forcats)
library(lubridate)
library(skimr)
library(mgcv)
library(gratia)

features_df <- read_csv("data/features.csv")
train_df <- read_csv("data/train.csv")

glimpse(features_df)

summary(features_df)

skim(features_df)

features_df$Store <- as_factor(features_df$Store)
skim(features_df)

filter(filter(features_df, Temperature > 78), Store == 1)

features_df %>%
  filter(Store == 1) 

store_1_subset <- features_df %>%
  filter(Store == 1) 

features_df %>%
  filter(Store == 1 & Date > as_date("2012-01-01"))

features_df %>%
  filter(Store == 1) %>%
  filter(Date > as_date("2012-01-01"))

features_df %>%
  filter(Store == 1) %>%
  select(Store, Date, Temperature, Fuel_Price)

features_df %>%
  filter(Store == 1) %>%
  select(-Temperature)

features_df %>%
  skim()

features_df_clean <- features_df %>%
  drop_na(CPI, Unemployment) %>%
  select(-starts_with("Markdown"))

features_df_clean <- features_df_clean %>%
  mutate(
    Day = day(Date),
    Month = month(Date),
    Year = year(Date),
    Week = week(Date)
  )

head(features_df_clean)

features_df %>%
  ggplot(aes(x = Date, y = Temperature)) +
  geom_point(aes(color = Store))


sample_stores <- sample(1:50, 10)

features_df %>%
  filter(Store %in% sample_stores) %>%
  ggplot(aes(x = Date, y = Temperature)) +
  geom_point(aes(color = Store), alpha = 0.4)


features_df %>%
  ggplot(aes(CPI)) +
  geom_histogram(fill = "white", color = "grey30")


features_df %>%
  group_by(Store) %>%
  summarise(avg_temp = mean(Temperature)) %>%
  arrange(avg_temp)


features_df %>%
  group_by(Store) %>%
  summarise(avg_temp = mean(Temperature)) %>%
  ggplot(aes(x = Store, y = avg_temp)) +
  geom_point()

skim(train_df)

train_df$Store <- as_factor(train_df$Store)
train_df$Dept <- as_factor(train_df$Dept)

train_df %>%
  filter(Store == 1) %>%
  filter(Dept == 2) %>%
  ggplot(aes(Date, Weekly_Sales, color = IsHoliday)) +
  geom_point()

train_df %>%
  group_by(Store) %>%
  summarise(total_sales = sum(Weekly_Sales)) %>%
  ggplot(aes(Store, total_sales)) +
  geom_col()

train_mod <- train_df %>%
  filter(Store == 1) %>%
  filter(Dept == 1) %>%
  mutate(
    Week = week(Date)
  )

train_mod %>%
  ggplot(aes(Week, Weekly_Sales)) +
  geom_point()


gam_mod <- gam(Weekly_Sales ~ s(Week), data=train_mod, method = "REML")

draw(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1, cex = 1, shade = TRUE)
