# --- Load Libraries ---
library(tidymodels)
library(vroom)
library(dplyr)
library(lubridate)

library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(dplyr)
library(patchwork)
library(glmnet)


library(embed)
library(lme4)
library(kknn)

wal_train <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/train.csv")
wal_test  <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/test.csv")
wal_stores <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/stores.csv")
wal_feat <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/features.csv")

# pull off all the features of DATE
# step_data(dateVar, features =c("))
# KNN - Riley

wal_feat <- wal_feat %>%
  mutate(across(starts_with("Markdown"),
                ~ ifelse(is.na(.) | . < 0, 0, .)),
         TotalMarkdown = rowSums(across(starts_with("Markdown"))),
         MarkdownFlag = TotalMarkdown > 0) %>%
  mutate(
    DecDate = decimal_date(Date),
    CPI = if_else(is.na(CPI), mean(CPI, na.rm = TRUE), CPI),
    Unemployment = if_else(is.na(Unemployment), mean(Unemployment, na.rm = TRUE), Unemployment)
  )



train_joined <- wal_train %>%
  left_join(wal_feat, by = c("Store", "Date"))

test_joined <- wal_test %>%
  left_join(wal_feat, by = c("Store", "Date"))


# 
#recipe <- recipe(ACTION ~ ., data = train_joined)


#cpi
#unemployment
# Total markdown

## Prophet Model

library(prophet)

store <- 11
dept  <- 11

sd_train <- train_joined %>%
  filter(Store == store, Dept == dept) %>%
  rename(y = Weekly_Sales, ds = Date)

sd_test <- test_joined %>%
  filter(Store == store, Dept == dept) %>%
  rename(ds = Date)

prophet_model <- prophet() %>%
  # add_regressor("CPI") %>%
  # add_regressor("Unemployment") %>%
  # add_regressor("TotalMarkdown") %>%
  fit.prophet(df = sd_train)

forecast <- predict(prophet_model, sd_test)

train_cast <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values

fitted_vals <- predict(prophet_model, df = sd_train)
test_preds <- predict(prophet_model, df = sd_test)

# You already have:
# prophet_model <- prophet() %>% fit.prophet(df = sd_train)

df_cv <- cross_validation(
  prophet_model,
  initial = 365,      # training window
  period = 90,        # spacing between cutoff dates
  horizon = 90,       # forecast window length
  units = "days"
)

df_perf <- performance_metrics(df_cv)
df_perf


# ggplot() +
#   geom_smooth(data = sd_train, aes(x = ds, y = y, color = "Data"),
#               method = "loess", se = FALSE, linetype = "dashed") +
#   geom_smooth(data = fitted_vals, aes(x = as.Date(ds), y = yhat, color = "Fitted"),
#               method = "loess", se = FALSE, linetype = "dashed") +
#   
#   geom_smooth(data = test_preds, aes(x = as.Date(ds), y = yhat, color = "Forecast"),
#               method = "loess", se = FALSE, linetype = "dashed") +
#   
#   scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
#   labs(color="")


g1 <- ggplot() +
  geom_line(data = sd_train, aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black",
                                "Fitted" = "blue",
                                "Forecast" = "red")) +
  labs(color = "", 
       title = paste("Store", store, "- Dept", dept, "Sales Forecast")) +
  theme_grey()

store <- 17
dept  <- 17

# Prepare train/test subsets
sd_train <- train_data %>%
  filter(Store == store, Dept == dept) %>%
  rename(y = Weekly_Sales, ds = Date)

sd_test <- test_data %>%
  filter(Store == store, Dept == dept) %>%
  rename(ds = Date)

# Fit Prophet model with regressors
prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  add_regressor("Unemployment") %>%
  add_regressor("TotalMarkdown") %>%
  fit.prophet(df = sd_train)

# Predictions
fitted_vals <- predict(prophet_model, df = sd_train)
test_preds  <- predict(prophet_model, df = sd_test)

# Plot
g2 <- ggplot() +
  geom_line(data = sd_train, aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black",
                                "Fitted" = "blue",
                                "Forecast" = "red")) +
  labs(color = "",
       title = paste("Store", store, "- Dept", dept, "Sales Forecast")) +
  theme_grey()

plot <- g1 / g2


-------------------------------------------------------------------------------
  
  
  library(tidyverse)
library(vroom)
library(tidymodels)
library(lubridate)
library(slider)
library(purrr)

#####################
# 1. Load Data
#####################
train <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/train.csv")
test <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/test.csv")
features <- vroom("~/GitHub/Stat 348/Walmart/walmart-recruiting-store-sales-forecasting/DATA/features.csv")

#####################
# 2. Feature Engineering
#####################

# Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm=TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log = log1p(MarkDown_Total)
  ) %>%
  select(-starts_with("MarkDown"))

# Impute CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with = imp_vars(DecDate, Store))

features_imputed <- juice(prep(feature_recipe))

# Merge with train/test
fullTrain <- train %>%
  left_join(features_imputed, by=c("Store","Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)

fullTest <- test %>%
  left_join(features_imputed, by=c("Store","Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)

# Encode holidays & date features
encode_holidays <- function(df){
  df %>%
    mutate(
      IsHoliday = as.integer(IsHoliday),
      Month = month(Date),
      Week = week(Date),
      Holiday_Month = interaction(IsHoliday, Month)
    )
}

fullTrain <- encode_holidays(fullTrain)
fullTest <- encode_holidays(fullTest)

#####################
# 3. Create Rolling Features
#####################

# Train rolling features
fullTrain <- fullTrain %>%
  group_by(Store, Dept) %>%
  arrange(Date) %>%
  mutate(
    Rolling4 = slide_dbl(Weekly_Sales, mean, .before=3, .complete=TRUE),
    Rolling13 = slide_dbl(Weekly_Sales, mean, .before=12, .complete=TRUE)
  ) %>%
  ungroup()

# Test rolling features
fullTest <- fullTest %>%
  group_by(Store, Dept) %>%
  arrange(Date) %>%
  group_modify(~{
    train_sales <- fullTrain %>%
      filter(Store==.y$Store, Dept==.y$Dept) %>%
      arrange(Date) %>%
      pull(Weekly_Sales)
    
    combined <- c(train_sales, rep(NA_real_, nrow(.x)))
    
    roll4 <- slide_dbl(combined, mean, .before=3, .complete=TRUE)
    roll13 <- slide_dbl(combined, mean, .before=12, .complete=TRUE)
    
    .x <- .x %>%
      mutate(
        Rolling4 = roll4[(length(train_sales)+1):length(roll4)],
        Rolling13 = roll13[(length(train_sales)+1):length(roll13)]
      )
    .x
  }) %>%
  ungroup()

#####################
# 4. Define Store/Dept Model Function
#####################
predict_store_dept <- function(df_train, df_test){
  if(nrow(df_train) == 0){
    return(df_test %>% transmute(Id = paste(Store, Dept, Date, sep="_"), Weekly_Sales = 0))
  } else if(nrow(df_train) < 10){
    return(df_test %>% transmute(Id = paste(Store, Dept, Date, sep="_"), Weekly_Sales = mean(df_train$Weekly_Sales)))
  } else {
    my_recipe <- recipe(Weekly_Sales ~ ., data=df_train) %>%
      step_rm(Date, Store, Dept)
    
    my_model <- rand_forest(mtry=3, trees=100, min_n=5) %>%
      set_engine("ranger") %>%
      set_mode("regression")
    
    my_wf <- workflow() %>%
      add_recipe(my_recipe) %>%
      add_model(my_model) %>%
      fit(df_train)
    
    df_test %>%
      transmute(
        Id = paste(Store, Dept, Date, sep="_"),
        Weekly_Sales = predict(my_wf, new_data = .) %>% pull(.pred)
      )
  }
}

#####################
# 5. Generate Predictions
#####################
all_preds <- fullTest %>%
  group_by(Store, Dept) %>%
  group_split() %>%
  map_dfr(~{
    store <- .x$Store[1]
    dept  <- .x$Dept[1]
    predict_store_dept(
      df_train = fullTrain %>% filter(Store == store, Dept == dept),
      df_test  = .x
    )
  })

#####################
# 6. Write Predictions
#####################
library(tidyverse)
library(vroom)
library(tidymodels)
library(lubridate)
library(slider)
library(purrr)

#####################
# 1. Load Data
#####################
train <- vroom("~/Downloads/WalmartRecruitingComp/walmart-recruiting-store-sales-forecasting/train.csv")
test <- vroom("~/Downloads/WalmartRecruitingComp/walmart-recruiting-store-sales-forecasting/test.csv")
features <- vroom("~/Downloads/WalmartRecruitingComp/walmart-recruiting-store-sales-forecasting/features.csv")

#####################
# 2. Feature Engineering
#####################

# Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm=TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log = log1p(MarkDown_Total)
  ) %>%
  select(-starts_with("MarkDown"))

# Impute CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with = imp_vars(DecDate, Store))

features_imputed <- juice(prep(feature_recipe))

# Merge with train/test
fullTrain <- train %>%
  left_join(features_imputed, by=c("Store","Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)

fullTest <- test %>%
  left_join(features_imputed, by=c("Store","Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)

# Encode holidays & date features
encode_holidays <- function(df){
  df %>%
    mutate(
      IsHoliday = as.integer(IsHoliday),
      Month = month(Date),
      Week = week(Date),
      Holiday_Month = interaction(IsHoliday, Month)
    )
}

fullTrain <- encode_holidays(fullTrain)
fullTest <- encode_holidays(fullTest)

#####################
# 3. Create Rolling Features
#####################

# Train rolling features
fullTrain <- fullTrain %>%
  group_by(Store, Dept) %>%
  arrange(Date) %>%
  mutate(
    Rolling4 = slide_dbl(Weekly_Sales, mean, .before=3, .complete=TRUE),
    Rolling13 = slide_dbl(Weekly_Sales, mean, .before=12, .complete=TRUE)
  ) %>%
  ungroup()

# Test rolling features
fullTest <- fullTest %>%
  group_by(Store, Dept) %>%
  arrange(Date) %>%
  group_modify(~{
    train_sales <- fullTrain %>%
      filter(Store==.y$Store, Dept==.y$Dept) %>%
      arrange(Date) %>%
      pull(Weekly_Sales)
    
    combined <- c(train_sales, rep(NA_real_, nrow(.x)))
    
    roll4 <- slide_dbl(combined, mean, .before=3, .complete=TRUE)
    roll13 <- slide_dbl(combined, mean, .before=12, .complete=TRUE)
    
    .x <- .x %>%
      mutate(
        Rolling4 = roll4[(length(train_sales)+1):length(roll4)],
        Rolling13 = roll13[(length(train_sales)+1):length(roll13)]
      )
    .x
  }) %>%
  ungroup()

#####################
# 4. Define Store/Dept Model Function
#####################
predict_store_dept <- function(df_train, df_test){
  if(nrow(df_train) == 0){
    return(df_test %>% transmute(Id = paste(Store, Dept, Date, sep="_"), Weekly_Sales = 0))
  } else if(nrow(df_train) < 10){
    return(df_test %>% transmute(Id = paste(Store, Dept, Date, sep="_"), Weekly_Sales = mean(df_train$Weekly_Sales)))
  } else {
    my_recipe <- recipe(Weekly_Sales ~ ., data=df_train) %>%
      step_rm(Date, Store, Dept)
    
    my_model <- rand_forest(mtry=3, trees=100, min_n=5) %>%
      set_engine("ranger") %>%
      set_mode("regression")
    
    my_wf <- workflow() %>%
      add_recipe(my_recipe) %>%
      add_model(my_model) %>%
      fit(df_train)
    
    df_test %>%
      transmute(
        Id = paste(Store, Dept, Date, sep="_"),
        Weekly_Sales = predict(my_wf, new_data = .) %>% pull(.pred)
      )
  }
}

#####################
# 5. Generate Predictions
#####################
all_preds <- fullTest %>%
  group_by(Store, Dept) %>%
  group_split() %>%
  map_dfr(~{
    store <- .x$Store[1]
    dept  <- .x$Dept[1]
    predict_store_dept(
      df_train = fullTrain %>% filter(Store == store, Dept == dept),
      df_test  = .x
    )
  })

#####################
# 6. Write Predictions
#####################
vroom_write(x=all_preds, 
            file=paste0("./Predictions.csv"), delim=",")


