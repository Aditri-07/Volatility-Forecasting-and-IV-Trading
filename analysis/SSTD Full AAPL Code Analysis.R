library(fGarch)
library(readxl)
library(tidyverse)  
library(lubridate)  
library(readxl)     
library(stats)     
library(zoo)        
library(lmtest)     
library(car)  
library(xts)
library(fGarch)
library(readxl)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(ggplot2)

#PRELIMINARY ANALYSIS----------------------------------------------------------------------------------
my_data<-read_excel("AAPLDATA.xlsx", col_names=TRUE)
my_data$Date<-as.Date(my_data$Date)
xts_data<-xts(my_data$AdjClose, order.by=my_data$Date)
colnames(xts_data)<-"AdjClose"

log_returns<-Return.calculate(xts_data, method="log")
log_returns<-na.omit(log_returns)
log_returns

acf(log_returns) 
acf(log_returns^2) 

Box.test(log_returns^2,lag=10,type="Ljung")
pacf(log_returns^2,lag=10, main="AAPL Squared Log Returns")

#Testing ARCH 3 model
model_1=garchFit(~garch(9,0),data=log_returns)
summary(model_1)

#Testing ARCH 1 model
model_2=garchFit(~garch(1,0),data=log_returns)
summary(model_2)

model_3=garchFit(~garch(1,0),data=log_returns,trace=F,cond.dist=c("std"))
summary(model_3)

model_4=garchFit(~garch(1,0),data=log_returns,trace=F,cond.dist=c("sstd"))
summary(model_4)

get_residuals<-residuals(model_4,standardize=T)
qqplot(rsstd(5000,0,1,6.74,0.836),get_residuals)
qqline(get_residuals)

model_5=garchFit(~1+garch(1,1), data=log_returns,trace=F)
summary(model_5)

conditional_stds=model_5@sigma.t 
residuals=residuals(model_5, standardize=T) 

vol_AAPL=ts(conditional_stds, frequency=252, start=c(2022,1))
res_AAPL=ts(residuals, frequency=252, start=c(2022,1))
plot(vol_AAPL,xlab='year',ylab='volatility',type='l')

plot(res_AAPL,xlab='year',ylab='residuals',type='l')

par(mfcol=c(2,2))
acf(residuals, lag=24)
pacf(residuals, lag=24)
acf(residuals^2, lag=24)
pacf(residuals^2, lag=24)

model_6=garchFit(~1+garch(1,1), data=log_returns,trace=F,cond.dist='std')
summary(model_6)

model_7=garchFit(~1+garch(1,1), data=log_returns,trace=F,cond.dist='sstd')
summary(model_7)

resid_model7<-residuals(model_7, standardize=TRUE)


acf(resid_model7, main="ACF of Residuals: AAPL Model 7")
pacf(resid_model7, main="PACF of Residuals: AAPL Model 7")
acf(resid_model7^2, main="ACF of Squared Residuals: AAPL Model 7")
pacf(resid_model7^2, main="PACF of Squared Residuals: AAPL Model 7")

conditional_stds6=model_6@sigma.t
residuals6=residuals(model_6, standardize=T)
vol_AAPL6=ts(conditional_stds6, frequency=252, start=c(2022,1))
res_AAPL6=ts(residuals6, frequency=252, start=c(2022,1))
plot(vol_AAPL6,xlab='year',ylab='volatility',type='l')
plot(res_AAPL6,xlab='year',ylab='residuals',type='l')

conditional_stds7=model_6@sigma.t
residuals7=residuals(model_7, standardize=T)
vol_AAPL7=ts(conditional_stds7, frequency=252, start=c(2022,1))
res_AAPL7=ts(residuals7, frequency=252, start=c(2022,1))
plot(vol_AAPL7,xlab='year',ylab='volatility',type='l')
plot(res_AAPL7,xlab='year',ylab='residuals',type='l')

model_list<-list(model_5, model_6, model_7)
model_names<-c("model_5", "model_6", "model_7")

log_returns<-as.numeric(log_returns)

all_days<-seq(as.Date("2022-01-03"), by="day", length.out=length(log_returns)+100)
tdx<-all_days[weekdays(all_days) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")][1:length(log_returns)]

par(mfcol=c(1,1))

for (i in 1:length(model_list)) {
  
  model<-model_list[[i]]
  model_name<-model_names[i]
  conditional_stds<-model@sigma.t
  
  mu<-mean(log_returns)
  upp<-mu+2*conditional_stds
  low<-mu-2*conditional_stds
  
  plot(tdx, log_returns, type="l", xlab="Year", ylab="Log Returns",
       main=paste("AAPL Log Returns with ±2σ GARCH Bands (", model_name, ")", sep=""),
       ylim=range(c(log_returns, upp, low)))
  lines(tdx, upp, col="red", lty=2)
  lines(tdx, low, col="red", lty=2)
  abline(h=mu, col="blue", lty=3)
}

x<-as.numeric(tail(log_returns,1))
mu<-mean(log_returns)
at<-x-mu
last_vol<-tail(conditional_stds7,1)
omega<-2.312e-06
alpha1<- 4.125e-02
beta1<-9.513e-01
f<-omega+alpha1*at^2+beta1*(last_vol)^2
sqrt(f)

predict(model_7,5)

#Forecasting Using GARCH(1,1)-SSTD----------------------------------------------------------------------------------
df<- read_excel("AAPLDATA.xlsx", col_names = TRUE)

df$Date <- as.Date(df$Date, format="%Y-%m-%d")
df$LogReturns <- c(NA, diff(log(df$AdjClose), lag = 1))
head(df)

target_dates <- c("2024-12-13", "2024-12-20", "2024-12-27", "2025-01-03", "2025-01-10", "2025-01-17", 
                  "2025-01-24","2025-01-31", "2025-02-07", "2025-02-14", "2025-02-21", "2025-02-28", 
                  "2025-03-07", "2025-03-14", "2025-03-21","2025-03-28","2025-04-04", "2025-04-11")

get_training_data <- function(df, target_date) {
  start_date <- as.Date(target_date) - 365*2  # Two years before the target date
  end_date <- as.Date(target_date) - 1  # One day before the target date
  return(subset(df, Date >= start_date & Date <= end_date))
}

predict_volatility <- function(training_data) {
  model <- garchFit(~ garch(1, 1), data = training_data$LogReturns, cond.dist = "sstd", trace = FALSE)
  
  forecast <- predict(model, n.ahead = 1)
  
  return(forecast$standardDeviation)
}

volatility_results <- data.frame(
  TargetDate = character(),
  PredictedVolatility = numeric(),
  stringsAsFactors = FALSE
)

for (date in target_dates) {
  training_data <- get_training_data(df, date)
  predicted_volatility <- predict_volatility(training_data)
  
  volatility_results <- rbind(volatility_results, data.frame(
    TargetDate = date,
    PredictedVolatility = predicted_volatility
  ))
}

print(volatility_results)

#AAPL CALLS SIMPLE REGRESSION----------------------------------------------------------------------------------
df_calls<- read_excel("AAPL Calls SSTD.xlsx", col_names = TRUE)
model_full_calls <- lm(IV_BSM ~ IV_Garch, data = df_calls)
cat("\nFull Model Summary (Entire Dataset):\n")
summary(model_full_calls)

plot(model_full_calls$fitted.values, residuals(model_full_calls),
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

p_value_intercept_full <- summary(model_full_calls)$coefficients[1, 4]
p_value_slope_full <- summary(model_full_calls)$coefficients[2, 4]
cat("\nIntercept p-value (Full Model):", p_value_intercept_full, "\n")
cat("Slope p-value (Full Model):", p_value_slope_full, "\n")

if (p_value_intercept_full < 0.05) {
  cat("Intercept is statistically significant (Full Model).\n")
} else {
  cat("Intercept is not statistically significant (Full Model).\n")
}

if (p_value_slope_full < 0.05) {
  cat("Slope is statistically significant (Full Model).\n")
} else {
  cat("Slope is not statistically significant (Full Model).\n")
}

# Check for Gauss-Markov assumptions ------------------------------------------------------------

# 1. Independence and Randomness of Error Terms: Durbin-Watson test
dw_test <- dwtest(model_full_calls)
cat("\nDurbin-Watson Test for Autocorrelation:\n")
print(dw_test)

# 2. Zero Conditional Mean: Residuals should have a mean of zero
mean_residuals <- mean(residuals(model_full_calls))
cat("\nMean of Residuals (Zero Conditional Mean):", mean_residuals, "\n")

# 3. Homoskedasticity of Error Terms: Breusch-Pagan Test
bp_test <- bptest(model_full_calls)
cat("\nBreusch-Pagan Test for Homoskedasticity:\n")
print(bp_test)

# 4. Normality of Error Terms: Normal Probability Plot and Shapiro-Wilk Test
cat("\nNormality of Error Terms (QQ plot and Shapiro-Wilk test):\n")
qqnorm(residuals(model_full_calls))
qqline(residuals(model_full_calls), col = "red")
shapiro_test <- shapiro.test(residuals(model_full_calls))
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test)

# Results Interpretation
if (dw_test$p.value < 0.05) {
  cat("Independence of Error Terms Violated: Autocorrelation detected.\n")
} else {
  cat("Independence of Error Terms Satisfied.\n")
}

if (abs(mean_residuals) > 0.05) {
  cat("Zero Conditional Mean Violated: Residuals mean is not zero.\n")
} else {
  cat("Zero Conditional Mean Satisfied.\n")
}

if (bp_test$p.value < 0.05) {
  cat("Homoskedasticity Violated: Heteroskedasticity detected.\n")
} else {
  cat("Homoskedasticity Satisfied.\n")
}

if (shapiro_test$p.value < 0.05) {
  cat("Normality of Error Terms Violated: Residuals not normally distributed.\n")
} else {
  cat("Normality of Error Terms Satisfied.\n")
}

#Chow Test ----------
n <- nrow(df_calls)  
split_index <- floor(n / 2)
df_calls_part1 <- df_calls[1:split_index, ]
df_calls_part2 <- df_calls[(split_index + 1):n, ]
model_part1 <- lm(IV_BSM ~ IV_Garch, data = df_calls_part1)
model_part2 <- lm(IV_BSM ~ IV_Garch, data = df_calls_part2)
model_full <- lm(IV_BSM ~ IV_Garch, data = df_calls)
rss_full <- sum(residuals(model_full)^2)
rss_part1 <- sum(residuals(model_part1)^2)
rss_part2 <- sum(residuals(model_part2)^2)
n1 <- nrow(df_calls_part1) 
n2 <- nrow(df_calls_part2)
k <- length(coef(model_full))  
f_statistic <- ((rss_full - (rss_part1 + rss_part2)) / k) / ((rss_part1 + rss_part2) / (n1 + n2 - 2 * k))
cat("\nChow Test F-statistic:", f_statistic, "\n")
alpha <- 0.05
f_critical <- qf(1 - alpha, df1 = k, df2 = n1 + n2 - 2 * k)
cat("Chow Test Critical F-statistic at 5% significance level:", f_critical, "\n")
if (f_statistic > f_critical) {
  cat("The Chow test indicates that there is a significant structural break in the regression model.\n")
} else {
  cat("The Chow test indicates that there is no significant structural break in the regression model.\n")
}

#AAPL PUTS REGRESSION----------------------------------------------------------------------------------
df<- read_excel("AAPL Puts SSTD.xlsx", col_names = TRUE)

model <- lm(IV_BSM ~ IV_Garch, data = df) #SIMPLE REGRESSION 
summary(model)

# Residuals vs Fitted Plot to check for nonlinearity
plot(model$fitted.values, residuals(model),
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

model <- lm(IV_BSM ~ log(IV_Garch), data = df) #LOG TRANSFORMATION 
summary(model)

model_poly<-lm(IV_BSM ~ poly(IV_Garch, 2, raw = TRUE), data = df) #QUADRATIC
summary(model_poly)

model <- glm(IV_BSM ~ IV_Garch, data = df) #GLM
summary(model)

#AAPL PUTS+VIX REGRESSION----------------------------------------------------------------------------------
df<- read_excel("AAPLVIX SSTD.xlsx", col_names = TRUE)

model <- lm(IV_BSM ~ IV_Garch + VIX, data = df) #SIMPLE REGRESSION 
summary(model)

res_y <- resid(lm(IV_BSM ~ IV_Garch, data = df))
res_x <- resid(lm(VIX ~ IV_Garch, data = df))
plot(res_x, res_y, 
     main = "Partial Regression Plot: VIX vs IV_BSM (controlling for IV_Garch)",
     xlab = "Residuals of VIX ~ IV_Garch", 
     ylab = "Residuals of IV_BSM ~ IV_Garch")
abline(lm(res_y ~ res_x), col = "blue", lty = 2)


model <- lm(IV_BSM ~ log(IV_Garch) + log(VIX), data = df) #LOG TRANSFORMATION
summary(model)

model_poly <- lm(IV_BSM ~ poly(IV_Garch, 2) + poly(VIX, 2), data = df) #QUADRATIC
summary(model_poly)

model <- glm(IV_BSM ~ IV_Garch + VIX, data = df) #GLM
summary(model)

#AAPL PUTS+VIX+CPI REGRESSION-------------------------------------------------------------
puts_df<- read_excel("AAPLVIX+CPI SSTD.xlsx", col_names = TRUE)
lm_put<-lm(IV_BSM~IV_Garch+VIX+CPI, data = puts_df) #SIMPLE 
summary(lm_put)

res_y <- resid(lm(IV_BSM ~ IV_Garch + VIX, data = puts_df))
res_x <- resid(lm(CPI ~ IV_Garch + VIX, data = puts_df))
plot(res_x, res_y,
     main = "Partial Regression Plot: CPI vs IV_BSM (controlling for IV_Garch, VIX)",
     xlab = "Residuals of CPI ~ IV_Garch + VIX",
     ylab = "Residuals of IV_BSM ~ IV_Garch + VIX")
abline(lm(res_y ~ res_x), col = "blue", lty = 2)


#Polynomial
lm_put_poly <- lm(IV_BSM ~ IV_Garch + I(IV_Garch^2) + VIX + I(VIX^2) 
                  + CPI + I(CPI^2), data = puts_df) 
summary(lm_put_poly)

#Log
puts_df$log_IV_Garch <- log(puts_df$IV_Garch)
puts_df$log_VIX <- log(puts_df$VIX)
puts_df$log_CPI <- log(puts_df$CPI)

lm_put_log <- lm(IV_BSM ~ log_IV_Garch + log_VIX + log_CPI, data = puts_df)
summary(lm_put_log)

#GLM
glm_put <- glm(IV_BSM ~ IV_Garch + VIX + CPI,
               data = puts_df,
               family = gaussian(link = "identity"))
summary(glm_put)

#AAPL CALLS TRADING STRATEGY------------------------------------------------
calls_df <- read_excel("AAPL Calls SSTD.xlsx", col_names = TRUE) %>%
  
  arrange(Date) %>%
  mutate(Date = as.Date(Date))

call_model_quad <- lm(IV_BSM ~ IV_Garch , data = calls_df)
calls_df$IV_BSM_hat <- predict(call_model_quad, newdata = calls_df)

threshold <- 0.01
calls_df <- calls_df %>%
  mutate(
    Signal = case_when(
      IV_BSM > IV_BSM_hat + threshold ~ "Sell",
      IV_BSM < IV_BSM_hat - threshold ~ "Buy",
      TRUE ~ "Hold"
    ),
    IV_BSM_next = lead(IV_BSM)
  )

initial_value <- 100000
calls_df <- calls_df %>%
  mutate(
    PnL = case_when(
      Signal == "Buy"  ~ (IV_BSM_next - IV_BSM) * 1000,
      Signal == "Sell" ~ (IV_BSM - IV_BSM_next) * 1000,
      TRUE             ~ 0
    ),
    PnL = replace_na(PnL, 0),
    Portfolio_Value = initial_value + cumsum(PnL),
    Cumulative_Return = (Portfolio_Value - initial_value) / initial_value
  )

calls_df %>%
  select(Date, Signal, IV_BSM, IV_BSM_hat, IV_BSM_next, PnL, Portfolio_Value, Cumulative_Return) %>%
  tail(10)

ggplot(calls_df, aes(x = Date)) +
  geom_line(aes(y = IV_BSM, color = "Actual IV_BSM")) +
  geom_line(aes(y = IV_BSM_hat, color = "Predicted IV_BSM")) +
  geom_point(aes(y = IV_BSM, fill = Signal), shape = 21, size = 3, alpha = 0.7) +
  labs(title = "AAPL Call Options: Actual vs Predicted IV",
       y = "Implied Volatility", color = "Line", fill = "Signal") +
  theme_minimal()

ggplot(calls_df, aes(x = Date, y = Portfolio_Value)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(title = "Portfolio Value from IV Trading Strategy",
       y = "Portfolio Value", x = "Date") +
  theme_minimal()

final_row <- tail(calls_df, 1)
cat("\n========== Strategy Performance ==========\n")
cat("Final Portfolio Value: $", round(final_row$Portfolio_Value, 2), "\n")
cat("Cumulative Return    :", round(final_row$Cumulative_Return * 100, 2), "%\n")






