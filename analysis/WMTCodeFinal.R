#WMT Preliminary Analysis

library(fGarch)
library(readxl)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(tidyr)

my_data<-read_excel("WMTDATA.xlsx", col_names=TRUE)
my_data$Date<-as.Date(my_data$Date)
xts_data<-xts(my_data$AdjClose, order.by=my_data$Date)
colnames(xts_data)<-"AdjClose"

log_returns<-Return.calculate(xts_data, method="log")
log_returns<-na.omit(log_returns)
log_returns

acf(log_returns)
acf(log_returns^2)

Box.test(log_returns^2,lag=10,type="Ljung")
pacf(log_returns^2,lag=10, main="WMT Squared Log Returns")

model_1=garchFit(~garch(1,0),data=log_returns)
summary(model_1)

model_2=garchFit(~garch(1,0),data=log_returns,trace=F,cond.dist=c("std"))
summary(model_2)

model_3=garchFit(~garch(1,0),data=log_returns,trace=F,cond.dist=c("sstd"))
summary(model_3)

get_residuals<-residuals(model_3,standardize=T)
qqplot(rsstd(5000,0,1,6.74,0.836),get_residuals)
qqline(get_residuals)

model_4=garchFit(~1+garch(1,1), data=log_returns,trace=F)
summary(model_4)

conditional_stds=model_4@sigma.t
residuals=residuals(model_4, standardize=T)

vol_WMT=ts(conditional_stds, frequency=252, start=c(2022,1))
res_WMT=ts(residuals, frequency=252, start=c(2022,1))
plot(vol_WMT,xlab='year',ylab='volatility',type='l')
plot(res_WMT,xlab='year',ylab='residuals',type='l')

par(mfcol=c(2,2))
acf(residuals, lag=24)
pacf(residuals, lag=24)
acf(residuals^2, lag=24)
pacf(residuals^2, lag=24)

model_5=garchFit(~1+garch(1,1), data=log_returns,trace=F,cond.dist='std')
summary(model_5)

model_6=garchFit(~1+garch(1,1), data=log_returns,trace=F,cond.dist='sstd')
summary(model_6)

resid_model6 <- residuals(model_6, standardize=TRUE)
par(mfrow=c(2,2), mar=c(4,4,4,1))

acf(resid_model6, main="WMT Model 6: ACF of Residuals")
pacf(resid_model6, main="WMT Model 6: PACF of Residuals")

acf(resid_model6^2, main="WMT Model 6: ACF of Squared Residuals")
pacf(resid_model6^2, main="WMT Model 6: PACF of Squared Residuals")

conditional_stds5=model_5@sigma.t
residuals5=residuals(model_5, standardize=T)
vol_WMT5=ts(conditional_stds5, frequency=252, start=c(2022,1))
res_WMT5=ts(residuals5, frequency=252, start=c(2022,1))
plot(vol_WMT5,xlab='year',ylab='volatility',type='l')
plot(res_WMT5,xlab='year',ylab='residuals',type='l')

conditional_stds6=model_6@sigma.t
residuals6=residuals(model_6, standardize=T)
vol_WMT6=ts(conditional_stds6, frequency=252, start=c(2022,1))
res_WMT6=ts(residuals6, frequency=252, start=c(2022,1))
plot(vol_WMT6,xlab='year',ylab='volatility',type='l')
plot(res_WMT6,xlab='year',ylab='residuals',type='l')

model_list<-list(model_4, model_5, model_6)
model_names<-c("model_4", "model_5", "model_6")

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
       main=paste("WMT Log Returns with ±2σ GARCH Bands (", model_name, ")", sep=""),
       ylim=range(c(log_returns, upp, low)))
  lines(tdx, upp, col="red", lty=2)
  lines(tdx, low, col="red", lty=2)
  abline(h=mu, col="blue", lty=3)
}

x<-as.numeric(tail(log_returns,1))
mu<-mean(log_returns)
at<-x-mu
last_vol<-tail(conditional_stds6,1)
omega<-2.312e-06
alpha1<- 4.125e-02
beta1<-9.513e-01
f<-omega+alpha1*at^2+beta1*(last_vol)^2
sqrt(f)

predict(model_6,5)

#Forecasting WMT
df<-read_excel("WMTDATA.xlsx", col_names=TRUE)

# Convert Date to Date type
df$Date<-as.Date(df$Date, format="%Y-%m-%d")
df$LogReturns<-c(NA, diff(log(df$AdjClose), lag=1))
head(df)

target_dates<-c("2024-12-20", "2024-12-27", "2025-01-03", "2025-01-10", "2025-01-17", "2025-01-24", 
                  "2025-01-31", "2025-02-07", "2025-02-14", "2025-02-21", "2025-02-28", "2025-03-07", 
                  "2025-03-14", "2025-03-21", "2025-03-28", "2025-04-04", "2025-04-11")

get_training_data<-function(df, target_date) {
  start_date<-as.Date(target_date)-365*2
  end_date<-as.Date(target_date)-1
  return(subset(df, Date>=start_date&Date<=end_date))
}

predict_volatility<-function(training_data) {
  model<-garchFit(~ garch(1, 1), data=training_data$LogReturns, cond.dist="sstd", trace=FALSE)
  
  forecast<-predict(model, n.ahead=1)

  return(forecast$standardDeviation)
}

volatility_results<-data.frame(
  TargetDate=character(),
  PredictedVolatility=numeric(),
  stringsAsFactors=FALSE
)

for (date in target_dates) {
  training_data<-get_training_data(df, date)
  predicted_volatility<-predict_volatility(training_data)
  
  volatility_results<-rbind(volatility_results, data.frame(
    TargetDate=date,
    PredictedVolatility=predicted_volatility
  ))
}

print(volatility_results)

#WMT Regression
#WMT Calls
library(dplyr)
library(ggplot2)
library(strucchange)
library(car)
library(lmtest)
library(sandwich)

calls_df<-read_excel("WMTCalls2.xlsx")
puts_df<-read_excel("WMTPuts2.xlsx")

calls_df$Date<-as.Date(calls_df$Date)
puts_df$Date<-as.Date(puts_df$Date)

calls_df<-calls_df %>% arrange(Date)
puts_df<-puts_df %>% arrange(Date)

full_model<-lm(IV_BSM~IV_Garch, data=calls_df)
summary(full_model)

call_model_quad<-lm(IV_BSM~IV_Garch+I(IV_Garch^2), data=calls_df)
summary(call_model_quad)

calls_df$log_IV_Garch<-log(calls_df$IV_Garch)
lm_log<-lm(IV_BSM~log_IV_Garch, data=calls_df)
summary(lm_log)

glm_call<-glm(IV_BSM~IV_Garch, data=calls_df, family=gaussian(link="identity"))
summary(glm_call)

#None of the above models have significant parameters, let's add explanatory variables

df_calls_vix<-read_excel("WMTVIX2.xlsx")

if (!"residuals_vix_only" %in% names(df_calls_vix)) {
  lm_vix_only <- lm(IV_BSM ~ VIX, data = df_calls_vix)
  df_calls_vix$residuals_vix_only <- residuals(lm_vix_only)
}

# Filter out rows with NA
df_plot<-df_calls_vix %>% filter(!is.na(VIX) & !is.na(residuals_vix_only))
print(
  ggplot(df_plot, aes(x=VIX, y=residuals_vix_only)) +
    geom_point(color="steelblue") +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title="Residuals from VIX-Only Model", x="VIX", y="Residuals") +
    theme_minimal()
)
#Not linear

df_calls_vix$Date<-as.Date(df_calls_vix$Date)
df_calls_vix<-df_calls_vix %>% arrange(Date)

lm_model_vix<-lm(IV_BSM~IV_Garch+VIX, data=df_calls_vix)
summary(lm_model_vix)

df_calls_vix<-df_calls_vix %>%
  mutate(
    IV_Garch_sq = IV_Garch^2,
    VIX_sq = VIX^2,
    log_IV_Garch = log(IV_Garch),
    log_VIX = log(VIX)
  )

lm_sq<-lm(IV_BSM~IV_Garch+VIX+IV_Garch_sq+VIX_sq, data=df_calls_vix)
summary(lm_sq)

lm_log<-lm(IV_BSM~log_IV_Garch+log_VIX, data=df_calls_vix)
summary(lm_log)

glm_simple_vix<-glm(IV_BSM~IV_Garch+VIX, data=df_calls_vix, family=gaussian(link="identity"))
summary(glm_simple_vix)

#All these are insignifiant

#Add another explanatory variable CPI
df_calls_cpi<-read_excel("WMTVIX+CPI2.xlsx")
df_calls_cpi$Date<-as.Date(df_calls_cpi$Date)
df_calls_cpi<-df_calls_cpi %>% arrange(Date)

if (!"residuals_cpi_only" %in% names(df_calls_cpi)) {
  lm_cpi_only <- lm(IV_BSM ~ CPI, data=df_calls_cpi)
  df_calls_cpi$residuals_cpi_only <- residuals(lm_cpi_only)
}

df_plot_cpi <- df_calls_cpi %>% filter(!is.na(CPI) & !is.na(residuals_cpi_only))

print(
  ggplot(df_plot_cpi, aes(x = CPI, y = residuals_cpi_only)) +
    geom_point(color = "darkorange3") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals from CPI-Only Model", x = "CPI", y = "Residuals") +
    theme_minimal()
)
#Residuals are not linear 

lm_model_cpi_vix <- lm(IV_BSM ~ IV_Garch + VIX + CPI, data = df_calls_cpi)
summary(lm_model_cpi_vix)

df_calls_cpi <- df_calls_cpi %>%
  mutate(
    IV_Garch_sq = IV_Garch^2,
    VIX_sq = VIX^2,
    CPI_sq = CPI^2,
    log_IV_Garch = log(IV_Garch),
    log_VIX = log(VIX),
    log_CPI = log(CPI)
  )

lm_poly_full<-lm(IV_BSM ~ IV_Garch + VIX + CPI +
                     IV_Garch_sq + VIX_sq + CPI_sq,
                   data = df_calls_cpi)
summary(lm_poly_full)

lm_log_cpi_vix<-lm(IV_BSM ~ log_IV_Garch + log_VIX + log_CPI, data = df_calls_cpi)
summary(lm_log_cpi_vix)

glm_cpi_vix <- glm(IV_BSM ~ IV_Garch + VIX + CPI, data = df_calls_cpi, family = gaussian(link = "identity"))
summary(glm_cpi_vix)
#This regression is not used

#WMT Puts
put_model<-lm(IV_BSM~IV_Garch, data=puts_df)
summary(put_model)
#Not significant

put_model_quad<-lm(IV_BSM~IV_Garch+I(IV_Garch^2), data=puts_df)
summary(put_model_quad)

#Variability in independent variable values, multicollinearity is good.
dwtest(put_model_quad)
bptest(put_model_quad)
sctest(put_model_quad, type="Chow", point=6)

residuals_quad<-residuals(put_model_quad)

residual_sum<-sum(residuals_quad)
print(residual_sum)
shapiro_test_result<-shapiro.test(residuals_quad)
print(shapiro_test_result)

# View result
print(shapiro_test_result)

#Trading Strategies
puts_df<-read_excel("WMTPuts2.xlsx")

puts_df <- puts_df %>%
  arrange(Date) %>%
  mutate(
    Date = as.Date(Date),
    IV_Garch_sq = IV_Garch^2
  )

put_model_quad<-lm(IV_BSM ~ IV_Garch + IV_Garch_sq, data = puts_df)
puts_df$IV_BSM_hat <- predict(put_model_quad, newdata = puts_df)

threshold <- 0.01
puts_df <- puts_df %>%
  mutate(
    Signal = case_when(
      IV_BSM > IV_BSM_hat + threshold ~ "Sell",
      IV_BSM < IV_BSM_hat - threshold ~ "Buy",
      TRUE ~ "Hold"
    ),
    IV_BSM_next = lead(IV_BSM)
  )

initial_value <- 100000
puts_df <- puts_df %>%
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

puts_df %>%
  select(Date, Signal, IV_BSM, IV_BSM_hat, IV_BSM_next, PnL, Portfolio_Value, Cumulative_Return) %>%
  tail(10)

ggplot(puts_df, aes(x = Date)) +
  geom_line(aes(y = IV_BSM, color = "Actual IV_BSM")) +
  geom_line(aes(y = IV_BSM_hat, color = "Predicted IV_BSM")) +
  geom_point(aes(y = IV_BSM, fill = Signal), shape = 21, size = 3, alpha = 0.7) +
  labs(title = "WMT Put Options: Actual vs Predicted IV",
       y = "Implied Volatility", color = "Line", fill = "Signal") +
  theme_minimal()

ggplot(puts_df, aes(x = Date, y = Portfolio_Value)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(title = "Portfolio Value from IV Trading Strategy",
       y = "Portfolio Value", x = "Date") +
  theme_minimal()

final_row <- tail(puts_df, 1)
cat("\n========== Strategy Performance ==========\n")
cat("Final Portfolio Value: $", round(final_row$Portfolio_Value, 2), "\n")
cat("Cumulative Return    :", round(final_row$Cumulative_Return * 100, 2), "%\n")

calls_df_aapl <- read_excel("AAPLCalls2.xlsx") %>%
  mutate(Date = as.Date(Date),
         IV_Garch_sq = IV_Garch^2) %>%
  arrange(Date)

puts_df_wmt <- read_excel("WMTPuts2.xlsx") %>%
  mutate(Date = as.Date(Date),
         IV_Garch_sq = IV_Garch^2) %>%
  arrange(Date)

call_model_aapl <- lm(IV_BSM ~ IV_Garch + IV_Garch_sq, data = calls_df_aapl)
put_model_wmt <- lm(IV_BSM ~ IV_Garch + IV_Garch_sq, data = puts_df_wmt)

threshold <- 0.01
calls_df_aapl <- calls_df_aapl %>%
  mutate(
    IV_BSM_hat = predict(call_model_aapl, newdata = .),
    Signal = case_when(
      IV_BSM > IV_BSM_hat + threshold ~ "Sell",
      IV_BSM < IV_BSM_hat - threshold ~ "Buy",
      TRUE ~ "Hold"
    ),
    IV_BSM_next = lead(IV_BSM),
    PnL = case_when(
      Signal == "Buy" ~ (IV_BSM_next - IV_BSM) * 1000,
      Signal == "Sell" ~ (IV_BSM - IV_BSM_next) * 1000,
      TRUE ~ 0
    )
  )

puts_df_wmt <- puts_df_wmt %>%
  mutate(
    IV_BSM_hat = predict(put_model_wmt, newdata = .),
    Signal = case_when(
      IV_BSM > IV_BSM_hat + threshold ~ "Sell",
      IV_BSM < IV_BSM_hat - threshold ~ "Buy",
      TRUE ~ "Hold"
    ),
    IV_BSM_next = lead(IV_BSM),
    PnL = case_when(
      Signal == "Buy" ~ (IV_BSM_next - IV_BSM) * 1000,
      Signal == "Sell" ~ (IV_BSM - IV_BSM_next) * 1000,
      TRUE ~ 0
    )
  )

portfolio_df <- inner_join(
  calls_df_aapl %>% select(Date, PnL_call = PnL),
  puts_df_wmt %>% select(Date, PnL_put = PnL),
  by = "Date"
) %>%
  mutate(
    Total_PnL = PnL_call + PnL_put,
    Portfolio_Value = 100000 + cumsum(replace_na(Total_PnL, 0)),
    Cumulative_Return = (Portfolio_Value - 100000) / 100000
  )

print(tail(portfolio_df, 10))

ggplot(portfolio_df, aes(x = Date, y = Portfolio_Value)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  labs(title = "Portfolio Value: AAPL Calls + WMT Puts Strategy",
       y = "Portfolio Value ($)", x = "Date") +
  theme_minimal()

final_value <- tail(portfolio_df$Portfolio_Value, 1)
cat("Final Portfolio Value:", round(final_value, 2), "\n")
cat("Total Return (%):", round(100 * tail(portfolio_df$Cumulative_Return, 1), 2), "%\n")
