install.packages("demography")
install.packages("StMoMo")
library(demography)
library(StMoMo)

EWdata <- hmd.mx(country = "GBRTENW",                       # code for total england & wales
                 username = "", 
                 password = "",
                 label = "England & Wales")

EWStMoMof <- StMoMoData(EWdata, series = "female")
EWStMoMom <- StMoMoData(EWdata, series = "male")

LC <- lc(link = "log")
CBD <- cbd()

ages.fit <- 55:89
years.fit <- 1961:2000
years.forecast <- 2001:2019

wxt <- genWeightMat(ages = ages.fit,
                    years = years.fit,
                    clip = 3)

EWStMoMofIn <- central2initial(EWStMoMof)
LCfit_f <- fit (LC, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit, wxt = wxt)
CBDfit_f <- fit (CBD, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit, wxt = wxt)

EWStMoMomIn <- central2initial(EWStMoMom)
LCfit_m <- fit (LC, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit, wxt = wxt)
CBDfit_m <- fit (CBD, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit, wxt = wxt)

final_data_years <- 2000:2018
forecast_years <- final_data_years +1


mxt_f <- EWStMoMofIn$Dxt/EWStMoMofIn$Ext
mxt_m <- EWStMoMomIn$Dxt/EWStMoMomIn$Ext

#Full iteration -female
RMSE_LC_f <- numeric(length(forecast_years))
RMSE_CBD_f <- numeric(length(forecast_years))
MAPE_LC_f <- numeric(length(forecast_years))
MAPE_CBD_f <- numeric(length(forecast_years))

for(i in 1: length(final_data_years)){
  final_data_year <- final_data_years[i]
  forecast_year <- forecast_years[i]
  
  years.fit.i <- (final_data_year-39):final_data_year
  wxt.i <- genWeightMat(ages = ages.fit, years = years.fit.i, clip = 3)
  
  LCfit_f.i <- fit(LC, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i, wxt = wxt.i)
  CBDfit_f.i <- fit(CBD, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i, wxt = wxt.i)
  
  #1yr foercast
  LCfor_f.i <- forecast(LCfit_f.i, h = 1)
  CBDfor_f.i <- forecast(CBDfit_f.i, h = 1)
  
  LC_pred_f <- LCfor_f.i$rates[as.character(ages.fit)]
  CBD_pred_f <- CBDfor_f.i$rates[as.character(ages.fit)]
  
  obs_f <- mxt_f[as.character(ages.fit), as.character(forecast_year)]
  
  #errors
  LC_err_f <- log(obs_f) - log(LC_pred_f)
  CBD_err_f <- log(obs_f) - log(CBD_pred_f)
  
  RMSE_LC_f[i] <- sqrt(mean(LC_err_f^2, na.rm = TRUE))
  RMSE_CBD_f[i] <- sqrt(mean(CBD_err_f^2, na.rm = TRUE))

  MAPE_LC_f[i]  <- 100 * mean(abs(LC_pred_f  - obs_f) / obs_f, na.rm = TRUE)
  MAPE_CBD_f[i] <- 100 * mean(abs(CBD_pred_f - obs_f) / obs_f, na.rm = TRUE)
}

LC_err_f
CBD_err_f
RMSE_LC_f
RMSE_CBD_f


plot(forecast_years, RMSE_LC_f, type="l",
     ylim = range(RMSE_LC_f,RMSE_CBD_f),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Female RMSE",
     xlim = c(2001, 2019),
     xaxt = "n")
axis(1, at = seq(2001, 2019, by = 2))
lines(forecast_years, RMSE_CBD_f, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_f < RMSE_CBD_f)
sum(RMSE_CBD_f < RMSE_LC_f)
sum(MAPE_LC_f  < MAPE_CBD_f)
sum(MAPE_CBD_f < MAPE_LC_f)

mean(RMSE_LC_f)
mean(RMSE_CBD_f)
mean(MAPE_LC_f)
mean(MAPE_CBD_f)

#Full iteration - male
RMSE_LC_m <- numeric(length(forecast_years))
RMSE_CBD_m <- numeric(length(forecast_years))
MAPE_LC_m <- numeric(length(forecast_years))
MAPE_CBD_m <- numeric(length(forecast_years))

for(i in 1: length(final_data_years)){
  final_data_year <- final_data_years[i]
  forecast_year <- forecast_years[i]
  
  years.fit.i <- (final_data_year-39):final_data_year
  wxt.i <- genWeightMat(ages = ages.fit, years = years.fit.i, clip = 3)
  
  LCfit_m.i <- fit(LC, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i, wxt = wxt.i)
  CBDfit_m.i <- fit(CBD, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i, wxt = wxt.i)
  
  #1yr foercast
  LCfor_m.i <- forecast(LCfit_m.i, h = 1)
  CBDfor_m.i <- forecast(CBDfit_m.i, h = 1)
  
  LC_pred_m <- LCfor_m.i$rates[as.character(ages.fit)]
  CBD_pred_m <- CBDfor_m.i$rates[as.character(ages.fit)]
  
  obs_m <- mxt_m[as.character(ages.fit), as.character(forecast_year)]
  
  #errors
  LC_err_m <- log(obs_m) - log(LC_pred_m)
  CBD_err_m <- log(obs_m) - log(CBD_pred_m)
  
  RMSE_LC_m[i] <- sqrt(mean(LC_err_m^2, na.rm = TRUE))
  RMSE_CBD_m[i] <- sqrt(mean(CBD_err_m^2, na.rm = TRUE))
  
  MAPE_LC_m[i]  <- 100 * mean(abs(LC_pred_m  - obs_m) / obs_m, na.rm = TRUE)
  MAPE_CBD_m[i] <- 100 * mean(abs(CBD_pred_m - obs_m) / obs_m, na.rm = TRUE)
}
LC_err_m
CBD_err_m
RMSE_LC_m
RMSE_CBD_m

plot(forecast_years, RMSE_LC_m, type="l",
     ylim = range(RMSE_LC_m,RMSE_CBD_m),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Male RMSE",
     xlim = c(2001, 2019),
     xaxt = "n")
axis(1, at = seq(2001, 2019, by = 2))
lines(forecast_years, RMSE_CBD_m, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_m < RMSE_CBD_m)
sum(RMSE_CBD_m < RMSE_LC_m)
sum(MAPE_LC_m  < MAPE_CBD_m)
sum(MAPE_CBD_m < MAPE_LC_m)

mean(RMSE_LC_m)
mean(RMSE_CBD_m)
mean(MAPE_LC_m)
mean(MAPE_CBD_m)


final_data_years3 <- 2000:2016
forecast_years3 <- final_data_years3 + 3
RMSE_LC_f3 <- numeric(length(final_data_years3))
RMSE_CBD_f3 <- numeric(length(final_data_years3))
MAPE_LC_f3 <- numeric(length(final_data_years3))
MAPE_CBD_f3 <- numeric(length(final_data_years3))

for(i in 1: length(final_data_years3)){
  final_data_year3 <- final_data_years3[i]
  forecast_year3 <- forecast_years3[i]
  
  years.fit.i3 <- (final_data_year3-39):final_data_year3
  wxt.i3 <- genWeightMat(ages = ages.fit, years = years.fit.i3, clip = 3)
  
  LCfit_f.i3 <- fit(LC, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i3, wxt = wxt.i3)
  CBDfit_f.i3 <- fit(CBD, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i3, wxt = wxt.i3)
  
  #3yr forecast
  LCfor_f.i3 <- forecast(LCfit_f.i3, h = 3)
  CBDfor_f.i3 <- forecast(CBDfit_f.i3, h = 3)
  
  LC_pred_f3 <- LCfor_f.i3$rates[as.character(ages.fit),3]
  CBD_pred_f3 <- CBDfor_f.i3$rates[as.character(ages.fit),3]
  
  obs_f <- mxt_f[as.character(ages.fit), as.character(forecast_year3)]
  
  #errors
  LC_err_f3 <- log(obs_f) - log(LC_pred_f3)
  CBD_err_f3 <- log(obs_f) - log(CBD_pred_f3)
  
  RMSE_LC_f3[i] <- sqrt(mean(LC_err_f3^2, na.rm = TRUE))
  RMSE_CBD_f3[i] <- sqrt(mean(CBD_err_f3^2, na.rm = TRUE))
  
  MAPE_LC_f3[i]  <- 100 * mean(abs(LC_pred_f3  - obs_f) / obs_f, na.rm = TRUE)
  MAPE_CBD_f3[i] <- 100 * mean(abs(CBD_pred_f3 - obs_f) / obs_f, na.rm = TRUE)
}

LC_err_f3
CBD_err_f3
RMSE_LC_f3
RMSE_CBD_f3


plot(forecast_years3, RMSE_LC_f3, type="l",
     ylim = range(RMSE_LC_f3,RMSE_CBD_f3),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Female RMSE",
     xlim = c(2003, 2019),
     xaxt = "n")
axis(1, at = seq(2003, 2019, by = 2))
lines(forecast_years3, RMSE_CBD_f3, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_f3 < RMSE_CBD_f3)
sum(RMSE_CBD_f3 < RMSE_LC_f3)
sum(MAPE_LC_f3 < MAPE_CBD_f3)
sum(MAPE_CBD_f3 < MAPE_LC_f3)

mean(RMSE_LC_f3)
mean(RMSE_CBD_f3)
mean(MAPE_LC_f3)
mean(MAPE_CBD_f3)

#Full iteration - male
RMSE_LC_m3 <- numeric(length(final_data_years3))
RMSE_CBD_m3 <- numeric(length(final_data_years3))
MAPE_LC_m3 <- numeric(length(final_data_years3))
MAPE_CBD_m3 <- numeric(length(final_data_years3))

for(i in 1: length(final_data_years3)){
  final_data_year3 <- final_data_years3[i]
  forecast_year3 <- forecast_years3[i]
  
  years.fit.i3 <- (final_data_year3-39):final_data_year3
  wxt.i3 <- genWeightMat(ages = ages.fit, years = years.fit.i3, clip = 3)
  
  LCfit_m.i3 <- fit(LC, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i3, wxt = wxt.i3)
  CBDfit_m.i3 <- fit(CBD, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i3, wxt = wxt.i3)
  
  #3yr forecast
  LCfor_m.i3 <- forecast(LCfit_m.i3, h = 3)
  CBDfor_m.i3 <- forecast(CBDfit_m.i3, h = 3)
  
  LC_pred_m3 <- LCfor_m.i3$rates[as.character(ages.fit),3]
  CBD_pred_m3 <- CBDfor_m.i3$rates[as.character(ages.fit),3]
  
  obs_m <- mxt_m[as.character(ages.fit), as.character(forecast_year3)]
  
  #errors
  LC_err_m3 <- log(obs_m) - log(LC_pred_m3)
  CBD_err_m3 <- log(obs_m) - log(CBD_pred_m3)
  
  RMSE_LC_m3[i] <- sqrt(mean(LC_err_m3^2, na.rm = TRUE))
  RMSE_CBD_m3[i] <- sqrt(mean(CBD_err_m3^2, na.rm = TRUE))
  
  MAPE_LC_m3[i]  <- 100 * mean(abs(LC_pred_m3  - obs_m) / obs_m, na.rm = TRUE)
  MAPE_CBD_m3[i] <- 100 * mean(abs(CBD_pred_m3 - obs_m) / obs_m, na.rm = TRUE)
}
LC_err_m3
CBD_err_m3
RMSE_LC_m3
RMSE_CBD_m3

plot(forecast_years3, RMSE_LC_m3, type="l",
     ylim = range(RMSE_LC_m3,RMSE_CBD_m3),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Male RMSE",
     xlim = c(2003, 2019),
     xaxt = "n")
axis(1, at = seq(2003, 2019, by = 2))
lines(forecast_years3, RMSE_CBD_m3, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_m3 < RMSE_CBD_m3)
sum(RMSE_CBD_m3 < RMSE_LC_m3)
sum(MAPE_LC_m3  < MAPE_CBD_m3)
sum(MAPE_CBD_m3 < MAPE_LC_m3)

mean(RMSE_LC_m3)
mean(RMSE_CBD_m3)
mean(MAPE_LC_m3)
mean(MAPE_CBD_m3)



final_data_years5 <- 2000:2014
forecast_years5 <- final_data_years5 + 5
RMSE_LC_f5 <- numeric(length(final_data_years5))
RMSE_CBD_f5 <- numeric(length(final_data_years5))
MAPE_LC_f5 <- numeric(length(final_data_years5))
MAPE_CBD_f5 <- numeric(length(final_data_years5))

for(i in 1: length(final_data_years5)){
  final_data_year5 <- final_data_years5[i]
  forecast_year5 <- forecast_years5[i]
  
  years.fit.i5 <- (final_data_year5-39):final_data_year5
  wxt.i5 <- genWeightMat(ages = ages.fit, years = years.fit.i5, clip = 3)
  
  LCfit_f.i5 <- fit(LC, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i5, wxt = wxt.i5)
  CBDfit_f.i5 <- fit(CBD, data = EWStMoMofIn, ages.fit = ages.fit, years.fit = years.fit.i5, wxt = wxt.i5)
  
  #5yr forecast
  LCfor_f.i5 <- forecast(LCfit_f.i5, h = 5)
  CBDfor_f.i5 <- forecast(CBDfit_f.i5, h = 5)
  
  LC_pred_f5 <- LCfor_f.i5$rates[as.character(ages.fit),5]
  CBD_pred_f5 <- CBDfor_f.i5$rates[as.character(ages.fit),5]
  
  obs_f <- mxt_f[as.character(ages.fit), as.character(forecast_year5)]
  
  #errors
  LC_err_f5 <- log(obs_f) - log(LC_pred_f5)
  CBD_err_f5 <- log(obs_f) - log(CBD_pred_f5)
  
  RMSE_LC_f5[i] <- sqrt(mean(LC_err_f5^2, na.rm = TRUE))
  RMSE_CBD_f5[i] <- sqrt(mean(CBD_err_f5^2, na.rm = TRUE))
  
  MAPE_LC_f5[i]  <- 100 * mean(abs(LC_pred_f5  - obs_f) / obs_f, na.rm = TRUE)
  MAPE_CBD_f5[i] <- 100 * mean(abs(CBD_pred_f5 - obs_f) / obs_f, na.rm = TRUE)
}

LC_err_f5
CBD_err_f5
RMSE_LC_f5
RMSE_CBD_f5


plot(forecast_years5, RMSE_LC_f5, type="l",
     ylim = range(RMSE_LC_f5,RMSE_CBD_f5),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Female RMSE",
     xlim = c(2005, 2019),
     xaxt = "n")
axis(1, at = seq(2005, 2019, by = 2))
lines(forecast_years5, RMSE_CBD_f5, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_f5 < RMSE_CBD_f5)
sum(RMSE_CBD_f5 < RMSE_LC_f5)
sum(MAPE_LC_f5 < MAPE_CBD_f5)
sum(MAPE_CBD_f5 < MAPE_LC_f5)

mean(RMSE_LC_f5)
mean(RMSE_CBD_f5)
mean(MAPE_LC_f5)
mean(MAPE_CBD_f5)

#Full iteration - male
RMSE_LC_m5 <- numeric(length(final_data_years5))
RMSE_CBD_m5 <- numeric(length(final_data_years5))
MAPE_LC_m5 <- numeric(length(final_data_years5))
MAPE_CBD_m5 <- numeric(length(final_data_years5))

for(i in 1: length(final_data_years5)){
  final_data_year5 <- final_data_years5[i]
  forecast_year5 <- forecast_years5[i]
  
  years.fit.i5 <- (final_data_year5-39):final_data_year5
  wxt.i5 <- genWeightMat(ages = ages.fit, years = years.fit.i5, clip = 3)
  
  LCfit_m.i5 <- fit(LC, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i5, wxt = wxt.i5)
  CBDfit_m.i5 <- fit(CBD, data = EWStMoMomIn, ages.fit = ages.fit, years.fit = years.fit.i5, wxt = wxt.i5)
  
  #5yr forecast
  LCfor_m.i5 <- forecast(LCfit_m.i5, h = 5)
  CBDfor_m.i5 <- forecast(CBDfit_m.i5, h = 5)
  
  LC_pred_m5 <- LCfor_m.i5$rates[as.character(ages.fit),5]
  CBD_pred_m5 <- CBDfor_m.i5$rates[as.character(ages.fit),5]
  
  obs_m <- mxt_m[as.character(ages.fit), as.character(forecast_year5)]
  
  #errors
  LC_err_m5 <- log(obs_m) - log(LC_pred_m5)
  CBD_err_m5 <- log(obs_m) - log(CBD_pred_m5)
  
  RMSE_LC_m5[i] <- sqrt(mean(LC_err_m5^2, na.rm = TRUE))
  RMSE_CBD_m5[i] <- sqrt(mean(CBD_err_m5^2, na.rm = TRUE))
  
  MAPE_LC_m5[i]  <- 100 * mean(abs(LC_pred_m5  - obs_m) / obs_m, na.rm = TRUE)
  MAPE_CBD_m5[i] <- 100 * mean(abs(CBD_pred_m5 - obs_m) / obs_m, na.rm = TRUE)
}
LC_err_m5
CBD_err_m5
RMSE_LC_m5
RMSE_CBD_m5

plot(forecast_years5, RMSE_LC_m5, type="l",
     ylim = range(RMSE_LC_m5,RMSE_CBD_m5),
     xlab="Forecast year", ylab="RMSE (log scale)",
     main="Male RMSE",
     xlim = c(2005, 2019),
     xaxt = "n")
axis(1, at = seq(2005, 2019, by = 2))
lines(forecast_years5, RMSE_CBD_m5, lty=2)
legend("topleft", legend=c("LC", "CBD"), lty=c(1,2), bty="n")

sum(RMSE_LC_m5 < RMSE_CBD_m5)
sum(RMSE_CBD_m5 < RMSE_LC_m5)
sum(MAPE_LC_m5  < MAPE_CBD_m5)
sum(MAPE_CBD_m5 < MAPE_LC_m5)

mean(RMSE_LC_m5)
mean(RMSE_CBD_m5)
mean(MAPE_LC_m5)
mean(MAPE_CBD_m5)

sum(RMSE_LC_f < RMSE_CBD_f)
sum(RMSE_CBD_f < RMSE_LC_f)
sum(RMSE_LC_f3 < RMSE_CBD_f3)
sum(RMSE_CBD_f3 < RMSE_LC_f3)
sum(RMSE_LC_f5 < RMSE_CBD_f5)
sum(RMSE_CBD_f5 < RMSE_LC_f5)

mean(RMSE_LC_f)
mean(RMSE_CBD_f)
mean(RMSE_LC_f3)
mean(RMSE_CBD_f3)
mean(RMSE_LC_f5)
mean(RMSE_CBD_f5)

sum(RMSE_LC_m < RMSE_CBD_m)
sum(RMSE_CBD_m < RMSE_LC_m)
sum(RMSE_LC_m3 < RMSE_CBD_m3)
sum(RMSE_CBD_m3 < RMSE_LC_m3)
sum(RMSE_LC_m5 < RMSE_CBD_m5)
sum(RMSE_CBD_m5 < RMSE_LC_m5)

mean(RMSE_LC_m)
mean(RMSE_CBD_m)
mean(RMSE_LC_m3)
mean(RMSE_CBD_m3)
mean(RMSE_LC_m5)
mean(RMSE_CBD_m5)

# 1-year horizon
mean(MAPE_LC_f)
mean(MAPE_CBD_f)
sum(MAPE_LC_f < MAPE_CBD_f)
sum(MAPE_CBD_f < MAPE_LC_f)

mean(MAPE_LC_m)
mean(MAPE_CBD_m)
sum(MAPE_LC_m < MAPE_CBD_m)
sum(MAPE_CBD_m < MAPE_LC_m)

# 3-year horizon
mean(MAPE_LC_f3)
mean(MAPE_CBD_f3)
sum(MAPE_LC_f3 < MAPE_CBD_f3)
sum(MAPE_CBD_f3 < MAPE_LC_f3)

mean(MAPE_LC_m3)
mean(MAPE_CBD_m3)
sum(MAPE_LC_m3 < MAPE_CBD_m3)
sum(MAPE_CBD_m3 < MAPE_LC_m3)

# 5-year horizon
mean(MAPE_LC_f5)
mean(MAPE_CBD_f5)
sum(MAPE_LC_f5 < MAPE_CBD_f5)
sum(MAPE_CBD_f5 < MAPE_LC_f5)

mean(MAPE_LC_m5)
mean(MAPE_CBD_m5)
sum(MAPE_LC_m5 < MAPE_CBD_m5)
sum(MAPE_CBD_m5 < MAPE_LC_m5)

# 2x10yr - female
mean(RMSE_LC_f[1:10]); mean(RMSE_CBD_f[1:10])   # 2001-2010
mean(RMSE_LC_f[11:19]); mean(RMSE_CBD_f[11:19]) # 2011-2019

# 4x5yr - female
mean(RMSE_LC_f[1:5]);  mean(RMSE_CBD_f[1:5])    # 2001-2005
mean(RMSE_LC_f[6:10]); mean(RMSE_CBD_f[6:10])   # 2006-2010
mean(RMSE_LC_f[11:15]); mean(RMSE_CBD_f[11:15]) # 2011-2015
mean(RMSE_LC_f[16:19]); mean(RMSE_CBD_f[16:19]) # 2016-2019

# 2x10yr - male
mean(RMSE_LC_m[1:10]); mean(RMSE_CBD_m[1:10])   # 2001-2010
mean(RMSE_LC_m[11:19]); mean(RMSE_CBD_m[11:19]) # 2011-2019

# 4x5yr - male
mean(RMSE_LC_m[1:5]);  mean(RMSE_CBD_m[1:5])    # 2001-2005
mean(RMSE_LC_m[6:10]); mean(RMSE_CBD_m[6:10])   # 2006-2010
mean(RMSE_LC_m[11:15]); mean(RMSE_CBD_m[11:15]) # 2011-2015
mean(RMSE_LC_m[16:19]); mean(RMSE_CBD_m[16:19]) # 2016-2019
