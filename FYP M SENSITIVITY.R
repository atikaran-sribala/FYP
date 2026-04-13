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

#1961-1990 training window
ages.fit <- 55:89
years.fit <- 1961:1990
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

aic_bic <- data.frame(
  Model = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
  AIC = c(AIC(LCfit_f), AIC(CBDfit_f), AIC(LCfit_m), AIC(CBDfit_m)),
  BIC = c(BIC(LCfit_f), BIC(CBDfit_f), BIC(LCfit_m), BIC(CBDfit_m))
)
print(aic_bic)

###############
#FULL FORECAST#
###############
LCfor_f <- forecast(LCfit_f, h = 29)
CBDfor_f <- forecast(CBDfit_f, h = 29)
LCfor_m <- forecast(LCfit_m, h = 29)
CBDfor_m <- forecast(CBDfit_m, h = 29)

mxt_f <- EWStMoMofIn$Dxt/EWStMoMofIn$Ext
mxt_m <- EWStMoMomIn$Dxt/EWStMoMomIn$Ext

#actual
amxt_f <- mxt_f[as.character(ages.fit),as.character(years.forecast)]
amxt_m <- mxt_m[as.character(ages.fit),as.character(years.forecast)]

LCfor_f$rates[,as.character(years.forecast)]

#full forecast errors
LC_ferr_f <- log(amxt_f)-log(LCfor_f$rates[,as.character(years.forecast)])
CBD_ferr_f <- log(amxt_f)-log(CBDfor_f$rates[,as.character(years.forecast)])
LC_ferr_m <- log(amxt_m)-log(LCfor_m$rates[,as.character(years.forecast)])
CBD_ferr_m <- log(amxt_m)-log(CBDfor_m$rates[,as.character(years.forecast)])

#OVerall RMSE
RMSE_f_LC_f <-sqrt(mean(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(mean(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(mean(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(mean(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

#full percentage errors (MAPE)
LC_fperr_f <- 100*(abs(LCfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
CBD_fperr_f <- 100*(abs(CBDfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
LC_fperr_m <- 100*(abs(LCfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m
CBD_fperr_m <- 100*(abs(CBDfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m

mean(LC_fperr_f) 
mean(CBD_fperr_f)
mean(LC_fperr_m)
mean(CBD_fperr_m)


#1975-2000 training window
ages.fit <- 55:89
years.fit <- 1975:2000
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

aic_bic <- data.frame(
  Model = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
  AIC = c(AIC(LCfit_f), AIC(CBDfit_f), AIC(LCfit_m), AIC(CBDfit_m)),
  BIC = c(BIC(LCfit_f), BIC(CBDfit_f), BIC(LCfit_m), BIC(CBDfit_m))
)
print(aic_bic)

###############
#FULL FORECAST#
###############
LCfor_f <- forecast(LCfit_f, h = 19)
CBDfor_f <- forecast(CBDfit_f, h = 19)
LCfor_m <- forecast(LCfit_m, h = 19)
CBDfor_m <- forecast(CBDfit_m, h = 19)

mxt_f <- EWStMoMofIn$Dxt/EWStMoMofIn$Ext
mxt_m <- EWStMoMomIn$Dxt/EWStMoMomIn$Ext

#actual
amxt_f <- mxt_f[as.character(ages.fit),as.character(years.forecast)]
amxt_m <- mxt_m[as.character(ages.fit),as.character(years.forecast)]

LCfor_f$rates[,as.character(years.forecast)]

#full forecast errors
LC_ferr_f <- log(amxt_f)-log(LCfor_f$rates[,as.character(years.forecast)])
CBD_ferr_f <- log(amxt_f)-log(CBDfor_f$rates[,as.character(years.forecast)])
LC_ferr_m <- log(amxt_m)-log(LCfor_m$rates[,as.character(years.forecast)])
CBD_ferr_m <- log(amxt_m)-log(CBDfor_m$rates[,as.character(years.forecast)])

#OVerall RMSE
RMSE_f_LC_f <-sqrt(mean(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(mean(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(mean(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(mean(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

#full percentage errors (MAPE)
LC_fperr_f <- 100*(abs(LCfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
CBD_fperr_f <- 100*(abs(CBDfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
LC_fperr_m <- 100*(abs(LCfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m
CBD_fperr_m <- 100*(abs(CBDfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m

mean(LC_fperr_f) 
mean(CBD_fperr_f)
mean(LC_fperr_m)
mean(CBD_fperr_m)


#ages 55-99
ages.fit <- 55:99
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

aic_bic <- data.frame(
  Model = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
  AIC = c(AIC(LCfit_f), AIC(CBDfit_f), AIC(LCfit_m), AIC(CBDfit_m)),
  BIC = c(BIC(LCfit_f), BIC(CBDfit_f), BIC(LCfit_m), BIC(CBDfit_m))
)
print(aic_bic)

dim(LCfor_f$rates[, as.character(years.forecast)])
dim(amxt_f)
rownames(LCfor_f$rates)
rownames(amxt_f)
###############
#FULL FORECAST#
###############
LCfor_f <- forecast(LCfit_f, h = 19)
CBDfor_f <- forecast(CBDfit_f, h = 19)
LCfor_m <- forecast(LCfit_m, h = 19)
CBDfor_m <- forecast(CBDfit_m, h = 19)

mxt_f <- EWStMoMofIn$Dxt/EWStMoMofIn$Ext
mxt_m <- EWStMoMomIn$Dxt/EWStMoMomIn$Ext

#actual
amxt_f <- mxt_f[as.character(ages.fit),as.character(years.forecast)]
amxt_m <- mxt_m[as.character(ages.fit),as.character(years.forecast)]

LCfor_f$rates[,as.character(years.forecast)]

#full forecast errors
LC_ferr_f <- log(amxt_f)-log(LCfor_f$rates[as.character(ages.fit),as.character(years.forecast)])
CBD_ferr_f <- log(amxt_f)-log(CBDfor_f$rates[as.character(ages.fit),as.character(years.forecast)])
LC_ferr_m <- log(amxt_m)-log(LCfor_m$rates[as.character(ages.fit),as.character(years.forecast)])
CBD_ferr_m <- log(amxt_m)-log(CBDfor_m$rates[as.character(ages.fit),as.character(years.forecast)])

#OVerall RMSE
RMSE_f_LC_f <-sqrt(mean(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(mean(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(mean(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(mean(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

# 55-59 = rows 1:5, 60-64 = rows 6:10, etc.
sqrt(mean(LC_ferr_f[1:5,]^2,   na.rm=TRUE))
sqrt(mean(CBD_ferr_f[1:5,]^2,  na.rm=TRUE))
sqrt(mean(LC_ferr_m[1:5,]^2,   na.rm=TRUE))
sqrt(mean(CBD_ferr_m[1:5,]^2,  na.rm=TRUE))

sqrt(mean(LC_ferr_f[6:10,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[6:10,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[6:10,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[6:10,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[11:15,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[11:15,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[11:15,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[11:15,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[16:20,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[16:20,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[16:20,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[16:20,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[21:25,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[21:25,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[21:25,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[21:25,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[26:30,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[26:30,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[26:30,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[26:30,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[31:35,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[31:35,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[31:35,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[31:35,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[36:40,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[36:40,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[36:40,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[36:40,]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[41:45,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[41:45,]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[41:45,]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[41:45,]^2, na.rm=TRUE))

#full percentage errors (MAPE)
LC_fperr_f <- 100*(abs(LCfor_f$rates[as.character(ages.fit),as.character(years.forecast)]-amxt_f))/amxt_f
CBD_fperr_f <- 100*(abs(CBDfor_f$rates[as.character(ages.fit),as.character(years.forecast)]-amxt_f))/amxt_f
LC_fperr_m <- 100*(abs(LCfor_m$rates[as.character(ages.fit),as.character(years.forecast)]-amxt_m))/amxt_m
CBD_fperr_m <- 100*(abs(CBDfor_m$rates[as.character(ages.fit),as.character(years.forecast)]-amxt_m))/amxt_m

mean(LC_fperr_f) 
mean(CBD_fperr_f)
mean(LC_fperr_m)
mean(CBD_fperr_m)

# MAPE by age bucket
mean(LC_fperr_f[1:5,],   na.rm=TRUE)   # 55-59
mean(CBD_fperr_f[1:5,],  na.rm=TRUE)
mean(LC_fperr_m[1:5,],   na.rm=TRUE)
mean(CBD_fperr_m[1:5,],  na.rm=TRUE)

mean(LC_fperr_f[6:10,],  na.rm=TRUE)   # 60-64
mean(CBD_fperr_f[6:10,], na.rm=TRUE)
mean(LC_fperr_m[6:10,],  na.rm=TRUE)
mean(CBD_fperr_m[6:10,], na.rm=TRUE)

mean(LC_fperr_f[11:15,], na.rm=TRUE)   # 65-69
mean(CBD_fperr_f[11:15,],na.rm=TRUE)
mean(LC_fperr_m[11:15,], na.rm=TRUE)
mean(CBD_fperr_m[11:15,],na.rm=TRUE)

mean(LC_fperr_f[16:20,], na.rm=TRUE)   # 70-74
mean(CBD_fperr_f[16:20,],na.rm=TRUE)
mean(LC_fperr_m[16:20,], na.rm=TRUE)
mean(CBD_fperr_m[16:20,],na.rm=TRUE)

mean(LC_fperr_f[21:25,], na.rm=TRUE)   # 75-79
mean(CBD_fperr_f[21:25,],na.rm=TRUE)
mean(LC_fperr_m[21:25,], na.rm=TRUE)
mean(CBD_fperr_m[21:25,],na.rm=TRUE)

mean(LC_fperr_f[26:30,], na.rm=TRUE)   # 80-84
mean(CBD_fperr_f[26:30,],na.rm=TRUE)
mean(LC_fperr_m[26:30,], na.rm=TRUE)
mean(CBD_fperr_m[26:30,],na.rm=TRUE)

mean(LC_fperr_f[31:35,], na.rm=TRUE)   # 85-89
mean(CBD_fperr_f[31:35,],na.rm=TRUE)
mean(LC_fperr_m[31:35,], na.rm=TRUE)
mean(CBD_fperr_m[31:35,],na.rm=TRUE)

mean(LC_fperr_f[36:40,], na.rm=TRUE)   # 90-94
mean(CBD_fperr_f[36:40,],na.rm=TRUE)
mean(LC_fperr_m[36:40,], na.rm=TRUE)
mean(CBD_fperr_m[36:40,],na.rm=TRUE)

mean(LC_fperr_f[41:45,], na.rm=TRUE)   # 95-99
mean(CBD_fperr_f[41:45,],na.rm=TRUE)
mean(LC_fperr_m[41:45,], na.rm=TRUE)
mean(CBD_fperr_m[41:45,],na.rm=TRUE)

dim(CBDfor_f$rates)
dim(amxt_f)
rownames(CBDfor_f$rates)
rownames(amxt_f)
colnames(CBDfor_f$rates)


#remaining life expectancies from age 65
#female: 21 years; males: 19 years
ages.fit_f <- 65:86
ages.fit_m <- 65:84
years.fit <- 1961:2000
#years.forecast_f <- 2001:2021
#years.forecast_m <- 2001:2019

wxt_f <- genWeightMat(ages = ages.fit_f,
                    years = years.fit,
                    clip = 3)
wxt_m <- genWeightMat(ages = ages.fit_m,
                      years = years.fit,
                      clip = 3)

EWStMoMofIn <- central2initial(EWStMoMof)
LCfit_f <- fit (LC, data = EWStMoMofIn, ages.fit = ages.fit_f, years.fit = years.fit, wxt = wxt_f)
CBDfit_f <- fit (CBD, data = EWStMoMofIn, ages.fit = ages.fit_f, years.fit = years.fit, wxt = wxt_f)

EWStMoMomIn <- central2initial(EWStMoMom)
LCfit_m <- fit (LC, data = EWStMoMomIn, ages.fit = ages.fit_m, years.fit = years.fit, wxt = wxt_m)
CBDfit_m <- fit (CBD, data = EWStMoMomIn, ages.fit = ages.fit_m, years.fit = years.fit, wxt = wxt_m)

aic_bic <- data.frame(
  Model = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
  AIC = c(AIC(LCfit_f), AIC(CBDfit_f), AIC(LCfit_m), AIC(CBDfit_m)),
  BIC = c(BIC(LCfit_f), BIC(CBDfit_f), BIC(LCfit_m), BIC(CBDfit_m))
)
print(aic_bic)

###############
#FULL FORECAST#
###############
LCfor_f <- forecast(LCfit_f, h = 19)
CBDfor_f <- forecast(CBDfit_f, h = 19)
LCfor_m <- forecast(LCfit_m, h = 19)
CBDfor_m <- forecast(CBDfit_m, h = 19)

mxt_f <- EWStMoMofIn$Dxt/EWStMoMofIn$Ext
mxt_m <- EWStMoMomIn$Dxt/EWStMoMomIn$Ext

#actual
amxt_f <- mxt_f[as.character(ages.fit_f),as.character(years.forecast)]
amxt_m <- mxt_m[as.character(ages.fit_m),as.character(years.forecast)]

LCfor_f$rates[,as.character(years.forecast)]
LCfor_m$rates[,as.character(years.forecast)]

#full forecast errors
LC_ferr_f <- log(amxt_f)-log(LCfor_f$rates[,as.character(years.forecast)])
CBD_ferr_f <- log(amxt_f)-log(CBDfor_f$rates[,as.character(years.forecast)])
LC_ferr_m <- log(amxt_m)-log(LCfor_m$rates[,as.character(years.forecast)])
CBD_ferr_m <- log(amxt_m)-log(CBDfor_m$rates[,as.character(years.forecast)])

#OVerall RMSE
RMSE_f_LC_f <-sqrt(mean(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(mean(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(mean(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(mean(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

#full percentage errors (MAPE)
LC_fperr_f <- 100*(abs(LCfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
CBD_fperr_f <- 100*(abs(CBDfor_f$rates[,as.character(years.forecast)]-amxt_f))/amxt_f
LC_fperr_m <- 100*(abs(LCfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m
CBD_fperr_m <- 100*(abs(CBDfor_m$rates[,as.character(years.forecast)]-amxt_m))/amxt_m

mean(LC_fperr_f) 
mean(CBD_fperr_f)
mean(LC_fperr_m)
mean(CBD_fperr_m)
