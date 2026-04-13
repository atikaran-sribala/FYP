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

aic_bic <- data.frame(
  Model = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
  AIC = c(AIC(LCfit_f), AIC(CBDfit_f), AIC(LCfit_m), AIC(CBDfit_m)),
  BIC = c(BIC(LCfit_f), BIC(CBDfit_f), BIC(LCfit_m), BIC(CBDfit_m))
)
print(aic_bic)

plot(residuals(LCfit_f), 
     type="signplot",
     main = "LC Female"
)
plot(residuals(CBDfit_f), 
     type="signplot",
     main = "CBD Female"
)
plot(residuals(LCfit_m), 
     type="signplot",
     main = "LC Male"
)
plot(residuals(CBDfit_m), 
     type="signplot",
     main = "CBD Male"
)

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

#full forecast errors
LC_ferr_f <- log(amxt_f)-log(LCfor_f$rates)
CBD_ferr_f <- log(amxt_f)-log(CBDfor_f$rates)
LC_ferr_m <- log(amxt_m)-log(LCfor_m$rates)
CBD_ferr_m <- log(amxt_m)-log(CBDfor_m$rates)

#OVerall RMSE
RMSE_f_LC_f <-sqrt(mean(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(mean(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(mean(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(mean(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

#RMSE by age
RMSE_f_LC_f <-sqrt(rowMeans(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(rowMeans(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(rowMeans(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(rowMeans(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m

#RMSE by year
RMSE_f_LC_f <-sqrt(colMeans(LC_ferr_f^2, na.rm = TRUE))
RMSE_f_CBD_f <-sqrt(colMeans(CBD_ferr_f^2, na.rm = TRUE))
RMSE_f_LC_m <-sqrt(colMeans(LC_ferr_m^2, na.rm = TRUE))
RMSE_f_CBD_m <-sqrt(colMeans(CBD_ferr_m^2, na.rm = TRUE))

RMSE_f_LC_f
RMSE_f_CBD_f
RMSE_f_LC_m
RMSE_f_CBD_m


# Set up the plot
par(mar = c(5, 5, 3, 2))

plot(years.forecast, RMSE_f_LC_f, type = "l", lty = 1, lwd = 2,
     ylim = range(c(RMSE_f_LC_f, RMSE_f_CBD_f, RMSE_f_LC_m, RMSE_f_CBD_m)),
     xlab = "Forecast year",
     ylab = "RMSE (log scale)",
     main = "Full Forecast RMSE by Year",
     col = "darkblue")

lines(years.forecast, RMSE_f_CBD_f, lty = 1, lwd = 2, col = "lightblue")
lines(years.forecast, RMSE_f_LC_m,  lty = 1, lwd = 2, col = "darkred")
lines(years.forecast, RMSE_f_CBD_m, lty = 1, lwd = 2, col = "red")

legend("topleft",
       legend = c("LC Female", "CBD Female", "LC Male", "CBD Male"),
       lty = c(1, 1, 1, 1),
       col = c("darkblue", "lightblue", "darkred", "red"),
       lwd = 2, cex = 0.9, bty = "n")
grid()


#full percentage errors (MAPE)
LC_fperr_f <- 100*(abs(LCfor_f$rates-amxt_f))/amxt_f
CBD_fperr_f <- 100*(abs(CBDfor_f$rates-amxt_f))/amxt_f
LC_fperr_m <- 100*(abs(LCfor_m$rates-amxt_m))/amxt_m
CBD_fperr_m <- 100*(abs(CBDfor_m$rates-amxt_m))/amxt_m

#MAPE by age
rowMeans(LC_fperr_f) 
rowMeans(CBD_fperr_f) 
rowMeans(LC_fperr_m) 
rowMeans(CBD_fperr_m) 

#MAPE by year
colMeans(LC_fperr_f) 
colMeans(CBD_fperr_f) 
colMeans(LC_fperr_m) 
colMeans(CBD_fperr_m)    

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

# 2x10yr - columns 1:10 = 2001-2010, columns 11:19 = 2011-2019
sqrt(mean(LC_ferr_f[,1:10]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,1:10]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,1:10]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,1:10]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[,11:19]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,11:19]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,11:19]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,11:19]^2, na.rm=TRUE))

# 4x5yr
sqrt(mean(LC_ferr_f[,1:5]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,1:5]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,1:5]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,1:5]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[,6:10]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,6:10]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,6:10]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,6:10]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[,11:15]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,11:15]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,11:15]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,11:15]^2, na.rm=TRUE))

sqrt(mean(LC_ferr_f[,16:19]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_f[,16:19]^2, na.rm=TRUE))
sqrt(mean(LC_ferr_m[,16:19]^2,  na.rm=TRUE))
sqrt(mean(CBD_ferr_m[,16:19]^2, na.rm=TRUE))

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

# MAPE by time period (2x10yr)
mean(LC_fperr_f[,1:10],  na.rm=TRUE)   # 2001-2010
mean(CBD_fperr_f[,1:10], na.rm=TRUE)
mean(LC_fperr_m[,1:10],  na.rm=TRUE)
mean(CBD_fperr_m[,1:10], na.rm=TRUE)

mean(LC_fperr_f[,11:19],  na.rm=TRUE)  # 2011-2019
mean(CBD_fperr_f[,11:19], na.rm=TRUE)
mean(LC_fperr_m[,11:19],  na.rm=TRUE)
mean(CBD_fperr_m[,11:19], na.rm=TRUE)

# MAPE by time period (4x5yr)
mean(LC_fperr_f[,1:5],   na.rm=TRUE)   # 2001-2005
mean(CBD_fperr_f[,1:5],  na.rm=TRUE)
mean(LC_fperr_m[,1:5],   na.rm=TRUE)
mean(CBD_fperr_m[,1:5],  na.rm=TRUE)

mean(LC_fperr_f[,6:10],  na.rm=TRUE)   # 2006-2010
mean(CBD_fperr_f[,6:10], na.rm=TRUE)
mean(LC_fperr_m[,6:10],  na.rm=TRUE)
mean(CBD_fperr_m[,6:10], na.rm=TRUE)

mean(LC_fperr_f[,11:15], na.rm=TRUE)   # 2011-2015
mean(CBD_fperr_f[,11:15],na.rm=TRUE)
mean(LC_fperr_m[,11:15], na.rm=TRUE)
mean(CBD_fperr_m[,11:15],na.rm=TRUE)

mean(LC_fperr_f[,16:19], na.rm=TRUE)   # 2016-2019
mean(CBD_fperr_f[,16:19],na.rm=TRUE)
mean(LC_fperr_m[,16:19], na.rm=TRUE)
mean(CBD_fperr_m[,16:19],na.rm=TRUE)
