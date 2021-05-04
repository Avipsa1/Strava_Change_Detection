library(fdasrvf)

######SRVF alignment of Strava data by year#############
#24 hour range for which to calculate SRVF of time series
hourRange <- as.matrix(c(1:24))
#12 month range for which to calculate SRRVF of time series
monthRange <- as.matrix(c(1:12))

#Read hourly means for all years
load("Strava2017-hourly-ridership-stat.Rdata")
load("Strava2018-hourly-ridership-stat.Rdata")

#Read monthly means for all years
load("Strava2017-monthly-ridership-stat.Rdata")
load("Strava2018-monthly-ridership-stat.Rdata")

#Run an SRVF alignment on the extracted PCs for each time scale
#hourly
srvf2017.hourly <- time_warping(t(as.matrix(strava2017.hourly.matrix[,c(2:25)])),hourRange, 
                                method = "mean", MaxItr = 20)
save(srvf2017.hourly, file = "Strava2017_hourly_SRVF_Aligned.Rdata")

srvf2018.hourly <- time_warping(t(as.matrix(strava2018.hourly.matrix[,c(2:25)])),hourRange, 
                                method = "mean", MaxItr = 20)
save(srvf2018.hourly, file = "Strava2018_hourly_SRVF_Aligned.Rdata")

#monthly
srvf2017.monthly <- time_warping(t(as.matrix(strava2017.monthly.matrix[,c(2:13)])),monthRange, 
                                 method = "mean", MaxItr = 20)
save(srvf2017.monthly, file = "Strava2017_monthly_SRVF_Aligned.Rdata")

srvf2018.monthly <- time_warping(t(as.matrix(strava2018.monthly.matrix[,c(2:13)])),monthRange, 
                                 method = "mean", MaxItr = 20)
save(srvf2018.monthly, file = "Strava2018_monthly_SRVF_Aligned.Rdata")

load("Strava2017_hourly_SRVF_Aligned.Rdata")
load("Strava2018_hourly_SRVF_Aligned.Rdata")
load("Strava2017_monthly_SRVF_Aligned.Rdata")
load("Strava2018_monthly_SRVF_Aligned.Rdata")

srvfnorm2017.hourly = round(normalize(srvf2017.hourly$f0),2)
srvfnorm2018.hourly = round(normalize(srvf2018.hourly$f0),2)
srvfnorm2017.monthly = round(normalize(srvf2017.monthly$f0),2)
srvfnorm2018.monthly = round(normalize(srvf2018.monthly$f0),2)

library(heatmaply)
#Plot the original data
png("Hourly-Monthly-original-data-2017-18.png",height = 2150, width = 3250, res = 300)
par(mar = c(6,6,6,6), mfrow = c(2,2))
#Hourly
matplot(srvf2017.hourly$time,srvfnorm2017.hourly,type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "Normalized Hourly Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,24))
matplot(srvf2018.hourly$time,srvfnorm2018.hourly,type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "Normalized Hourly Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,24))

#Monthly
matplot(srvf2017.monthly$time,srvfnorm2017.monthly,type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "Normalized Monthly Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))
matplot(srvf2018.monthly$time,srvfnorm2017.monthly,type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "Normalized Monthly Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))

dev.off()

#Plot the aligned data
png("Hourly-Monthly-aligned-data-2017-18.png",height = 2550, width = 3150, res = 300)
par(mar = c(6,6,6,6), mfrow = c(2,2))
#Hourly
matplot(srvf2017.hourly$time,srvf2017.hourly$fn,type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "SRVF Aligned Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,24))
matplot(srvf2018.hourly$time,srvf2018.hourly$fn,type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "SRVF Aligned Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,24))

#Monthly
matplot(srvf2017.monthly$time,srvf2017.monthly$fn,type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "SRVF Aligned Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))
matplot(srvf2018.monthly$time,srvf2018.monthly$fn,type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "SRVF Aligned Ridership", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))

dev.off()

#Plot mean signature of aligned curves for all 3 years
png("Mean-curves-SRVF-aligned-data-2017-18.png",width = 4050, height = 1800, res = 300)
par(mar = c(6,6,6,6),mfrow = c(1,2))
plot(srvf2017.hourly$time,srvf2017.hourly$fmean,type = "l", lwd = 2, xlab = "Hour of Day (t)", frame = FALSE,
        ylab = "Mean Function (f)", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, col = "red",
     ylim = c(0,1), xlim = c(0,25))
#lines(srvf2016.hourly$time,srvf2017.hourly$fmean,lty = 2, lwd = 2, col = "blue", add = TRUE)
lines(srvf2018.hourly$time,srvf2018.hourly$fmean,lty = 6, lwd = 2, col = "blue", add = TRUE)
legend("topright", legend = c("2017","2018"), lwd = c(2,2), 
       lty = c(1,2), col = c("red", "blue"))

plot(srvf2017.monthly$time,srvf2017.monthly$fmean,type = "l", lwd = 2, xlab = "Month of Year (t)", frame = FALSE,
     ylab = "Mean Function (f)", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, col = "red",
     ylim = c(0,1), xlim = c(0,13))
#lines(srvf2016.monthly$time,srvf2017.monthly$fmean,lty = 2, lwd = 2, col = "blue", add = TRUE)
lines(srvf2018.monthly$time,srvf2018.monthly$fmean,lty = 6, lwd = 2, col = "blue", add = TRUE)
legend("topright", legend = c("2017","2018"), lwd = c(2,2), 
       lty = c(1,2), col = c("red", "blue"))

dev.off()
