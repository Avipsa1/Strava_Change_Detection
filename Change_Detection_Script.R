# load packages
library(timeDate)
library(fdasrvf)
library(tidyverse)
library(cluster)
library(factoextra)
library(fdakma)


#24 hour range for which to calculate SRVF of time series
hourRange <- as.matrix(c(1:24))
#12 month range for which to calculate SRRVF of time series
monthRange <- as.matrix(c(1:12))


#Load SRVF-aligned ridership in 2017 and 2018
load("Strava2017_hourly_SRVF_Aligned.Rdata")
load("Strava2018_hourly_SRVF_Aligned.Rdata")
load("Strava2017_monthly_SRVF_Aligned.Rdata")
load("Strava2018_monthly_SRVF_Aligned.Rdata")

#Calculate change in ridership from the mean signature 
change2018.hourly <- apply(srvf2018.hourly$fn, 2, function(x) x - srvf2017.hourly$fmean)
change2018.monthly <- apply(srvf2018.monthly$fn, 2, function(x) x - srvf2017.monthly$fmean)

#Check optimal k for kMeans using gap statistic


png("K-Means_elbow_plot-hourly-monthly.png",width = 3550,height=2550,res=300)
par(mar=c(6,6,6,6),mfrow = c(1,2))
fviz_nbclust(change2018.hourly,FUNcluster = kmeans,method = "gap_stat",
             linecolor = "red", barfill = "blue")
fviz_nbclust(change2018.monthly,FUNcluster = kmeans,method = "gap_stat",
             linecolor = "red", barfill = "blue")
dev.off()

#Run K-means clustering on the hourly and monthly change to group streets into 4 categories 

kmeans.hourly= kmeans_align(change2018.hourly,hourRange,K = 4,MaxItr = 20)
paste(round((table(kmeans.hourly$labels)/29757)*100),2),'%')
kmeans.monthly = kmeans_align(change2018.monthly,monthRange,K = 4)

save(kmeans.hourly,file = "Kmeans_4_clusters_2017_18_hourly-change.Rdata")
save(kmeans.monthly,file = "Kmeans_4_clusters_2017_18_monthly-change.Rdata")


kmeans.monthly= kmeans_align(change2018.monthly,monthRange,K = 4, alignment = T, MaxItr = 20)
save(kmeans.monthly,file = "Kmeans_4_clusters_2017_18_monthly-change.Rdata")


load("Strava2018-hourly-ridership-stat.Rdata")

strava2018.streets = data.frame(strava2018.hourly.matrix$edge_id,kmeansly$labels)
#plot the clusters
par(mar=c(6,6,6,6), mfrow = c(2,4))
plot(kmeans.hourly)
plot(kmeans.monthly)

change2018.clusters4 <- data.frame(edge_id = strava2018.hourly.matrix$edge_id,labels = kmeans.hourly$labels)
clusters4.info <- data.frame(labels = c(1,2,3,4), mean_change_daily = unlist(lapply(kmeans.hourly$fn,mean)))
change2018.clusters4 <- merge(change2018.clusters4, clusters4.info, by = "labels")
head(change2018.clusters4)

write.csv(change2018.clusters4,"Change2018_4clusters_with_mean_daily_change.csv")

change2018.clusters7 <- data.frame(edge_id = strava2018.hourly.matrix$edge_id,labels = kmeans.hourly.7k$labels)
clusters7.info <- data.frame(labels = c(1,2,3,4,5,6,7), mean_change_daily = unlist(lapply(kmeans.hourly.7k$fn,mean)))
change2018.clusters7 <- merge(change2018.clusters7, clusters7.info, by = "labels")
head(change2018.clusters7)

write.csv(change2018.clusters7,"Change2018_7clusters_with_mean_daily_change.csv")

#Calculate mean functional change as Root-Mean-Squared
sqr.diff.mean.hourly <- apply(kmeans.hourly$templates.q,2,function(x) (x-mean(x))^2)
rms.change.hourly <- round(sqrt(colSums(sqr.diff.mean.hourly)/24),3)
rms.change.hourly

paste(round((table(kmeans.hourly$labels)/29757)*100,2),'%')

sqr.diff.mean.monthly <- apply(kmeans.monthly$templates.q,2,function(x) (x-mean(x))^2)
rms.change.monthly <- round(sqrt(colSums(sqr.diff.mean.monthly)/24),3)
rms.change.monthly

paste(round((table(kmeans.monthly$labels)/29757)*100,2),'%')


change2018.clusters4.monthly <- data.frame(edge_id = strava2018.hourly.matrix$edge_id,
                                           labels = kmeans.monthly$labels)
clusters4.info <- data.frame(labels = c(1,2,3,4), mean_change_annual = unlist(lapply(kmeans.monthly$fn,mean)))
change2018.clusters4 <- merge(change2018.clusters4.monthly, clusters4.info, by = "labels")
head(change2018.clusters4)

write.csv(change2018.clusters4,"Change2018_4clusters_monthly_with_mean_yearly_change.csv")
save(kmeans.monthly,file="Kmeans-monthly-4clusters-aligned.Rdata")
