library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(sf)
library(lubridate)
library(timeDate)
library(stargazer)


clusters4.hourly = read.csv("Change2018_4clusters_with_mean_daily_change.csv")
clusters4.monthly = read.csv("Change2018_4clusters_monthly_with_mean_yearly_change.csv")

load("Strava2018-hourly-ridership-stat.Rdata")
load("Strava2018-monthly-ridership-stat.Rdata")

#Calculate % of segments in each category
#Hourly
round((table(clusters4.hourly$labels)/29757)*100,2)

#Monthly
round((table(clusters4.monthly$labels)/29757)*100,2)

#Plot kmeans hourly and monthly
load("Kmeans-monthly-4clusters-aligned.Rdata")
load("Kmeans_4_clusters_2017_18_hourly-change-aligned.Rdata")

png("Hourly-Monthly-Kmeans-clusters-with-common-y-axis.png",width = 4350, height = 2150, res = 300)
par(mar = c(3,3,3,3), mfrow = c(2,4))
#hourly clusters
matplot(kmeans.hourly$qn[[1]], col = "grey", ylim = c(-0.6,0.6), type = "")
lines(kmeans.hourly$templates.q[,1],col = "red")
matplot(kmeans.hourly$qn[[2]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.hourly$templates.q[,2],col = "blue")
matplot(kmeans.hourly$qn[[3]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.hourly$templates.q[,3],col = "darkgreen")
matplot(kmeans.hourly$qn[[4]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.hourly$templates.q[,4],col = "orange")
#monthly clusters
matplot(kmeans.monthly$qn[[1]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.monthly$templates.q[,1],col = "red")
matplot(kmeans.monthly$qn[[2]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.monthly$templates.q[,2],col = "blue")
matplot(kmeans.monthly$qn[[3]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.monthly$templates.q[,3],col = "darkgreen")
matplot(kmeans.monthly$qn[[4]], col = "grey", ylim = c(-0.6,0.6))
lines(kmeans.monthly$templates.q[,4],col = "orange")

#Assign qualitative names to the categories based on variability in normalized ridership stat
hourly2018.ridership.clust <- merge(strava2018.hourly.matrix, clusters4.hourly, by = "edge_id")
monthly2018.ridership.clust <- merge(strava2018.monthly.matrix, clusters4.monthly, by = "edge_id")

png("Hourly-Monthly-clusters-2018-normalized-ridership.png",height = 1750, width = 4550, res = 300)
par(mar = c(6,6,6,6), mfrow = c(4,2))
#Hourly

par(mfrow = c(2,4))
matplot(as.matrix(c(0:23)),t(hourly2018.ridership.clust[hourly2018.ridership.clust$labels == 1,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "Normalized Hourly Ridership", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,1.5))
matplot(as.matrix(c(0:23)),t(hourly2018.ridership.clust[hourly2018.ridership.clust$labels == 2,c(2:25)])
        ,type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,1.5))
matplot(as.matrix(c(0:23)),t(hourly2018.ridership.clust[hourly2018.ridership.clust$labels == 3,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,1.5))
matplot(as.matrix(c(0:23)),t(hourly2018.ridership.clust[hourly2018.ridership.clust$labels == 4,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,1.5))



#Monthly
matplot(as.matrix(c(0:11)),t(monthly2018.ridership.clust[monthly2018.ridership.clust$labels == 1,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "Normalized Monthly Ridership", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,12), ylim = c(0,1.5))
matplot(as.matrix(c(0:11)),t(monthly2018.ridership.clust[monthly2018.ridership.clust$labels == 2,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, 
        xlim = c(0,12), ylim = c(0,1.5))
matplot(as.matrix(c(0:11)),t(monthly2018.ridership.clust[monthly2018.ridership.clust$labels == 3,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, 
        xlim = c(0,12), ylim = c(0,1.5))
matplot(as.matrix(c(0:11)),t(monthly2018.ridership.clust[monthly2018.ridership.clust$labels == 4,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, 
        xlim = c(0,12), ylim = c(0,1.5))

dev.off()




streets = read_sf("Phoenix_Strava_Edges_2017_2018_annual_change.shp")

streets.merged.k4 = merge(streets,clusters4.hourly, by.x = "ID", by.y = "edge_id", all.x = TRUE)
head(streets.merged.k4)

streets.monthly.k4 = merge(streets,clusters4.monthly, by.x = "ID", by.y = "edge_id", all.x = TRUE)

st_write(streets.merged.k4,"Streets_with_4Change_Clusters.shp", append = FALSE)
st_write(streets.monthly.k4,"Streets_with_4Change_Clusters_monthly.shp", append = FALSE)

#Based on infrastructure data, we will identify the potential
#causes of change in one or two areas with high change density
load("Strava2018-cleaned-hourly-data-Phoenix.Rdata")

change2018.hourly.clusters4 <- merge(hourly2018_phx,clusters4.hourly, by = "edge_id")
change2018.monthly.clusters4 <- merge(hourly2018_phx,clusters4.monthly, by = "edge_id")

#Function to calculate features from hourly data
hourly_ridership_estimate <- function(data)
{
  data$date <- strptime(paste0(data$day," ",data$year, " ",data$hour, " ",data$minute),"%j %Y %H %M")
  data$date_day <- format(data$date, "%Y-%m-%d")
  data$day_of_month <- day(data$date_day)
  
  #Sum Strava activity counts by date for each day
  data.hourly <- aggregate(total_activity_count~edge_id + hour + date_day,data=data,FUN="sum")
  data.hourly[is.na(data.hourly)] <- 0
  data.daily <- aggregate(total_activity_count~edge_id + hour,data=data.hourly,FUN="mean")
  
  # change column names to indicate that it is the calculated mean of Strava activity counts
  names(data.daily) <- c("edge_id", "hour", "mean_activity_count")
  
  data.daily[is.na(data.daily)] <- 0
  
  #create a matrix with Strava counts for each edge at each hour
  unique_edges = unique(data.daily$edge_id)
  # sequence of 24 hours, counting from zero
  unique_hours = seq(0, 23, 1)
  # create a matrix for all edges
  matrix <- data.frame(unique_edges)
  # Initialize the matrix with 0
  for (i in 1:length(unique_hours)) {
    matrix[, i+1] <- 0
  }
  # name the columns with hours
  names(matrix) <- c("edge_id", paste0(unique_hours))
  # populate the matrix with Strava counts
  for (h in unique_hours){
    subs <- subset(data.daily, hour==h)
    un_edges = unique(subs$edge_id)
    for (id in un_edges){
      sub2 <- subset(subs, edge_id == id)
      row1 <- which(matrix$edge_id == id)
      # index + 2:
      # +1 to skip edge_id
      # +1 because we count hours from zero, yet the index starts at 1
      matrix[row1,h+2] <- sub2$mean_activity_count
      }
    }
  return(matrix)
}

#Function to calculate features from monthly data
monthly_ridership_estimate <- function(data)
{
  data$date <- strptime(paste0(data$day," ",data$year, " ",data$hour, " ",data$minute),"%j %Y %H %M")
  data$date_day <- format(data$date, "%Y-%m-%d")
  data$month <- month(data$date_day)
  #Sum Strava activity counts by date for each day
  data.monthly <- aggregate(total_activity_count~edge_id + month,data=data,FUN="mean")
  data.monthly[is.na(data.monthly)] <- 0
  names(data.monthly) <- c("edge_id", "month", "monthly_activity_count")
  
  #create a matrix with Strava counts for each edge at each hour
  unique_edges = unique(data.monthly$edge_id)
  # sequence of 12 months, counting from zero
  unique_time = seq(0, 11, 1)
  
  # create a matrix for all edges
  matrix <- data.frame(unique_edges)
  # Initialize the matrix with 0
  for (i in 1:length(unique_time)) {
    matrix[, i+1] <- 0
  }
  # name the columns with hours
  names(matrix) <- c("edge_id", paste0(unique_time))
  # populate the matrix with Strava counts
  for (m in unique_time){
    subs <- subset(data.monthly, month==m)
    un_edges = unique(subs$edge_id)
    for (id in un_edges){
      sub2 <- subset(subs, edge_id == id)
      row1 <- which(matrix$edge_id == id)
      # index + 2:
      # +1 to skip edge_id
      # +1 because we count hours from zero, yet the index starts at 1
      matrix[row1,m+2] <- sub2$monthly_activity_count
    }
  }
  return(matrix)
}

load('Strava2018-cleaned-hourly-data-Phoenix.Rdata')

strava2018.hourly.matrix1  <- hourly_ridership_estimate(hourly2018_phx)
strava2018.monthly.matrix1 <- monthly_ridership_estimate(hourly2018_phx)

hourly2018.raw.clusters <- merge(strava2018.hourly.matrix1, clusters4.hourly, by = "edge_id")
monthly2018.raw.clusters <- merge(strava2018.monthly.matrix1, clusters4.monthly, by = "edge_id")

save(hourly2018.raw.clusters,file = "Hourly2018-Change_clusters-with-raw-ridership.Rdata")
save(monthly2018.raw.clusters,file = "Monthly2018-Change_clusters-with-raw-ridership.Rdata")


png("Hourly-Monthly-clusters-2018-raw-ridership.png",height = 1750, width = 4550, res = 300)
par(mar = c(6,6,6,6), mfrow = c(2,4))

#Hourly
matplot(as.matrix(c(0:23)),t(hourly2018.raw.clusters[hourly2018.raw.clusters$labels == 1,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "Mean Hourly Ridership", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,300))
matplot(as.matrix(c(0:23)),t(hourly2018.raw.clusters[hourly2018.raw.clusters$labels == 2,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,300))
matplot(as.matrix(c(0:23)),t(hourly2018.raw.clusters[hourly2018.raw.clusters$labels == 3,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,300))
matplot(as.matrix(c(0:23)),t(hourly2018.raw.clusters[hourly2018.raw.clusters$labels == 4,c(2:25)]),
        type = 'l', lwd = 2, xlab = "Hour of Day", frame = FALSE,
        ylab = "", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,24), ylim = c(0,300))



#Monthly
matplot(as.matrix(c(0:11)),t(monthly2018.raw.clusters[monthly2018.raw.clusters$labels == 1,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE,
        ylab = "Mean Monthly Ridership", cex.main = 2, cex.axis = 1.2, 
        cex.lab = 1.5, xlim = c(0,12), ylim = c(0,300))
matplot(as.matrix(c(0:11)),t(monthly2018.raw.clusters[monthly2018.raw.clusters$labels == 2,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE, ylim = c(0,300),
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))
matplot(as.matrix(c(0:11)),t(monthly2018.raw.clusters[monthly2018.raw.clusters$labels == 3,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE, ylim = c(0,300),
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))
matplot(as.matrix(c(0:11)),t(monthly2018.raw.clusters[monthly2018.raw.clusters$labels == 4,c(2:13)]),
        type = 'l', lwd = 2, xlab = "Month of Year", frame = FALSE, ylim = c(0,300),
        ylab = "", cex.main = 2, cex.axis = 1.2, cex.lab = 1.5, xlim = c(0,12))

dev.off()

strava2018.hourly.k4 <- merge(strava2018.hourly.matrix1,change2018.hourly.clusters4, by = "edge_id")
strava2018.monthly.k4 <- merge(strava2018.monthly.matrix1,change2018.monthly.clusters4,by = "edge_id")

head(strava2018.hourly.k4)
head(strava2018.monthly.k4)

#Calculate mean weekday/weekend ridership in each cluster




#Hourly
mean.hourly.k4 <- aggregate(.~labels, data = strava2018.hourly.k4[,c('labels','mean_weekday_count', 
                            'mean_weekend_count' , 'total_daily_count')],FUN = mean)
min.hourly.k4 <- aggregate(.~labels, data = strava2018.hourly.k4[,c('labels','mean_weekday_count', 
                           'mean_weekend_count' , 'total_daily_count')],FUN = min)
max.hourly.k4 <- aggregate(.~labels, data = strava2018.hourly.k4[,c('labels','mean_weekday_count', 
                           'mean_weekend_count' , 'total_daily_count')],FUN = max)

hourly.k4.summary <- rbind(mean.hourly.k4,min.hourly.k4,max.hourly.k4)
hourly.k4.summary$pct_segments <- rep(as.matrix(round((table(streets.monthly.k4$labels)/29757)*100,2),nrow=4),3)
hourly.k4.summary$summary <- c(rep('mean',4),rep('min',4),rep('max',4))
stargazer(hourly.k4.summary,type = "text",summary = FALSE)

#Monthly
mean.monthly.k4 <- aggregate(.~labels, data = strava2018.monthly.k4[,c('labels','mean_weekday_count', 
                             'mean_weekend_count' , 'total_annual_count')],FUN = mean)
min.monthly.k4 <- aggregate(.~labels, data = strava2018.monthly.k4[,c('labels','mean_weekday_count', 
                           'mean_weekend_count' , 'total_annual_count')],FUN = min)
max.monthly.k4 <- aggregate(.~labels, data = strava2018.monthly.k4[,c('labels','mean_weekday_count', 
                            'mean_weekend_count' , 'total_annual_count')],FUN = max)

monthly.k4.summary <- rbind(mean.monthly.k4,min.monthly.k4,max.monthly.k4)
monthly.k4.summary$pct_segments <- rep(as.matrix(round((table(clusters4.monthly$labels)/29757)*100,2),nrow=4),3)
monthly.k4.summary$summary <- c(rep('mean',4),rep('min',4),rep('max',4))
stargazer(monthly.k4.summary,type = "text")

stargazer(strava2018.monthly.k4, type = "text")

#Calculate mean functional change
load("Kmeans-monthly-4clusters-aligned.Rdata")
load("Kmeans_4_clusters_2017_18_hourly-change.Rdata")

round((table(kmeans.hourly$labels)/29757)*100,2)

#Calculate mean functional change as Root-Mean-Squared
sqr.diff.mean.hourly <- apply(kmeans.hourly$templates.q,2,function(x) (x-mean(x))^2)
rms.change.hourly <- round(sqrt(colSums(sqr.diff.mean.hourly)/24),3)
rms.change.hourly

sqr.diff.mean.monthly <- apply(kmeans.monthly$templates.q,2,function(x) (x-mean(x))^2)
rms.change.monthly <- round(sqrt(colSums(sqr.diff.mean.monthly)/24),3)
rms.change.monthly

hourly.crashes = read.csv("Hourly_clusters_with_crash_counts.csv")[,c(1,2,6,7)]
head(crashes)
hourly.exposure = aggregate(.~clusters, data = hourly.crashes[,-c(1)],FUN = "sum")
hourly.exposure$exposure = round(hourly.exposure$Num_Crashes/hourly.exposure$Length_km,3)
hourly.exposure

monthly.crashes = read.csv("Monthly_clusters_with_crash_counts.csv")
head(monthly.crashes)
monthly.exposure = aggregate(.~clusters, data = monthly.crashes[,-c(1)],FUN = "sum")
monthly.exposure$exposure = round(monthly.exposure$Num_crashes/monthly.exposure$Length_km,3)
monthly.exposure
