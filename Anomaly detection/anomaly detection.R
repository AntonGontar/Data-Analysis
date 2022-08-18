install.packages("devtools")
library(devtools)

devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(ggplot2)
remove.packages('AnomalyDetection')

require(devtools) install_version('ggplot2', version = '2.2.1', repos = 'http://cran.us.r-project.org')

##инфо
help(AnomalyDetectionTs)
help(AnomalyDetectionVec)


##пример измененнный
data(raw_data)
raw_data$timestamp <- as.POSIXct(raw_data$timestamp )##корректировка формата
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both',plot=TRUE) 
res$anoms$timestamp <- as.POSIXct(res$anoms$timestamp)
res$plot

ggplot(raw_data, aes(timestamp, count)) + 
  geom_line(data=raw_data, aes(timestamp, count), color='blue') + 
  geom_point(data=res$anoms, aes(timestamp, anoms), color='red')



AnomalyDetectionVec(raw_data[,2], max_anoms=0.02, period=1440, direction='both', only_last=FALSE, plot=TRUE)


res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last='day', plot=TRUE)

raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last='day', plot=TRUE)
res$anoms$timestamp <- as.POSIXct(res$anoms$timestamp)
ggplot(raw_data, aes(timestamp, count)) + 
  geom_line(data=raw_data, aes(timestamp, count), color='blue') + 
  geom_point(data=res$anoms, aes(timestamp, anoms), color='red')
res$plot

res


