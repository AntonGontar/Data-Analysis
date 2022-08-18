
library(CausalImpact)
library("sqldf")
library("googleAnalyticsR")
library("plotly")


file_x <- results_20211125_095840_results_20211125_095840
file_y <- file_x[!is.na(file_x$X2), ]
file_y


# Запрашиваем данные из GA
ga_auth()


account_list <- ga_account_list()
account_list

ga_id <- 0000000000


all_session <-google_analytics(ga_id, 
                                  date_range = c("2021-10-01", "2021-11-22"), 
                                  metrics = c("sessions"), 
                                  dimensions = c("date"))

all_session$date <- gsub('-', '', all_session$date)
all_session

xall_file <- merge(x = all_session, y = file_y, by = "date", all.x = TRUE)
xall_file$rate <-(xall_file$X2/xall_file$sessions)*100
xall_file$rate[is.na(xall_file$rate)] <-  0
xall_file$X2[is.na(xall_file$X2)] <-  0

xall_file

#############################################################################################
# Создаем вектор значений
time.points <- seq.Date(as.Date("2021-10-01"), by = 1, length.out = 45)
mobile_data <- zoo(cbind(xall_file$rate), time.points)



# Периоды до и после даты события
pre.period <- as.Date(c("2021-10-01", "2021-10-26"))
post.period <- as.Date(c("2021-10-27", "2021-11-14"))

# Подключение пакета (есть сезонность)
impact <- CausalImpact(mobile_data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
plot(impact)
summary(impact)
summary(impact, "report")

#############################################################################################
time.points <- seq.Date(as.Date("2021-10-01"), by = 1, length.out = 45)
desktop_data <- zoo(cbind(desktop$q), time.points)



# Периоды до и после даты события
pre.period <- as.Date(c("2021-10-01", "2021-10-26"))
post.period <- as.Date(c("2021-10-27", "2021-11-14"))

# Подключение пакета (есть сезонность)
impact <- CausalImpact(desktop_data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
plot(impact)
summary(impact)
summary(impact, "report")


