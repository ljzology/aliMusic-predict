artist1 <- sqldf("select * from artist_song_play where artist_id = '023406156015ef87f99521f3b343f71f'")
artist1_sum <- sqldf("select gmt_create, sum(act_type) from artist1 group by gmt_create")
data<-xts(artist1_sum[,2],seq(as.POSIXct("2015-03-01"),len=length(artist1_sum[,2]),by="day"))
plot(data)
test=auto.arima(data)   #auto pipei
data_diff1<-diff(data,differences = 1)
data_diff2<-diff(data,differences = 2)
data_diff3<-diff(data,differences = 3)
plot(data_diff1)
plot(data_diff2)
plot(data_diff3)
acf=acf(coredata(data_diff1[-c(1),]),lag.max = 90,plot = FALSE)  #lag.max = 99  max lag 
pacf=pacf(coredata(data_diff1[-c(1),]),lag.max = 150,plot = FALSE)
acf=acf(coredata(data_diff2[-c(1,2),]),lag.max = 90,plot = FALSE)  #lag.max = 99  max lag 
pacf=pacf(coredata(data_diff2[-c(1,2),]),lag.max = 150,plot = FALSE)
plot(acf)
plot(pacf)
data.fit <- arima(data,order=c(1,1,1), seasonal=list(order=c(1,1,0), period=30))
forecast <- forecast.Arima(data.fit,h=60,level=c(99.5))
plot.forecast(forecast)
plays <- forecast$mean
write.table(plays , "/home/ndscbigdata/workspace/ljz/almusic/forecast_artist3.csv")
