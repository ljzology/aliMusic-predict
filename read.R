library(DBI)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(forecast)
library(xts)
library(tcltk)

setwd("/home/ndscbigdata/workspace/ljz/almusic/") #设置工作区间  
#dir()
user_table = read.csv("mars_tianchi_user_actions.csv",header = F, 
                      sep = ",",na.strings = "NA",stringsAsFactors = FALSE,
                      blank.lines.skip = TRUE) #读取csv文件  
user_table <- data.frame(user_id=user_table[,1],song_id=user_table[,2],
                          gmt_create=user_table[,3],act_type=user_table[,4],
                          Ds=user_table[,5])
song_table<-read.csv("mars_tianchi_songs.csv",header = F, 
                     sep = ",",na.strings = "NA",stringsAsFactors = FALSE,
                     blank.lines.skip = TRUE)
song_table <- data.frame(song_id=song_table[,1],artist_id=song_table[,2],
                         puglist_time=song_table[,3],song_init_plays=song_table[,4],
                         language=song_table[,5],gender=song_table[,6])
#head(user_table)    #输出向量data4中的内容  
#View(ccc)
user_table[,3] <- as.Date(as.POSIXct(user_table[,3], origin="1970-01-01"))
user_table[,3] = format(user_table[,3],"%Y-%m-%d")

#write.csv(user_table , "/home/ndscbigdata/workspace/ljz/almusic/user_time.csv")

#-----------------groupby--------------------

artist_song_play <- sqldf("select t.artist_id, s.user_id, s.song_id, s.gmt_create, s.act_type from song_table t 
                          join user_table s on t.song_id = s.song_id")
#-----------------delete 2015-02-28-------------
artist_song_play <- sqldf("select * from artist_song_play where gmt_create != '2015-02-28'")
artist_list <- sqldf("select artist_id, sum(act_type) from artist_song_play t group by t.artist_id")
#write.csv(artist_list , "/home/ndscbigdata/workspace/ljz/almusic/artist_list.csv")
sqldf("select count(*) from artist_list")
artist1 <- sqldf("select * from artist_song_play where artist_id = '28e32be6ba67e0c6fdfdff80ce07dfd4'")
artist1_sum <- sqldf("select gmt_create, sum(act_type) from artist1 group by gmt_create")

#------------------forecast-----------------------

data<-xts(artist1_sum[,2],seq(as.POSIXct("2015-03-01"),len=length(artist1_sum[,2]),by="day"))
#data<-data.frame(play_time=artist1_sum[,1],act_counts=artist1_sum[,2])
head(data)
plot(data)
data_diff1<-diff(data,differences = 1)
data_diff2<-diff(data,differences = 2)
data_diff3<-diff(data,differences = 3)
plot(data_diff1)
plot(data_diff2)
plot(data_diff3)
#data_diff2[-2,]
acf=acf(coredata(data_diff1[-c(1),]),lag.max = 90,plot = FALSE)  #lag.max = 99  max lag 
pacf=pacf(coredata(data_diff1[-c(1),]),lag.max = 150,plot = FALSE)
plot(acf)
plot(pacf)
data.fit <- arima(data,order=c(1,1,0), seasonal=list(order=c(1,1,0), period=30))

#forecast 7 days's date
forecast <- forecast.Arima(data.fit,h=60,level=c(99.5))
#write.csv(artist_list , "/home/ndscbigdata/workspace/ljz/almusic/forecast_artist_list.csv",append=TRUE)
#-----------install.packages("readr")-----
#forecast
#par(new=TRUE)
#par(mfrow=c(1,2))
plot.forecast(forecast)
plays <- forecast$mean
write.table(plays , "/home/ndscbigdata/workspace/ljz/almusic/forecast_artist3.csv",append=TRUE)

