submit_table = read.csv("forecast_artist3.csv",header = F, 
                        sep = ",",na.strings = "NA",stringsAsFactors = FALSE,
                        blank.lines.skip = TRUE) #读取csv文件 
is.na(submit_table[,1])
na.omit(submit_table)  #delete na rows
write.csv(na.omit(submit_table) , "/home/ndscbigdata/workspace/ljz/almusic/submit_table_v2.csv")
#--------------------qu zheng --------------
submit_table[,1] <- round(submit_table[,1])
#--------------------qu abstract------------
submit_table[,1] <- abs(submit_table[,1])

