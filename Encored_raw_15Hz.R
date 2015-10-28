
options(digits=15)
options('digits')

# 밀리초 감안하여 시간 배분

get.milisec = function(length){
        add_milisec = seq(0, 1000, length.out = (length+1))
        return(add_milisec[1:(length(add_milisec)-1)])
}

data = read.csv("2015-08-15.csv")
summary(data)
str(data)

as_unix_timestamp = as.numeric(as.POSIXlt(as.character(data$timestamp), origin="1970-01-01",tz="GMT"))
unix_timestamp_with_milisec = as_unix_timestamp * 1000
data = cbind(data, unix_timestamp_with_milisec)
# sorting
data = data[order(data$unix_timestamp_with_milisec),]

# # sorting test
# if(names(table(data$unix_timestamp_with_milisec - unix_timestamp_with_milisec)) == 0){
#         print("no problem")
# }

consecutive_counting = rle(data$unix_timestamp_with_milisec)$lengths

adding_milisec = lapply(X = consecutive_counting, FUN = get.milisec)
adding_milisec = unlist(adding_milisec)
adding_milisec = round(adding_milisec)

data$unix_timestamp_with_milisec = data$unix_timestamp_with_milisec + adding_milisec

summary(data)
str(data)



# 초단위 데이터 테이블로 변환

ranged.cumsum = function(i, index, data){
        return(sum(data$active_power[index[i]:(index[i+1]-1)]))
}


index = cumsum(c(1,consecutive_counting))
sec_timestamp = as.character(data$timestamp[index])
sec_timestamp = sec_timestamp[-length(sec_timestamp)]

sec_active_power = sapply(X = 1:(length(index)-1), FUN = ranged.cumsum, index=index, data=data)

# sec_active_power = numeric(0)
# 
# for(i in 1:(length(index)-1)){
#         sec_active_power = c(sec_active_power, sum(data$active_power[index[i]:(index[i]+consecutive_counting[i]-1)]))
# }

sec_table = data.frame(sec_timestamp, sec_active_power)

summary(sec_table)
str(sec_table)
