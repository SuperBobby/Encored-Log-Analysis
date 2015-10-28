

options(digits=15)
options('digits')

get.milisec = function(length){
        add_milisec = seq(0, 1000, length.out = length)
        return(add_milisec)
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

# table(consecutive_counting)


adding_milisec = sapply(X = consecutive_counting, FUN = get.milisec)
adding_milisec = unlist(adding_milisec)

head(adding_milisec, 16)
head(data$unix_timestamp_with_milisec, 16)
