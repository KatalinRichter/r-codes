### Settings ###
path <- c('N:/onlinemarketing/Intelligence/XXX') #where are the files located
url <- c( #list of the pages you're pulling the data for
#break ----
  "/polar/",
  "/XXX/",
  "/YYY/",
  
#end ----
  "/ZZZ/"
)

files <- list.files(path)
at_rankings_agg <- NULL

### Aggregation ###
for (i in 1:length(files)){
  if(i==1){
    tmp <- read.csv(paste0(path,files[i]), sep=",")
    Date <- substr(files[i],nchar(files[i])-11,nchar(files[i])-4)
    at_rankings_agg <- cbind(Date, tmp)
  } else {
    tmp <- read.csv(paste0(path,files[i]), sep=",")
    Date <- substr(files[i],nchar(files[i])-11,nchar(files[i])-4)
    at_rankings_agg <- rbind(at_rankings_agg, cbind(Date, tmp))
  }
}


### URL Filter ###
at_rankings_agg$URL <- as.character(at_rankings_agg$URL)
at_rankings_agg$url.match <- rep(1,nrow(at_rankings_agg))

for(i in 1:nrow(at_rankings_agg)){
  match <- is.element(substr(at_rankings_agg$URL[i],23,nchar(at_rankings_agg$URL[i])),url)
  if(match==FALSE){
    at_rankings_agg$url.match[i] <- 0
  }
}

at_rankings_agg <- subset(at_rankings_agg,at_rankings_agg$url.match==1)



### Save Output ###
write.csv(at_rankings_agg,paste0(path,"at_rankings_agg.csv"))



