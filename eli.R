### The Life of Eli ###
### devtools::session_info()
# !!! reinstalls all packages 
### update.packages(ask = FALSE, dependencies = c('Suggests'))

# access files from google drive
# install.packages("googledrive") # get googledrive package
# library(googledrive)
# # http://googledrive.tidyverse.org/
# # common commands: find, ls, mv, cp, mkdir, and rm
# 

# drive_find(n_max=10) # set output limits
# drive_find(type = "folder")
# drive_get("~/Data/eli/feb.csv")

setwd("/Users/malishev/Documents/Data/Eli")
list.files()
d <- "feb" # choose month or total period
data <- read.csv(paste0(d,".csv"),header=T,sep=",", stringsAsFactors=FALSE)
colnames(data) <- c("Activity","Trait","Start","Finish","Measure")
data[c("Activity", "Trait")] <- sapply(data[c("Activity", "Trait")],as.character)
head(data)

#change time vectors from character to posix    
?split
?grepl
?substr
?strsplit 
?nchar

# separate into date, month, and time (by am and pm) cols. remove year. 
#opt 1
ss <- strsplit(data$Start, "-2018");ss
ss <- lapply(ss, as.character);ss
ss <- unlist(ss);ss
ss <- strsplit(ss, "-");ss
ss <- unlist(ss);ss
ss <- lapply(ss, as.character);ss

#opt 2 # gsub replaces all instances
ss <- gsub("-2018","",data$Start);tail(ss)[1]
ss <- gsub("-","/",ss);tail(ss)[1]

#opt 3
ss <- sub("-2018","",data$Start);ss[1]
month.abb
month.name
ff <- substr(ss,0,6) ;ff[1] # separate first section before period 
t <- substr(ss,9,99) ;t # separate second section after period 
t <- gsub(" pm","",t);t # remove pm
t <- gsub(" am","",t);t # remove am
format(as.POSIXct((ll) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M") #convert to POSIX     
# add time col to df after separating (only need time difference to get activity time per activity state for plots)   
hour <- substr(t,1,2);hour# create hour col
min <- substr(t,3,99);min# create min col



class(strsplit(data$Start, "-")[[101]]) # this is a character vec

#change days and months to two chars
data$Day[nchar(data$Day)==1]<-paste(0,data$Day[nchar(data$Day)==1],sep="")
data$Month[nchar(data$Month)==1]<-paste(0,data$Month[nchar(data$Month)==1],sep="")

#potential to separate by am and pm
strsplit(data$Start, "p")

micro_shd<-subset(micro_shd_all, format(as.POSIXlt(micro_shd_all$dates), "%y/%m/%d")>=daystart & format(as.POSIXlt(micro_shd_all$dates), "%y/%m/%d")<=dayfin)

data$Start <- as.POSIXct(as.character(data$Trait),format="%m/%d/%Y %H:%M")
head(data)







