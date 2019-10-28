# Examine covariability in air pollutants from LUR model results
# Start with the transects of Utrecht, maybe the whole map of Utrecht later on.


library(reshape2)
library(ggplot2)
library(GGally)
library(data.table)

transectNO2 <- "F://NO2_sonic_files/"
transectNO <- "F://NO_sonic/"
transectO3 <- "F://O3_Sonic/"
imgdir <- "F://"

NO2wkday <- c("NO2_4p_01-wkday", "NO2_4p_02-wkday", "NO2_4p_03-wkday", "NO2_4p_04-wkday", "NO2_4p_05-wkday", "NO2_4p_06-wkday",
               "NO2_4p_07-wkday", "NO2_4p_08-wkday", "NO2_4p_09-wkday", "NO2_4p_10-wkday", "NO2_4p_11-wkday", "NO2_4p_12-wkday")
NO2wkend <- c("NO2_4p_01-wkend", "NO2_4p_02-wkend", "NO2_4p_03-wkend", "NO2_4p_04-wkend", "NO2_4p_05-wkend", "NO2_4p_06-wkend",
               "NO2_4p_07-wkend", "NO2_4p_08-wkend", "NO2_4p_09-wkend", "NO2_4p_10-wkend", "NO2_4p_11-wkend", "NO2_4p_12-wkend")

NOwkday <- c("NO_4p_01-wkday", "NO_4p_02-wkday", "NO_4p_03-wkday", "NO_4p_04-wkday", "NO_4p_05-wkday", "NO_4p_06-wkday",
              "NO_4p_07-wkday", "NO_4p_08-wkday", "NO_4p_09-wkday", "NO_4p_10-wkday", "NO_4p_11-wkday", "NO_4p_12-wkday")
NOwkend <- c("NO_4p_01-wkend", "NO_4p_02-wkend", "NO_4p_03-wkend", "NO_4p_04-wkend", "NO_4p_05-wkend", "NO_4p_06-wkend",
              "NO_4p_07-wkend", "NO_4p_08-wkend", "NO_4p_09-wkend", "NO_4p_10-wkend", "NO_4p_11-wkend", "NO_4p_12-wkend")

O3wkday <- c("O3_4p_01-wkday", "O3_4p_02-wkday", "O3_4p_03-wkday", "O3_4p_04-wkday", "O3_4p_05-wkday", "O3_4p_06-wkday",
              "O3_4p_07-wkday", "O3_4p_08-wkday", "O3_4p_09-wkday", "O3_4p_10-wkday", "O3_4p_11-wkday", "O3_4p_12-wkday")
O3wkend <- c("O3_4p_01-wkend", "O3_4p_02-wkend", "O3_4p_03-wkend", "O3_4p_04-wkend", "O3_4p_05-wkend", "O3_4p_06-wkend",
              "O3_4p_07-wkend", "O3_4p_08-wkend", "O3_4p_09-wkend", "O3_4p_10-wkend", "O3_4p_11-wkend", "O3_4p_12-wkend")

##############################################
# 1. Scatterplot of airpollutants, differentiate between weekdays weekends
# Flatten time component except weekdays weekends
# rbindlist (data.table) vectorizes a whole list


### NO2 weekdays ###
NO2wkdayc <- lapply(NO2wkday, function(x) read.csv(paste0(transectNO2, x, "/", "no2_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
NO2wkdayvec <- c(as.matrix(rbindlist(NO2wkdayc)))
# Transpose all entries
NO2wkdayt <- lapply(1:length(NO2wkdayc), function(x) as.data.frame(t(NO2wkdayc[[x]])))

# Melt!
NO2wkdaytm <- melt(NO2wkdayt)
unique(NO2wkdaytm$variable)
unique(NO2wkdaytm$L1)
names(NO2wkdaytm) <- c('hour', 'value', 'month')
NO2wkdaytm$hour <- as.numeric(gsub("\\D", "", NO2wkdaytm$hour))

### NO2 weekend ###
NO2wkendc <- lapply(NO2wkend, function(x) read.csv(paste0(transectNO2, x, "/", "no2_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
# Transpose all entries
NO2wkendt <- lapply(1:length(NO2wkendc), function(x) as.data.frame(t(NO2wkendc[[x]])))

# Melt!
NO2wkendtm <- melt(NO2wkendt)
names(NO2wkdaytm) <- c('hour', 'value', 'month')



### NO weekdays ###
NOwkdayc <- lapply(NOwkday, function(x) read.csv(paste0(transectNO, x, "/", "no_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
NOwkdayvec <- c(as.matrix(rbindlist(NOwkdayc))) ; rm(NOwkdayc)

### O3 weekdays ###
O3wkdayc <- lapply(O3wkday, function(x) read.csv(paste0(transectO3, x, "/", "o3_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
O3wkdayvec <- c(as.matrix(rbindlist(O3wkdayc))) ; rm(O3wkdayc)

### Combine and plot ###
# The dataframe has over 2milion records, sample 1/10 for plotting
# Save to png, the pdf is too big to print quickly to the screen
AirPolwkday <- data.frame(NO2=NO2wkdayvec, NO=NOwkdayvec, O3=O3wkdayvec, NOxRatio =NOwkdayvec/NO2wkdayvec )
mysample <- sample(dim(AirPolwkday)[1],round(dim(AirPolwkday)[1]/5000))
str(AirPolwkday)
ggpairs(AirPolwkday[mysample,], title='Air Pollution Correlation - WEEKDAY')
ggsave(filename = paste0(imgdir, 'AirPolwkdayscatter.png'), device="png", width = 6, height = 5, scale=1.5)

# WEEKEND DAYS ###############################
### NO2 weekend ###
NO2wkendc <- lapply(NO2wkend, function(x) read.csv(paste0(transectNO2, x, "/", "no2_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
NO2wkendvec <- c(as.matrix(rbindlist(NO2wkendc))) ; rm(NO2wkendc)

### NO weekdays ###
NOwkendc <- lapply(NOwkend, function(x) read.csv(paste0(transectNO, x, "/", "no_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
NOwkendvec <- c(as.matrix(rbindlist(NOwkendc))) ; rm(NOwkendc)

### O3 weekdays ###
O3wkendc <- lapply(O3wkend, function(x) read.csv(paste0(transectO3, x, "/", "o3_d_c_4000.csv"),
                    header=FALSE, sep=",", dec=".", stringsAsFactors=FALSE))
O3wkendvec <- c(as.matrix(rbindlist(O3wkendc))) ; rm(O3wkendc)

### Combine and plot ###
# The dataframe has over 2milion records, sample 1/10 for plotting
# Save to png, the pdf is too big to print quickly to the screen
AirPolwkend <- data.frame(NO2=NO2wkendvec, NO=NOwkendvec, O3=O3wkendvec)
mysample <- sample(dim(AirPolwkend)[1],round(dim(AirPolwkend)[1]/10))
ggpairs(AirPolwkend[mysample,], title="Air Pollution Correllation - WEEKEND")
ggsave(filename = paste0(imgdir, 'AirPolwkendscatter.png'), device="png", width = 6, height = 5, scale=1.5)
# ggsave(filename = paste0(imgdir, 'AirPolwkendscatter.pdf'), width = 6, height = 5, scale=1.5)



#### JANUARI ####
# NO2
NO2jan <- read.csv(paste0(transectNO2, NO2wkday[1], "/", "no2_d_c_4000.csv"),
                  header=F)
NO2jant <- as.data.frame(t(NO2jan))
names(NO2jant) <- paste0("t", 1:24)
row.names(NO2jant) <- 1:dim(NO2jant)[1]


# NO
NOjan <- read.csv(paste0(transectNO, NOwkday[1], "/", "no_d_c_4000.csv"),
                   header=F)
NOjant <- as.data.frame(t(NOjan))
names(NOjant) <- paste0("t", 1:24)
row.names(NOjant) <- 1:dim(NOjant)[1]
NOjant[1:10,1:10]

# 03
O3jan <- read.csv(paste0(transectO3, O3wkday[1], "/", "o3_d_c_4000.csv"),
                  header=F)
O3jant <- as.data.frame(t(O3jan))
names(O3jant) <- paste0("t", 1:24)
row.names(O3jant) <- 1:dim(O3jant)[1]
O3jant[1:10,1:10]

janwkday9u <- data.frame(NO2=NO2jant[,12], NO=NOjant[,12], "NO_NO2" =NOjant[,12]/NO2jant[,12] , O3=O3jant[, 12])
# plot(janwkday9u)
names(janwkday9u)[3] = "NO / NO2"
ggpairs(janwkday9u, title="Air Pollants Correllation - Jaunaray, 12pm, weekday")
ggsave("Jan12wd_covar.png")


#### JUNE ####
# NO2
NO2jun <- read.csv(paste0(transectNO2, NO2wkday[6], "/", "no2_d_c_4000.csv"),
                   header=F)
NO2junt <- as.data.frame(t(NO2jun))
names(NO2junt) <- paste0("t", 1:24)
row.names(NO2junt) <- 1:dim(NO2junt)[1]
NO2junt[1:10,1:10]

# NO
NOjun <- read.csv(paste0(transectNO, NOwkday[6], "/", "no_d_c_4000.csv"),
                  header=F)
NOjunt <- as.data.frame(t(NOjun))
names(NOjunt) <- paste0("t", 1:24)
row.names(NOjunt) <- 1:dim(NOjunt)[1]
NOjunt[1:10,1:10]

# 03
O3jun <- read.csv(paste0(transectO3, O3wkday[6], "/", "o3_d_c_4000.csv"),
                  header=F)
O3junt <- as.data.frame(t(O3jun))
names(O3junt) <- paste0("t", 1:24)
row.names(O3junt) <- 1:dim(O3junt)[1]
O3junt[1:10,1:10]


junwkday9u <- data.frame(NO2=NO2junt[,9], NO=NOjunt[,9], O3=O3junt[,9])
range(junwkday9u$NO2)
range(junwkday9u$NO)
range(junwkday9u$O3)
plot(junwkday9u)
junwkday14u <- data.frame(NO2=NO2junt[,14], NO=NOjunt[,14], O3=O3junt[,14])
plot(junwkday14u)
junwkday18u <- data.frame(NO2=NO2junt[,18], NO=NOjunt[,18], O3=O3junt[,18])
plot(junwkday18u)
