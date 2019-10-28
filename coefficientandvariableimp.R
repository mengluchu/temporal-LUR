
library(leaps)
library(plyr)    # 'join', 'rbind.fill', 'dlply'
library(reshape2)
library(ggplot2)
library(glmnet)  # Package to fit ridge/lasso/elastic net models
library(forecast) # CV()
library(gsubfn)
library(relaimpo)
library(dbplyr)
lmldir <- "C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/LUR_Utrecht/"
preddir <- "C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/LUR_Utrecht/"
NO2dir <-"C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/LUR_Utrecht/"
imgdir <- "C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/LUR_Utrecht/"

#NO2
#myP = c( "hvy_traf_ld_50", "mjr_rd_len_25" ,"rd_len_1000",   "rd_len_5000" )
#O3
#myP = c( "port_5000", "traf_ld_300" ,"traf_mjr_ld_25" ,  "traf_mjr_ld_300" )
#NO
#myP = c("hvy_traf_ld_50" "ind_5000"       "mjr_rd_len_25"  "rd_len_1000")
 
load(paste(NO2dir, "NO2_2000_2012.RData", sep="")) # data.frame NO2

load(paste(NO2dir, "NO_2000_2012.RData", sep="")) # data.frame NO2
load(paste(NO2dir, "O3_2000_2012.RData", sep="")) # data.frame NO2

load(paste0(lmldir, "coordlml2015v2.RData"))

coefandimp("NO",NO, c("hvy_traf_ld_50", "ind_5000","mjr_rd_len_25",  "rd_len_1000") )

coefandimp("O3",O3, c( "port_5000", "traf_ld_300" ,"traf_mjr_ld_25" ,  "traf_mjr_ld_300" ) )

coefandimp("NO2",NO2, c( "hvy_traf_ld_50", "mjr_rd_len_25" ,"rd_len_1000",   "rd_len_5000" ) )

coefandimp = function(variablename, datadf, myP, coefdir = "coefdir"){
stn <- stn[stn$owner=='RIVM',]
stn$station <- as.character(stn$station)
stn <- stn[order(stn$station),]
rownames(stn) <- seq(length=nrow(stn)) # niet echt nodig, wel netjes
load(paste(preddir, "Epreds.RData", sep=""))
predabbr <- list("major" = "mjr", "road" = "rd", "load"="ld", "heavy"="hvy", "length"="len",
                 "industry" = "ind")
prednames <- names(Epreds)
prednamesnew <- gsubfn(paste(names(predabbr),collapse="|"),predabbr,prednames)
names(Epreds) <- prednamesnew
 
NO2stn <- sort(names(datadf)[-1]) # Minus column 'datum'
 
### Dummy variables ###
HH <- as.integer(substr(datadf$datum,12,13))
# unique(HH) ; table(HH) ; unique(table(HH))
mn <- months(as.Date(substr(datadf$datum, 1, 10), format = "%d/%m/%Y"), abbreviate = T)
mnt <- factor(mn, levels = c('jan', 'feb', 'mrt', 'apr', 'mei', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec'))
# unique(mn) ; table(mn)
wk <- weekdays(as.Date(substr(datadf$datum, 1, 10), format = "%d/%m/%Y"), abbreviate = T)
wkf <- factor(x=wk, levels = c('ma', 'di', 'wo', 'do', 'vr', 'za', 'zo'))
levels(wkf)[1:5] <- 'wkday'
levels(wkf)[2:3] <- 'wkend'
# barplot(table(wkf)) ; wkf[40:100]


### SET TIME PERIOD ###
# Expand ESCAPE period to five years
# Middle of ESCAPE start and stop date is 31-12-2009
ESdate <- seq(which(datadf$datum=="01/07/2006 01"), which(datadf$datum=="01/07/2011 24"))

# Subset NO2 data
NO2E <- NO2[ESdate,]

# Delete any stations with NA only?
NO2NAi <- sapply(NO2E, function(x) sum(!is.na(x))==0) # yep
NO2Ev <- NO2E[,!NO2NAi]
identical(names(NO2Ev),sort(names(NO2Ev)))


### Remove stations that have less than 20% data ###
NO2stnsumna <- sapply(NO2Ev[-1], function(x) 1-sum(is.na(x))/dim(NO2Ev)[1])
delvars <- names(NO2Ev) %in% names(which(NO2stnsumna<0.2))
NO2Ev <- NO2Ev[!delvars]


 
### CALCULATE MEAN PER STATION FOR PREDICTOR SELECTION ###
NO2Emean <- data.frame(stn=names(NO2Ev[-1]), NO2E=colMeans(NO2Ev[-1], na.rm = T),
                       stringsAsFactors=FALSE)
row.names(NO2Emean) <- 1:dim(NO2Emean)[1]
EPSN <- join(NO2Emean, Epreds, by="stn")
print(coefficients(lm(as.formula(paste0("NO2E~", paste(myP, collapse="+"))),
                data=EPSN)))
### ADD DUMMY VARIABLES FOR COEFFICIENT CALCULATION ###
# month, wkday/wkend, HOUR
# Results for functions weekdays and months is LOCALE dependent
NO2Ev$HH <- as.integer(substr(NO2Ev$datum,12,13))
mn <- months(as.Date(substr(NO2Ev$datum, 1, 10), format = "%d/%m/%Y"), abbreviate = T)
NO2Ev$M <- factor(mn, levels = c('jan', 'feb', 'mrt', 'apr', 'mei', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec'))
wk <- weekdays(as.Date(substr(NO2Ev$datum, 1, 10), format = "%d/%m/%Y"), abbreviate = T)
wkf <- factor(x=wk, levels = c('ma', 'di', 'wo', 'do', 'vr', 'za', 'zo'))
levels(wkf)[1:5] <- 'wkday'
levels(wkf)[2:3] <- 'wkend'
NO2Ev$wk <- wkf


### AGGREGATE AP CONCENTRATION PER DUMMY VARIABLE ###
NO2Evm <- melt(NO2Ev[,-1], id=c('HH', 'wk', 'M'))
NO2EaggrHwk <- aggregate(NO2Evm$value, by=list(NO2Evm$HH, NO2Evm$M ,NO2Evm$wk, NO2Evm$variable), FUN=mean, na.rm=T)
names(NO2EaggrHwk) <- c('HH', 'M', 'wk', 'stn', 'NO2')
NO2Eaggr <- NO2EaggrHwk
NO2c <- join(NO2Eaggr, Epreds, by='stn', type='left')
# Drop column x and y (first find them)
NO2c <- NO2c[-c(which(names(NO2c)=='x'),which(names(NO2c)=='y'))]

# Which predictors were selected for each model with n predictors
prselfun <- function(x){
  ps <- summary(NO2.ms.ef)$which[x, ]
  names(which(ps==TRUE))
}
prsel <- lapply(1:7, prselfun)


### COMBINE AGGREGATE AND PREDICTORS ###

lmlistm4 <- dlply(NO2c, .(HH, M, wk),
                  function(x) lm(as.formula(paste0("NO2~", paste(myP, collapse="+"))),
                                 data=x))
### CALCULATE PARAMETER COEFFICIENTS ###
var_importance = function(m)
{
  myVec <- c(calc.relimp(m, type =c("lmg"),rela = F)@lmg, unexplained = 1- summary(m)$r.squared)
}
 
 extractcoef <- function(m) { # function to extract coefficients from models
   cf <- coef(m)
   np <- length(cf)
   pval <-  lmp(m)
   myVec <- c(coef(m), pval, summary(m)$adj.r.squared)
  names(myVec) <- c("intercept", names(myVec)[2:np], "pval", "r2")
  data.frame(t(myVec))
}


NO2cm4 <- ldply(lmlistm4, var_importance)

NO2cm4m = NO2cm4%>% melt( id=c('HH', 'wk', 'M'))
#%>% filter(M=="jan"|M=="apr"|M=="jul"|M=="okt")
ggplot(data=NO2cm4m, aes(x=HH, y=value, group=M, color=M)) + geom_line(size=1) +
  facet_grid(variable~wk, scales="free") +
  theme(axis.title.y = element_blank(), legend.title=element_blank()) +
  labs(title=paste('Explained variance:', variablename) ,x='Hour') +
  scale_x_continuous(breaks=seq(0,24, by=3))
#ggsave("F:LUR_Utrecht/O3_vimp.png")

######### coefficients save
 
NO2cm4 <- ldply(lmlistm4, extractcoef)
 
allmonths <- unique(as.character(NO2cm4$M))
wkdtype <- unique(as.character(NO2cm4$wk))

for (curM in allmonths) {
  for (curwk in wkdtype) {
    curmod <- NO2cm4[NO2cm4$M==curM & NO2cm4$wk==curwk,]
    curmodclean <- subset(curmod, select = -c(M, wk))
    write.csv(curmodclean, file=paste0(imgdir,"/", coefdir,"/",variablename ,"-",curM, "-", curwk, ".csv"), row.names = F)
  }
}
}
