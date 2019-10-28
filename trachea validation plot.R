a = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/air pollution_Larken_and_utrecht/trachea_no2combined_ivan.csv",sep = ";")
attach(a)
TRACHEA = as.numeric(gsub(",",".",no2_uu))
Prediction = as.numeric(gsub(",",".",NO2_ivan))
plot(TRACHEA,Prediction)
summary(lm(TRACHEA~Prediction))
summary(TRACHEA)
a$TRAEHEA = TRACHEA
a$Prediction = Prediction
library(ggplot2)
ggplot(a, aes(y=Prediction, x=TRACHEA)) +
  geom_point(size=2, shape=23)+  geom_smooth(method=lm, se=FALSE) + geom_abline(intercept = 0, slope = 1)+
  xlim(5, 60)+
  ylim(5, 60)
ggsave("validation.png",device = "png")
