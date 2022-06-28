library(corrplot)
library(ROSE)
library(arm)

colnames(df)
sum(is.na(df_new))



df <- read.csv("fundamentals222.csv"
market_cap=log(df$Market.Cap)
Net_income=log(df$Net.Income.a.)
topct <- function(x) { as.numeric( sub("\\D*([0-9.]+)\\D*","\\1",x) )/100 }
div_yield<-topct(df$Div.Yield)
df_new <- cbind(df, market_cap, Net_income,div_yield)
df_new
colnames(df_new)




#LINEAR  REGRESSION MODEL

lp<-lm(PRICE~Market.Cap+P.E.ttm+EPS.ttm+Beta+div_yield,data=df)
summary(lp)


#LINEAR LOG REGRESSION MODEL

ln<-lm(PRICE~market_cap+P.E.ttm+EPS.ttm+Beta+div_yield,data=df)
summary(ln)



#PLOTTING SCATTERPLOTS OF EACH FACTOR AGAINST PRICE
par(mfrow=c(3,2))     
plot(df$PRICE~market_cap) 
plot(df$PRICE~df$P.E.ttm)
plot(df$PRICE~df$EPS.ttm) 
plot(df$PRICE~df$Beta)
plot(df$PRICE~div_yield)   


#PLOTTING HISTOGRAMS OF EACH FACTOR 
par(mfrow=c(3,2))    
hist(df_new$market_cap) 
hist(df$P.E.ttm)
hist(df$EPS.ttm) 
hist(df$Beta)
hist(df_new$div_yield)

#CORREALTION PLOT
library(corrplot)

datacorr <- subset(df_new, select=c("PRICE","market_cap", "P.E.ttm", "EPS.ttm","Beta","div_yield"))
mydata.cor = cor(datacorr)
title = "Correlation of Variables"
corrplot(mydata.cor, method = "color", diag = FALSE, type = "upper",  order = "hclust", title = title, addCoef.col= "black", sig.level = 0.05, insig = "blank", mar = c(0,0,1,0))

















#NORMALISING THE FACTORS
df_sub=df_new[c("PRICE","market_cap","div_yield","P.E.ttm","EPS.ttm","Beta")]
data_norm <- data.frame(df_sub)
data_norm
data_norm$market_cap = (data_norm$market_cap-mean(data_norm$market_cap))/sd(data_norm$market_cap)
data_norm$div_yield = (data_norm$div_yield-mean(data_norm$div_yield))/sd(data_norm$div_yield)
data_norm$P.E.ttm = (data_norm$P.E.ttm-mean(data_norm$P.E.ttm))/sd(data_norm$P.E.ttm)
data_norm$EPS.ttm = (data_norm$EPS.ttm-mean(data_norm$EPS.ttm))/sd(data_norm$Beta)
data_norm$Beta = (data_norm$Beta-mean(data_norm$Beta))/sd(data_norm$Beta)
head(data_norm)

#PLOTTING HISTOGRAMS OF EACH NORMALISED FACTOR 
par(mfrow=c(3,2))    
hist(data_norm$market_cap) 
hist(data_norm$P.E.ttm)
hist(data_norm$EPS.ttm) 
hist(data_norm$Beta)
hist(data_norm$div_yield)

library(rlang)


par(mfrow=c(1,2))
qqnorm(data_norm$market_cap, main='Normal')
qqline(data_norm$market_cap)


qqnorm(df_new$market_cap, main='non-Normal')
qqline(df_new$market_cap)


library(base64)
suppressWarnings
library(ggplot2)
install.packages("patchwork")
library(patchwork)

#create plot to visualize fitted linear regression model
A<-ggplot(data_norm,aes(market_cap,PRICE)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) 

B<-ggplot(data_norm,aes(P.E.ttm,PRICE)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) 
  
C<-ggplot(data_norm,aes(EPS.ttm,PRICE)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) 
  
D<-ggplot(data_norm,aes(div_yield,PRICE)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) 

E<-ggplot(data_norm,aes(Beta,PRICE)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)  

A+B+C+D+E










#RESIDUALS FOR NORMAL
par(mfrow=c(2,2)) 
plot(lp$res~ln$fitted)
hist(lp$res)
qqnorm(lp$res); qqline(lp$res, col = 2,lwd=2,lty=2)
shapiro.test(lp$res)



#RESIDUALS FOR LOG
par(mfrow=c(2,2)) 
plot(ln$res~ln$fitted)
hist(ln$res)
qqnorm(ln$res); qqline(ln$res, col = 2,lwd=2,lty=2)
shapiro.test(ln$res)





















