# TWO STEPS ERROR CORRECTION MECHANIZM 


library(dplyr)
library(mice)
library(lubridate)

# set local directory
setwd('C:\\Users\\denyc\\Desktop\\dok')
dane_ = read.csv('dane_preze.csv',head=T, sep=';')

# MISSING VALUES TREATMENT
# write.csv(completedData, 'dane_do_prezentacji1.csv')
# wyznaczenie brakow w celu poprawnego uzupelnienia 
# wyznaczenie braków danych
dane_[,'Cherkasy'][dane_[,'Cherkasy']==0]<-NA # 'zera' zamieniam na NA
dane_[,'Mykolaiv'][dane_[,'Mykolaiv']==0]<-NA
dane_[,'Poltava'][dane_[,'Poltava']==0]<-NA
# dane_[,'Odesa'][dane_[,'Odesa']==0]<-NA
dane_[,'Vinnytsia'][dane_[,'Vinnytsia']==0]<-NA
dane_[,'Kherson'][dane_[,'Kherson']==0]<-NA
dane_[,'Khmelnytsk'][dane_[,'Khmelnytsk']==0]<-NA
dane_[,'Zhytomyr'][dane_[,'Zhytomyr']==0]<-NA
dane_[,'Kyiv'][dane_[,'Kyiv']==0]<-NA
dane_[,'Zaporizhzhia'][dane_[,'Zaporizhzhia']==0]<-NA
dane_[,'Lugansk'][dane_[,'Lugansk']==0]<-NA
dane_[,'Kirovohrad'][dane_[,'Kirovohrad']==0]<-NA

md.pattern(dane_)
aggr_plot <- aggr(dane_, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(dane_[c(2,3)])

# mozliwe opcje do redukcji brakow danych

# Method           Description Scale type Default
# pmm              Predictive mean matching numeric Y
# norm             Bayesian linear regression numeric
# norm.nob         Linear regression, non-Bayesian numeric
# mean             Unconditional mean imputation numeric
# 2L.norm          Two-level linear model numeric
# logreg           Logistic regression factor, 2 levels Y
# polyreg          Multinomial logit model factor, >2 levels Y
# polr Ordered     logit model ordered, >2 levels Y
# lda Linear       discriminant analysis factor
# sample           Random sample from the observed data any

# uzupelnienie braków danych, używam biblioteki library(mice)
tempData <- mice(dane_,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData1 <- complete(tempData,1)
# https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf


# SIMPLE OUTLIERS TREATMENT, zrobilem proste podejscie wszytko co  wyzsze od srednia+moment_odch stan zamienaim na mediane
completedData2 = data.frame(sapply(completedData1[,-c(1)],function(j){ifelse( abs(j-mean(j))>sd(j)*2.9, median(j), j)}))

#as.POSIXct(harMet_15Min$datetime[1],format="%d-%m-%Y%H:%M")
completedData3 = completedData2 %>% 
  mutate(day = floor_date(dmy_hm(completedData$Date), '1 day'), # formatuje kolumne z Data do dni
         week = floor_date(dmy_hm(completedData$Date), 'week') # formatuje kolumne z Data do tygodni
         ) %>%
  group_by(day) %>% # grupuje tabele wg tygodni
  do(data.frame(
    Date = max(.$day),
    Cherkasy = mean(.$Cherkasy),
    Mykolaiv = mean(.$Mykolaiv),
    Poltava = mean(.$Poltava),
    Vinnytsia = mean(.$Vinnytsia),
    Kherson = mean(.$Kherson),
    Khmelnytsk = mean(.$Khmelnytsk),
    Zhytomyr = mean(.$Zhytomyr),
    Kyiv = mean(.$Kyiv),
    Zaporizhzhia = mean(.$Zaporizhzhia),
    Lugansk = mean(.$Lugansk),
    Kirovohrad = mean(.$Kirovohrad),
    Lugansk = mean(.$Lugansk)
    # Odesa = median(.$Odesa)
  )) %>% data.frame()

# eksport przygotowanych danych do excela
write.csv(completedData3, 'prep_dataWEEK.csv') # zapisuje dane do csv
