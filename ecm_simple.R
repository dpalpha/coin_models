

# TWO STEPS COINTEGRATION SEARCH SIMPLE ERROR CORRECTION MODEL 
# ERROR CORECTION MODEL dl_Y_ua = b1*dl_ua(-1) + b2*residuals_ECM(-1) + b3*dl_ue(-1)   gdzie ECM = L_Y = b0 + b1*L_ua+b2*L_ue

# test kointegracji
library(urca)
library(vars)


summary(cur.urdf)
Y=completedData3$Khmelnytsk 
X=completedData3$Kherson
VARselect(cbind(Y,X), lag.max = 10, type = 'const')

# EQUATION: WITHOUT BIAS TREND Khmelnytsk<-Kherson
Y=completedData3$Khmelnytsk
X=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval

lag=1
summary(lm(
          diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-lag)]
  +lm( # równanie kointegracyjne
    log(X)  # logarytm ceny Kmelnitsk
    ~log(Y) # logarytm ceny Kharkow
    )$residuals[-(length(X)-lag):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-lag)])) # dl_X(-1)
  +0 # usuwam stał
))
# podsumowanie jest relacja logrun Khmelnytsk<-Kherson


# EQUATION: WITH BIAS TREND Khmelnytsk<-Kherson
Y=completedData3$Khmelnytsk
X=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Kmelnitsk
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))

# tak sprawdzalem reszty w ECM z trendem
plot(lm(log(Y)~log(X)+seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))])
# bez trendu
plot(lm(log(Y)~log(X))$residuals[-(length(X)-1):-(length(X))])
# potwierdzenie dlugokresowej relacji longrun


# EQUATION WITH BIAS TREND Mykolaiv<-Kherson
Y=completedData3$Mykolaiv
X=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Mykolaiv
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# potwierdzenie dlugokresowej relacji Mykolaiv<-Kherson

# EQUATION WITH BIAS TREND Poltava<-Kherson
X=completedData3$Poltava
Y=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval
# wystpuje kointegracja

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Poltava
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# brak dlugokresowej relacji  Poltava->Kherson


# EQUATION WITH BIAS TREND Vinnytsia<-Kherson
Y=completedData3$Vinnytsia
X=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval
# wystpuje kointegracja

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Vinnytsia
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# brak dlugokresowej relacji Vinnytsia<-Kherson


# EQUATION WITH BIAS TREND Zhytomyr<-Kherson, Zhytomyr->Kherson
X=completedData3$Zhytomyr
Y=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval
# wystpuje kointegracja

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Zhytomyr
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))

# brak dlugokresowej relacji  Zhytomyr->Kherson


# EQUATION WITH BIAS TREND Zhytomyr<-Kherson, Zhytomyr->Kherson
X=completedData3$Zhytomyr
Y=completedData3$Kherson

# test na kointegracje
cur.urdf<-ur.df(lm(log(X)~log(Y))$residuals, type="none", selectlags="AIC")
summary(cur.urdf)
cur.urdf@teststat
cur.urdf@cval
# wystpuje kointegracja

summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Kmelnitsk
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# brak dlugokresowej relacji  Zhytomyr->Kherson


# EQUATION WITH BIAS TREND Zaporizhzhia<-Kherson, Zaporizhzhia->Kherson
Y=completedData3$Zaporizhzhia
X=completedData3$Kherson
summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Zaporizhzhia
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# brak dlugokresowej relacji  Zaporizhzhia->Kherson


# EQUATION WITH BIAS TREND Kirovohrad<-Kherson, Kirovohrad->Kherson
X=completedData3$Kirovohrad
Y=completedData3$Kherson
summary(lm(
  diff(log(Y))[-1] # Y:  dl_Y pierwsze roznice z logarytmu
  ~diff(log(Y))[-(length(X)-1)]
  +lm(       # równanie kointegrujace
    log(Y)   # logarytm ceny Kirovohrad
    ~log(X)  # logarytm ceny Kharkow
    +seq(1,length(X),1))$residuals[-(length(X)-1):-(length(X))] # reszty (-1)
  +diff(log(X[-(length(X)-1)])) # dl_X(-1)
  +1 #  stala
))
# brak dlugokresowej relacji  Kirovohrad<-Kherson, Kirovohrad->Kherson




cointest=ca.jo(cbind(Y,X), K=10, type="eigen",ecdet = "const", spec = "transitory" )
cointest@teststat[2]
cointest@teststat[1]
cointest@cval

