# Testing markovianity property
library(reshape2)
library(dplyr)
library(markovchain)

# licznosci wg stanow
P <- matrix(c(0, 0, 0, 0, 50, 43, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 38, 16, 38,1, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 27, 20, 19,20, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 1,  1,  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0,  0,  0, 0, 78, 16, 1,  1, 0, 0, 0, 0, 
              0, 0, 0, 0, 0,  0,  0, 0, 38, 25, 29, 1, 0, 0, 0, 0, 
              0, 0, 0, 0, 0,  0,  0, 0, 23, 25, 29, 1, 0, 0, 0, 0, 
              0, 0, 0, 0, 0,  0,  0, 0, 17, 19, 25, 11,0, 0, 0, 0, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    11, 12, 1, 12, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    11, 12, 1, 13, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    23, 12, 1, 13, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    27, 12, 1, 17, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    18, 12, 1, 19, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    19, 12, 1, 21, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    10, 12, 1, 12, 
              0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,    10, 12, 1, 12),
            nrow = 16, ncol = 16, byrow = TRUE)

row.names(P)<-c('K1_0', 'K1_1_30', 'K1_31_60', 'K1_61_90', 'K2_0', 'K2_1_30', 'K2_31_60', 'K2_61_90', 'K3_0', 'K3_1_30', 'K3_31_60', 'K3_61_90', 'K3+_0', 'K3+_1_30', 'K3+_31_60', 'K3+_61_90')
colnames(P)<-c('K1_0', 'K1_1_30', 'K1_31_60', 'K1_61_90', 'K2_0', 'K2_1_30', 'K2_31_60', 'K2_61_90', 'K3_0', 'K3_1_30', 'K3_31_60', 'K3_61_90', 'K3+_0', 'K3+_1_30', 'K3+_31_60', 'K3+_61_90')
ttabela = melt(P) # transponuje macierz do sekwencji wystapienia stanow
ttabela[,'value']<-ifelse(ttabela[,'value']==0,1, ttabela[,'value']) # zamieniam 0 na 1

vec_with_seqentions<-c()
for(i in 1:nrow(ttabela)){
  freq <- rep(paste0(ttabela[i,'Var1'],'_',ttabela[i,'Var2']), ttabela[i,'value'])
  vec_with_seqentions<-append(vec_with_seqentions,freq)
}
# vec_test<-as.factor(vec_test)
verifyMarkovProperty(vec_with_seqentions)
fisher.test(P, simulate.p.value = TRUE)
# Exactly the same p-value, as Cochran's conditions are never met:
fisher.test(P, simulate.p.value = TRUE, hybrid=TRUE)
