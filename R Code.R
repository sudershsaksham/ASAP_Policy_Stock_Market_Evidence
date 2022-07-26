library(tidyverse)
library(ggthemes)

# Reading Pre-ASAP Crash Data and Creating Prediction Data Frame
estim1 <- data.frame(read.csv("GitHub/ASAP_Policy_Stock_Market_Evidence/Estimation Dataset I - Sheet1.csv"))
evt1 <- data.frame(read.csv("GitHub/ASAP_Policy_Stock_Market_Evidence/Event Dataset I - Sheet1.csv"))
pred <- data.frame()

# Using Linear Model to calculate Fama-French Factors
for(i in 1:15){
  ind = (4*i) - 3
  temp <- summary(lm(estim1[,ind] ~ estim1[,(ind + 1)] + estim1[,(ind + 2)] + estim1[,(ind + 3)]))$coefficients
  pred[1,i] <- temp[1,1]
  pred[2,i] <- temp[2,1]
  pred[3,i] <- temp[3,1]
  pred[4,i] <- temp[4,1]
}

# Creating Abnormal Returns Data Set
ar1 <- data.frame()
for(i in 1:15){
  ind = (4*i) - 3
  for(j in 1:10) {
    ar1[j,i] = (evt1[j,ind] - (pred[1,i] + pred[2,i]*evt1[j,ind + 1] + pred[3,i]*evt1[j,(ind + 2)] + pred[4,i]*evt1[j,(ind + 3)]))
  }
}

# Adding Average of Rows in Abnormal Returns Data Set
ar1 <- ar1 %>% mutate(avg= rowMeans(ar1))

# Reading Post-ASAP Crash Data and Creating Prediction Data Frame
estim2 <- data.frame(read.csv("GitHub/ASAP_Policy_Stock_Market_Evidence/Estimation Dataset II - Sheet1.csv"))
evt2 <- data.frame(read.csv("GitHub/ASAP_Policy_Stock_Market_Evidence/Event Dataset II - Sheet1.csv"))
pred <- data.frame()

# Using Linear Model to calculate Fama-French Factors
for(i in 1:15){
  ind = (4*i) - 3
  temp <- summary(lm(estim2[,ind] ~ estim2[,(ind + 1)] + estim2[,(ind + 2)] + estim2[,(ind + 3)]))$coefficients
  pred[1,i] <- temp[1,1]
  pred[2,i] <- temp[2,1]
  pred[3,i] <- temp[3,1]
  pred[4,i] <- temp[4,1]
}

# Creating Abnormal Returns Data Set
ar2 <- data.frame()
for(i in 1:15){
  ind = (4*i) - 3
  for(j in 1:10) {
    ar2[j,i] = evt1[j,ind] - (pred[1,i] + pred[2,i]*evt1[j,(ind + 1)] + pred[3,i]*evt1[j,(ind + 2)] + pred[4,i]*evt1[j,(ind + 3)])
  }
}

# Adding Average of Rows in Abnormal Returns Data Set
ar2 <- ar2 %>% mutate(avg= rowMeans(ar2))

# Creating Heat Map For Both Pre and Post ASAP Data Sets
heat_map_data1 <- rbind(estim1,evt1)
heat_map_data1 <- heat_map_data1[4*(1:15)-3]
heat_map_data1 <- gather(heat_map_data1, key = "Case", Returns, factor_key = TRUE)
heat_map_data1 <- mutate(heat_map_data1, date=rep(-119:10,times=14))
ggplot(data=heat_map_data1) + 
  geom_tile(mapping=aes(x=Case,y=date,fill=Returns),color="white",lwd=0.5)+
  scale_fill_binned(type = "viridis")+
  theme_solarized()+
  ggtitle("Actual Returns Before and After Crash",subtitle = "Pre-ASAP Policy")+
  ylab("Days Before and After Crash")+
  xlab("Different Cases")+
  theme(axis.text.x=element_blank())

heat_map_data2 <- rbind(estim2,evt2)
heat_map_data2 <- heat_map_data2[4*(1:15)-3]
heat_map_data2 <- gather(heat_map_data2, key = "Case", Returns, factor_key = TRUE)
heat_map_data2 <- mutate(heat_map_data2, date=rep(-119:10,times=14))
ggplot(data=heat_map_data2) + 
  geom_tile(mapping=aes(x=Case,y=date,fill=Returns),color="white",lwd=0.5)+
  scale_fill_binned(type = "viridis")+
  theme_solarized()+
  ggtitle("Actual Returns Before and After Crash",subtitle = "Post-ASAP Policy")+
  ylab("Days Before and After Crash")+
  xlab("Different Cases")+
  theme(axis.text.x=element_blank())

# Plotting Abnormal Returns for Both Pre and Post ASAP Data Sets
ggplot(data=ar1) +
  geom_smooth(mapping = aes(y=avg,x=1:10), color='Black') +
  geom_point(mapping = aes(y=V1,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V2,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V3,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V4,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V5,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V6,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V7,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V8,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V9,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V10,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V11,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V12,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V13,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V14,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V15,x=1:10),alpha=0.3)+
  theme_solarized()+
  ggtitle("Abnormal Returns in Event Window",subtitle = "Pre-ASAP Policy")+
  ylab("Abnormal Returns")+
  xlab("Days Since Crash")


ggplot(data=ar2) +
  geom_smooth(mapping = aes(y=avg,x=1:10), color='Black') +
  geom_point(mapping = aes(y=V1,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V2,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V3,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V4,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V5,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V6,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V7,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V8,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V9,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V10,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V11,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V12,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V13,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V14,x=1:10),alpha=0.3)+
  geom_point(mapping = aes(y=V15,x=1:10),alpha=0.3)+
  theme_solarized()+
  ggtitle("Abnormal Returns in Event Window",subtitle = "Post-ASAP Policy")+
  ylab("Abnormal Returns")+
  xlab("Days Since Crash")
