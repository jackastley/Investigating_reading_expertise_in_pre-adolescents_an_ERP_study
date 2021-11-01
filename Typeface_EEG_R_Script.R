### TYPEFACE EEG CHILDREN STUDY ###

#CLEAR R ENVIRONMENT

rm(list=ls())

#LOAD IN PACKAGES

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(e1071)
library(psych)
library(corrplot)
library(RColorBrewer)
library(emmeans)
library(dplyr)


#SET WORKING DIRECTORY
setwd("//rdfs.unisa.edu.au/Group_bbb_research/CAIN/PEOPLE/JackA/Typeface_EEG/Data_Analysis")


### CREATE AND FORMAT ERROR RATE DATA FRAME

er_file<-"ERROR_RATE_DATA.csv"
er_data<-read.csv(er_file)

#CONVERT FROM WIDE TO LONG DATA
er_data<- er_data %>% pivot_longer(
  cols= c(TFCR_L:NDNR_L,TFCR_W:NDNR_W),
  names_pattern = "(.)(.)(.*)_(.)",
  names_to = c("Target", "Fluency","Response","Task"),
  values_to = "value"
)

#CALCULATE EROR RATES
error_rates<-tibble(Fluent_Target_Letter=NA,Disfluent_Target_Letter=NA,Fluent_NonTarget_Letter=NA,Disfluent_NonTarget_Letter=NA,Fluent_Target_Word=NA,Disfluent_Target_Word=NA,Fluent_NonTarget_Word=NA,Disfluent_NonTarget_Word=NA, .rows = 39)

correct<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="T"&er_data$Task=="L"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="T"&er_data$Task=="L"&er_data$Response=="IR")]
error_rates$Fluent_Target_Letter<-(incorrect/(correct+incorrect))*100


correct<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="T"&er_data$Task=="L"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="T"&er_data$Task=="L"&er_data$Response=="IR")]
error_rates$Disfluent_Target_Letter<-(incorrect/(correct+incorrect))*100


correct<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="N"&er_data$Task=="L"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="N"&er_data$Task=="L"&er_data$Response=="IR")]
error_rates$Fluent_NonTarget_Letter<-(incorrect/(correct+incorrect))*100

correct<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="N"&er_data$Task=="L"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="N"&er_data$Task=="L"&er_data$Response=="IR")]
error_rates$Disfluent_NonTarget_Letter<-(incorrect/(correct+incorrect))*100


correct<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="T"&er_data$Task=="W"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="T"&er_data$Task=="W"&er_data$Response=="IR")]
error_rates$Fluent_Target_Word<-(incorrect/(correct+incorrect))*100


correct<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="T"&er_data$Task=="W"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="T"&er_data$Task=="W"&er_data$Response=="IR")]
error_rates$Disfluent_Target_Word<-(incorrect/(correct+incorrect))*100


correct<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="N"&er_data$Task=="W"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="F"&er_data$Target=="N"&er_data$Task=="L"&er_data$Response=="IR")]
error_rates$Fluent_NonTarget_Word<-(incorrect/(correct+incorrect))*100

correct<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="N"&er_data$Task=="W"&er_data$Response=="CR")]
incorrect<-er_data$value[which(er_data$Fluency=="D"&er_data$Target=="N"&er_data$Task=="W"&er_data$Response=="IR")]
error_rates$Disfluent_NonTarget_Word<-(incorrect/(correct+incorrect))*100

#MAKE AN ERROR RATES MATRIX
error_rates$Average<-NA
error_rates$Average_L<-NA
error_rates$Average_W<-NA
error_rates<-as.matrix(error_rates)


#CALCULATE AVERAGE ERROR RATE PER PARTICIPANT AND IN EACH TASK
for(p in 1:39){
  error_rates[p,9]<-mean(error_rates[p,1:8], na.rm=T)
  error_rates[p,10]<-mean(error_rates[p,1:4], na.rm=T)
  error_rates[p,11]<-mean(error_rates[p,5:8], na.rm=T)
}

nt_error_rates<-error_rates[,c(3,4,7,8)]
nt_error_rates<-cbind(nt_error_rates, Average=0,Average_L=0,Average_W=0)

for(p in 1:39){
  nt_error_rates[p,5]<-mean(nt_error_rates[p,1:4], na.rm=T)
  nt_error_rates[p,6]<-mean(nt_error_rates[p,1:2], na.rm=T)
  nt_error_rates[p,7]<-mean(nt_error_rates[p,3:4], na.rm=T)
}


error_rates<-format(error_rates, digits=2, nsmall=2)
#write.csv(error_rates, file="error_rates.csv")
nt_error_rates<-format(nt_error_rates, digits=2, nsmall=2)
#write.csv(nt_error_rates, file="nt_error_rates.csv")

#Participant 22 has an error rate greater than 50% for non-targets


#SET DATA FILE NAME

filename<-"ALL_DATA.csv"

#LOAD DATA
all_data<-read.csv(filename)

#DELETE PARTICIPANT 22
all_data<-all_data[-22,]


##CHECK ALL THE DATA FOR OUTLIERS

#CREATE Z SCORE DATA FRAME
all_data_means<-colMeans(all_data,na.rm = TRUE)
all_data_means<-as.data.frame(all_data_means)
all_data_means<-t(all_data_means)


all_data_sds<-sapply(all_data[,1:length(all_data)],sd, na.rm = TRUE)
all_data_sds<-as.data.frame(all_data_sds)
all_data_sds<-t(all_data_sds)

zscores<-matrix(nrow = 38, ncol = 94)

for(c in 1:length(all_data)){
  for(r in 1:length(all_data$id)){
    zscores[r,c]<-(all_data[r,c]-all_data_means[,c])/all_data_sds[,c]
  }
}

colnames(zscores)<-colnames(all_data)


#IDENTIFY OUTLIERS
o_coords<-matrix(nrow = 200, ncol=2)
colnames(o_coords)<-c("row", "column")
o_coords<-as.data.frame(o_coords)
count<-0

for(c in 1:length(all_data)){
  for(r in 1:length(all_data$id)){
    if(is.na(zscores[r,c])){
    }else(
      if(zscores[r,c] > 3 | zscores[r,c] < -3){
        count<-count+1
        o_coords$row[count]<-r
        o_coords$column[count]<-c
      }
    )
  }
}
o_coords<-na.omit(o_coords)


#remove demographic coordinates
o_coords<-filter(.data = o_coords, o_coords$column > 4)


#NA outliers we don't want
for(l in 1:length(o_coords$row)){
  all_data[o_coords$row[l],o_coords$column[l]]<-NA
}

#remove WJ_RV_ANA with 99 value
all_data[27,13]<-NA

#CREATE DATA FRAME SO ANOVA WILL WORK

#CONVERT FROM WIDE TO LONG DATA
new_data<- all_data %>% pivot_longer(
  cols= w_p1a_f_l: l_sla_d_r,
  names_pattern = "(.*)_(.*)(.)_(.*)_(.*)",
  names_to = c("Task", "Component","Value_Type", "Fluency", "Hemisphere"),
  values_to = "value"
)

#Separate amplitudes and latencies
amplitudes<-filter(.data = new_data, new_data$Value_Type == "a")
latencies<-filter(.data = new_data, new_data$Value_Type == "l")


###ANOVAs

#Word task
p1_amps_w<-filter(amplitudes, amplitudes$Component == "p1" & amplitudes$Task == "w")

hist(p1_amps_w$value)

anova_p1a_w<-aov_ez(id = "id", dv = "value", data = p1_amps_w, within = c("Fluency","Hemisphere"))
anova_p1a_w

#check normality
hist(anova_p1a_w$lm$residuals)
shapiro.test(anova_p1a_w$lm$residuals)
skewness(anova_p1a_w$lm$residuals)
kurtosis(anova_p1a_w$lm$residuals)


#calculate mean and sd

erp<-"p1a_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p1a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)




p1_lats_w<-filter(latencies, latencies$Component == "p1" & latencies$Task == "w")

anova_p1l_w<-aov_ez(id = "id", dv = "value", data = p1_lats_w, within = c("Fluency","Hemisphere"))
anova_p1l_w

#check normality
hist(anova_p1l_w$lm$residuals)
shapiro.test(anova_p1l_w$lm$residuals)
skewness(anova_p1l_w$lm$residuals)
kurtosis(anova_p1l_w$lm$residuals)


#calculate mean and sd

erp<-"p1l_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p1l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)




n1_amps_w<-filter(amplitudes, amplitudes$Component == "n1"& amplitudes$Task == "w")

anova_n1a_w<-aov_ez(id = "id", dv = "value", data = n1_amps_w, within = c("Fluency","Hemisphere"))
anova_n1a_w

#check normality
hist(anova_n1a_w$lm$residuals)
shapiro.test(anova_n1a_w$lm$residuals)
skewness(anova_n1a_w$lm$residuals)
kurtosis(anova_n1a_w$lm$residuals)


#calculate mean and sd

erp<-"n1a_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

n1a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)



n1_lats_w<-filter(latencies, latencies$Component == "n1" & latencies$Task == "w")

anova_n1l_w<-aov_ez(id = "id", dv = "value", data = n1_lats_w, within = c("Fluency","Hemisphere"))
anova_n1l_w

#check normality
hist(anova_n1l_w$lm$residuals)
shapiro.test(anova_n1l_w$lm$residuals)
skewness(anova_n1l_w$lm$residuals)
kurtosis(anova_n1l_w$lm$residuals)


#calculate mean and sd

erp<-"n1l_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

n1l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)



p3_amps_w<-filter(amplitudes, amplitudes$Component == "p3"& amplitudes$Task == "w")

anova_p3a_w<-aov_ez(id = "id", dv = "value", data = p3_amps_w, within = c("Fluency","Hemisphere"))
anova_p3a_w

#check normality
hist(anova_p3a_w$lm$residuals)
shapiro.test(anova_p3a_w$lm$residuals)
skewness(anova_p3a_w$lm$residuals)
kurtosis(anova_p3a_w$lm$residuals)


#calculate mean and sd

erp<-"p3a_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p3a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)



p3_lats_w<-filter(latencies, latencies$Component == "p3" & latencies$Task == "w")

anova_p3l_w<-aov_ez(id = "id", dv = "value", data = p3_lats_w, within = c("Fluency","Hemisphere"))
anova_p3l_w

#check normality
hist(anova_p3l_w$lm$residuals)
shapiro.test(anova_p3l_w$lm$residuals)
skewness(anova_p3l_w$lm$residuals)
kurtosis(anova_p3l_w$lm$residuals)



#calculate mean and sd

erp<-"p3l_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p3l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)





sl_amps_w<-filter(amplitudes, amplitudes$Component == "sl"& amplitudes$Task == "w")

anova_sla_w<-aov_ez(id = "id", dv = "value", data = sl_amps_w, within = c("Fluency","Hemisphere"))
anova_sla_w

#check normality
hist(anova_sla_w$lm$residuals)
shapiro.test(anova_sla_w$lm$residuals)
skewness(anova_sla_w$lm$residuals)
kurtosis(anova_sla_w$lm$residuals)



#calculate mean and sd

erp<-"sla_w"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

sla<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)


dstatsw<-list(p1a=p1a,p1l=p1l,n1a=n1a,n1l=n1l,p3a=p3a,p3l=p3l,sla=sla)


#Letter Task

p1_amps_l<-filter(amplitudes, amplitudes$Component == "p1" & amplitudes$Task == "l")

anova_p1a_l<-aov_ez(id = "id", dv = "value", data = p1_amps_l, within = c("Fluency","Hemisphere"))
anova_p1a_l

#check normality
hist(anova_p1a_l$lm$residuals)
shapiro.test(anova_p1a_l$lm$residuals)
skewness(anova_p1a_l$lm$residuals)
kurtosis(anova_p1a_l$lm$residuals)



p1_lats_l<-filter(latencies, latencies$Component == "p1" & latencies$Task == "l")

anova_p1l_l<-aov_ez(id = "id", dv = "value", data = p1_lats_l, within = c("Fluency","Hemisphere"))
anova_p1l_l

#check normality
hist(anova_p1l_l$lm$residuals)
shapiro.test(anova_p1l_l$lm$residuals)
skewness(anova_p1l_l$lm$residuals)
kurtosis(anova_p1l_l$lm$residuals)



n1_amps_l<-filter(amplitudes, amplitudes$Component == "n1"& amplitudes$Task == "l")

anova_n1a_l<-aov_ez(id = "id", dv = "value", data = n1_amps_l, within = c("Fluency","Hemisphere"))
anova_n1a_l

#check normality
hist(anova_n1a_l$lm$residuals)
shapiro.test(anova_n1a_l$lm$residuals)
skewness(anova_n1a_l$lm$residuals)
kurtosis(anova_n1a_l$lm$residuals)



n1_lats_l<-filter(latencies, latencies$Component == "n1" & latencies$Task == "l")

anova_n1l_l<-aov_ez(id = "id", dv = "value", data = n1_lats_l, within = c("Fluency","Hemisphere"))
anova_n1l_l

#check normality
hist(anova_n1l_l$lm$residuals)
shapiro.test(anova_n1l_l$lm$residuals)
skewness(anova_n1l_l$lm$residuals)
kurtosis(anova_n1l_l$lm$residuals)



p3_amps_l<-filter(amplitudes, amplitudes$Component == "p3"& amplitudes$Task == "l")

anova_p3a_l<-aov_ez(id = "id", dv = "value", data = p3_amps_l, within = c("Fluency","Hemisphere"))
anova_p3a_l

#check normality
hist(anova_p3a_l$lm$residuals)
shapiro.test(anova_p3a_l$lm$residuals)
skewness(anova_p3a_l$lm$residuals)
kurtosis(anova_p3a_l$lm$residuals)


p3_lats_l<-filter(latencies, latencies$Component == "p3" & latencies$Task == "l")

anova_p3l_l<-aov_ez(id = "id", dv = "value", data = p3_lats_l, within = c("Fluency","Hemisphere"))
anova_p3l_l


#check normality
hist(anova_p3l_l$lm$residuals)
shapiro.test(anova_p3l_l$lm$residuals)
skewness(anova_p3l_l$lm$residuals)
kurtosis(anova_p3l_l$lm$residuals)


sl_amps_l<-filter(amplitudes, amplitudes$Component == "sl"& amplitudes$Task == "l")

anova_sla_l<-aov_ez(id = "id", dv = "value", data = sl_amps_l, within = c("Fluency","Hemisphere"))
anova_sla_l

#check normality
hist(anova_sla_l$lm$residuals)
shapiro.test(anova_sla_l$lm$residuals)
skewness(anova_sla_l$lm$residuals)
kurtosis(anova_sla_l$lm$residuals)


#calculate mean and sd

erp<-"p1a_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p1a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)


erp<-"p1l_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p1l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)



erp<-"n1a_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

n1a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)

erp<-"n1l_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

n1l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)


erp<-"p3a_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p3a<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)


erp<-"p3l_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

p3l<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)


erp<-"sla_l"
df<-get(paste("anova_",erp, sep = ""))
mfl<-mean(df$data$wide$f_l)
mfr<-mean(df$data$wide$f_r)
mdl<-mean(df$data$wide$d_l)
mdr<-mean(df$data$wide$d_r)
sfl<-sd(df$data$wide$f_l)
sfr<-sd(df$data$wide$f_r)
sdl<-sd(df$data$wide$d_l)
sdr<-sd(df$data$wide$d_r)

sla<-data.frame(mfl,mfr,mdl,mdr,sfl,sfr,sdl,sdr)



dstatsl<-list(p1a=p1a,p1l=p1l,n1a=n1a,n1l=n1l,p3a=p3a,p3l=p3l,sla=sla)



#POST-HOC COMPARISONS FOR FLUENCY:HEMISPHERE INTERACTION FOR P3 LATS IN THE LETTER TASK
contrasting <- emmeans(anova_p3l_l, ~ Fluency*Hemisphere)
contrast(contrasting, method =  "pairwise", adjust = "none")
eff_size(contrasting, sigma = mean(sigma(anova_p3l_l$lm)), edf=df.residual(anova_p3l_l$lm))

#MAKE BOXPLOT TO CHECK INTERACTION

#boxplot
anova_p3l_l$data$long$interaction<-interaction(anova_p3l_l$data$long$Fluency, anova_p3l_l$data$long$Hemisphere, sep=":")

boxplot<-ggplot(aes(y = value, x = interaction), data = anova_p3l_l$data$long) + 
  geom_boxplot()
boxplot


## check mean and sd for p3 lats
mean(p3_lats_l$value, na.rm = TRUE)
sd(p3_lats_l$value, na.rm = TRUE)

mean(p3_lats_w$value, na.rm = TRUE)
sd(p3_lats_w$value, na.rm = TRUE)



#Create matrices for each outcome variable
test<-all_data[c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
dis_amps_w_l<-test[6:9]
dis_lats_l_l<-test[10:12]


test<-all_data[c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
dis_amps_w_r<-test[6:9]
dis_lats_l_r<-test[10:12]

test<-all_data[c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)]
flu_amps_l_l<-test[9:12]
flu_lats_w_l<-test[6:8]

test<-all_data[c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)]
flu_amps_l_r<-test[9:12]
flu_lats_w_r<-test[6:8]

test<-all_data[c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)]
dis_amps_l_l<-test[9:12]
dis_lats_w_l<-test[6:8]

test<-all_data[c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)]
dis_amps_l_r<-test[9:12]
dis_lats_w_r<-test[6:8]

test<-all_data[c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
flu_amps_w_l<-test[5:8]
flu_lats_l_l<-test[9:11]

test<-all_data[c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)]
flu_amps_w_r<-test[5:8]
flu_lats_l_r<-test[9:11]


#CALCULATE DIFFERENCES BETWEEN FLUENT AND DISFLUENT AMPLITUDES FOR EACH COMPONENT AND CORRELATE WITH BEHAVIOURAL DATA
behavioural_data<-all_data[3:38]

correlations<-list()


diff_amps_w_l<- (dis_amps_w_l - flu_amps_w_l)
colnames(diff_amps_w_l)<-c("P1","N1","P3","SLA")
correlations$diff_amps_w_l_cor<-corr.test(behavioural_data,diff_amps_w_l, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_amps_w_l_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



diff_amps_l_l<- (dis_amps_l_l - flu_amps_l_l)
colnames(diff_amps_l_l)<-c("P1","N1","P3","SLA")
correlations$diff_amps_l_l_cor<-corr.test(behavioural_data,diff_amps_l_l, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_amps_l_l_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



diff_amps_w_r<- (dis_amps_w_r - flu_amps_w_r)
colnames(diff_amps_w_r)<-c("P1","N1","P3","SLA")
correlations$diff_amps_w_r_cor<-corr.test(behavioural_data,diff_amps_w_r, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_amps_w_r_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



diff_amps_l_r<- (dis_amps_l_r - flu_amps_l_r)
colnames(diff_amps_l_r)<-c("P1","N1","P3","SLA")
correlations$diff_amps_l_r_cor<-corr.test(behavioural_data,diff_amps_l_r, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_amps_l_r_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



diff_lats_w_l<- (dis_lats_w_l - flu_lats_w_l)
colnames(diff_lats_w_l)<-c("P1","N1","P3")
correlations$diff_lats_w_l_cor<-corr.test(behavioural_data,diff_lats_w_l, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_lats_w_l_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



diff_lats_l_l<- (dis_lats_l_l - flu_lats_l_l)
colnames(diff_lats_l_l)<-c("P1","N1","P3")
correlations$diff_lats_l_l_cor<-corr.test(behavioural_data,diff_lats_l_l, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_lats_l_l_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)


diff_lats_w_r<- (dis_lats_w_r - flu_lats_w_r)
colnames(diff_lats_w_r)<-c("P1","N1","P3")
correlations$diff_lats_w_r_cor<-corr.test(behavioural_data,diff_lats_w_r, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_lats_w_r_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)


diff_lats_l_r<- (dis_lats_l_r - flu_lats_l_r)
colnames(diff_lats_l_r)<-c("P1","N1","P3")
correlations$diff_lats_l_r_cor<-corr.test(behavioural_data,diff_lats_l_r, adjust="none")

#CORRELATION P-VALUE MATRIX
corrplot(correlations$diff_lats_l_r_cor$p[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(0,1)
)



### CALCULATE MEANS AND SDS FOR HYPOTHESES TESTING

#1:

#CALCULATE MEAN AND SD SL AMPLITUDE FOR FLUENT AND DISFLUENT FONTS FOR THE WORD TASK
#disfluent
(mean(anova_sla_w$data$wide$d_l)+mean(anova_sla_w$data$wide$d_r))/2
(sd(anova_sla_w$data$wide$d_l)+sd(anova_sla_w$data$wide$d_r))/2

#fluent
(mean(anova_sla_w$data$wide$f_l)+mean(anova_sla_w$data$wide$f_r))/2
(sd(anova_sla_w$data$wide$f_l)+sd(anova_sla_w$data$wide$f_r))/2


#CALCULATE MEAN AND SD AMPLITUDE FOR LEFT AND RIGHT HEMISPHERE FOR N1 WORD TASK

right_n1a_w<- filter(n1_amps_w, n1_amps_w$Hemisphere == "r")
left_n1a_w<- filter(n1_amps_w, n1_amps_w$Hemisphere == "l")

mean(left_n1a_w$value, na.rm=T)
sd(left_n1a_w$value, na.rm=T)

mean(right_n1a_w$value, na.rm=T)
sd(right_n1a_w$value, na.rm=T)


#CHECK NUMBER OF VALUES ARE CONSISTENT

which(left_n1a_w$value %in% NA)
which(right_n1a_w$value %in% NA)


#CORRELATE DISFLUENT AND FLUENT AMPLITUDES SEPARATLEY

#set variables
hem<-c("l","r")
task<-c("w","l")
fluency<-c("dis","flu")
value<-c("amps","lats")

correlations_2<-list()
count<-0
corobject_c<-c()

for(v in 1:length(value)){
  for(f in 1:length(fluency)){
    for(t in 1:length(task)){
      for(h in 1:length(hem)){
        count<-count+1
        object<-paste(fluency[f], value[v], task[t], hem[h], sep="_")
        corobject<-paste(object,"cor", sep="_")
        
        corobject_c[count]<-paste(object,"cor", sep="_")
        correlations_2[[corobject]]<-corr.test(behavioural_data, get(object),adjust="none")
        
        correlations_2[[corobject]]$plot<-
          corrplot(correlations_2[[corobject]]$p[3:17,], method = "number", 
                   tl.cex=0.8,
                   cl.ratio=0.4,
                   col = brewer.pal(n = 10, name = 'Dark2'),
                   cl.lim= c(0,1)
          )
      }
    }
  }
}


correlations_2[[corobject]]$plot<-
  corrplot(correlations_2[[corobject_c[8]]]$p[3:17,], method = "number", 
           tl.cex=0.8,
           cl.ratio=0.4,
           col = brewer.pal(n = 10, name = 'Dark2'),
           cl.lim= c(0,1)
  )


#fluent letter amplitudes on right hemisphere have a low p-val for WJ
plot(flu_amps_l_r$l_p1a_f_r,behavioural_data$WJ_Tot)





corrplot(correlations$diff_amps_l_r_cor$r[3:17,], method = "number", 
         tl.cex=0.8,
         cl.ratio=0.4,
         col = brewer.pal(n = 10, name = 'Dark2'),
         cl.lim= c(-1,1)
)


plot(diff_lats_l_r$P3,behavioural_data$WJ_PC)



