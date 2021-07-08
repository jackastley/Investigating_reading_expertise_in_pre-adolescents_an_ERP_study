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


#SET WORKING DIRECTORY

setwd("//rdfs.unisa.edu.au/Group_bbb_research/CAIN/PEOPLE/JackA/Typeface_EEG/Data_Analysis")

#SET DATA FILE NAME

filename<-"ALL_DATA.csv"

#LOAD DATA

all_data<-read.csv(filename)


##CHECK ALL THE DATA FOR OUTLIERS

#CREATE Z SCORE DATA FRAME

zscores<- (all_data[1:length(all_data),]-mean(all_data[1:length(all_data)]))/sd(all_data[1:length(all_data)])

all_data_means<-colMeans(all_data,na.rm = TRUE)
all_data_means<-as.data.frame(all_data_means)
all_data_means<-t(all_data_means)


all_data_sds<-sapply(all_data[,1:length(all_data)],sd, na.rm = TRUE)
all_data_sds<-as.data.frame(all_data_sds)
all_data_sds<-t(all_data_sds)

zscores<-matrix(nrow = 39, ncol = 94)

for(c in 1:length(all_data)){
  for(r in 1:length(all_data$id)){
    zscores[r,c]<-(all_data[r,c]-all_data_means[,c])/all_data_sds[,c]
  }
}

colnames(zscores)<-colnames(all_data)


#IDENTIFY OUTLIERS
o_coords<-matrix(nrow = 3666, ncol=2)
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

#remove behavioural data coordinates
o_coords<-filter(.data = o_coords, o_coords$column > 38)

#NA outliers we don't want
all_data[o_coords$row,o_coords$column]<-NA


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



p1_lats_w<-filter(latencies, latencies$Component == "p1" & latencies$Task == "w")

anova_p1l_w<-aov_ez(id = "id", dv = "value", data = p1_lats_w, within = c("Fluency","Hemisphere"))
anova_p1l_w

#check normality
hist(anova_p1l_w$lm$residuals)
shapiro.test(anova_p1l_w$lm$residuals)
skewness(anova_p1l_w$lm$residuals)
kurtosis(anova_p1l_w$lm$residuals)


n1_amps_w<-filter(amplitudes, amplitudes$Component == "n1"& amplitudes$Task == "w")

anova_n1a_w<-aov_ez(id = "id", dv = "value", data = n1_amps_w, within = c("Fluency","Hemisphere"))
anova_n1a_w

#check normality
hist(anova_n1a_w$lm$residuals)
shapiro.test(anova_n1a_w$lm$residuals)
skewness(anova_n1a_w$lm$residuals)
kurtosis(anova_n1a_w$lm$residuals)


n1_lats_w<-filter(latencies, latencies$Component == "n1" & latencies$Task == "w")

anova_n1l_w<-aov_ez(id = "id", dv = "value", data = n1_lats_w, within = c("Fluency","Hemisphere"))
anova_n1l_w

#check normality
hist(anova_n1l_w$lm$residuals)
shapiro.test(anova_n1l_w$lm$residuals)
skewness(anova_n1l_w$lm$residuals)
kurtosis(anova_n1l_w$lm$residuals)



p3_amps_w<-filter(amplitudes, amplitudes$Component == "p3"& amplitudes$Task == "w")

anova_p3a_w<-aov_ez(id = "id", dv = "value", data = p3_amps_w, within = c("Fluency","Hemisphere"))
anova_p3a_w

#check normality
hist(anova_p3a_w$lm$residuals)
shapiro.test(anova_p3a_w$lm$residuals)
skewness(anova_p3a_w$lm$residuals)
kurtosis(anova_p3a_w$lm$residuals)



p3_lats_w<-filter(latencies, latencies$Component == "p3" & latencies$Task == "w")

anova_p3l_w<-aov_ez(id = "id", dv = "value", data = p3_lats_w, within = c("Fluency","Hemisphere"))
anova_p3l_w

#check normality
hist(anova_p3l_w$lm$residuals)
shapiro.test(anova_p3l_w$lm$residuals)
skewness(anova_p3l_w$lm$residuals)
kurtosis(anova_p3l_w$lm$residuals)


sl_amps_w<-filter(amplitudes, amplitudes$Component == "sl"& amplitudes$Task == "w")

anova_sla_w<-aov_ez(id = "id", dv = "value", data = sl_amps_w, within = c("Fluency","Hemisphere"))
anova_sla_w

#check normality
hist(anova_sla_w$lm$residuals)
shapiro.test(anova_sla_w$lm$residuals)
skewness(anova_sla_w$lm$residuals)
kurtosis(anova_sla_w$lm$residuals)


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
corrplot(correlations$diff_amps_w_l_cor$r[1:10,], method = "number", tl.cex=0.8, number.cex = 0.5, cl.cex = 0.5,
         cl.ratio=0.4)

diff_amps_l_l<- (dis_amps_l_l - flu_amps_l_l)
colnames(diff_amps_l_l)<-c("P1","N1","P3","SLA")
correlations$diff_amps_l_l_cor<-corr.test(behavioural_data,diff_amps_l_l, adjust="none")

diff_amps_w_r<- (dis_amps_w_r - flu_amps_w_r)
colnames(diff_amps_w_r)<-c("P1","N1","P3","SLA")
correlations$diff_amps_w_r_cor<-corr.test(behavioural_data,diff_amps_w_r, adjust="none")


diff_amps_l_r<- (dis_amps_l_r - flu_amps_l_r)
colnames(diff_amps_l_r)<-c("P1","N1","P3","SLA")
correlations$diff_amps_l_r_cor<-corr.test(behavioural_data,diff_amps_l_r, adjust="none")




diff_lats_w_l<- (dis_lats_w_l - flu_lats_w_l)
colnames(diff_lats_w_l)<-c("P1","N1","P3")
correlations$diff_lats_w_l_cor<-corr.test(behavioural_data,diff_lats_w_l, adjust="none")


diff_lats_l_l<- (dis_lats_l_l - flu_lats_l_l)
colnames(diff_lats_l_l)<-c("P1","N1","P3")
correlations$diff_lats_l_l_cor<-corr.test(behavioural_data,diff_lats_l_l, adjust="none")

diff_lats_w_r<- (dis_lats_w_r - flu_lats_w_r)
colnames(diff_lats_w_r)<-c("P1","N1","P3")
correlations$diff_lats_w_r_cor<-corr.test(behavioural_data,diff_lats_w_r, adjust="none")

diff_lats_l_r<- (dis_lats_l_r - flu_lats_l_r)
colnames(diff_lats_l_r)<-c("P1","N1","P3")
correlations$diff_lats_l_r_cor<-corr.test(behavioural_data,diff_lats_l_r, adjust="none")
