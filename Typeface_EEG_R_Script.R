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


# #COMBINE LEFT AND RIGHT HEMISPHERES

# l_amps<-filter(.data = amplitudes, amplitudes$Hemisphere == "l")
# r_amps<-filter(.data = amplitudes, amplitudes$Hemisphere == "r")
# 
# mean_amps<-(l_amps$value+r_amps$value)/2
# 
# new_amps<- subset(l_amps, select = -Hemisphere)
# new_amps$value<-mean_amps
# 
# 
# latencies<-filter(.data = new_data, new_data$Value_Type == "l")
# l_lats<-filter(.data = latencies, latencies$Hemisphere == "l")
# r_lats<-filter(.data = latencies, latencies$Hemisphere == "r")
# 
# mean_lats<-(l_lats$value+r_lats$value)/2
# 
# new_lats<- subset(l_lats, select = -Hemisphere)
# new_lats$value<-mean_lats


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




# ### CALCULATE DIFFERENCE BETWEEN FLUENT AND DISFLUENT AMPLITUDES
# p1_amps_w_f<-filter(.data = p1_amps_w, p1_amps_w$Fluency == "f")
# p1_amps_w_d<-filter(.data = p1_amps_w, p1_amps_w$Fluency == "d")
# p1_amps_w_dif<-p1_amps_w_f$value - p1_amps_w_d$value
# p1_amps_w_dif_l<-p1_amps_w_dif[c(TRUE, FALSE)]
# p1_amps_w_dif_r<-p1_amps_w_dif[c(FALSE, TRUE)]
# 
# 
# p1_lats_w_f<-filter(.data = p1_lats_w, p1_lats_w$Fluency == "f")
# p1_lats_w_d<-filter(.data = p1_lats_w, p1_lats_w$Fluency == "d")
# p1_lats_w_dif<-p1_lats_w_f$value - p1_lats_w_d$value
# p1_amps_w_dif_l<-p1_amps_w_dif[c(TRUE, FALSE)]
# p1_amps_w_dif_r<-p1_amps_w_dif[c(FALSE, TRUE)]
# 
# 
# p1_amps_l_f<-filter(.data = p1_amps_l, p1_amps_l$Fluency == "f")
# p1_amps_l_d<-filter(.data = p1_amps_l, p1_amps_l$Fluency == "d")
# p1_amps_l_dif<-p1_amps_l_f$value - p1_amps_l_d$value
# p1_amps_w_dif_l<-p1_amps_w_dif[c(TRUE, FALSE)]
# p1_amps_w_dif_r<-p1_amps_w_dif[c(FALSE, TRUE)]
# 
# 
# p1_lats_l_f<-filter(.data = p1_lats_l, p1_lats_l$Fluency == "f")
# p1_lats_l_d<-filter(.data = p1_lats_l, p1_lats_l$Fluency == "d")
# p1_lats_l_dif<-p1_lats_l_f$value - p1_lats_l_d$value
# p1_amps_w_dif_l<-p1_amps_w_dif[c(TRUE, FALSE)]
# p1_amps_w_dif_r<-p1_amps_w_dif[c(FALSE, TRUE)]
# 
# 
# 
# 
# n1_amps_w_f<-filter(.data = n1_amps_w, n1_amps_w$Fluency == "f")
# n1_amps_w_d<-filter(.data = n1_amps_w, n1_amps_w$Fluency == "d")
# n1_amps_w_dif<-n1_amps_w_f$value - n1_amps_w_d$value
# 
# 
# n1_lats_w_f<-filter(.data = n1_lats_w, n1_lats_w$Fluency == "f")
# n1_lats_w_d<-filter(.data = n1_lats_w, n1_lats_w$Fluency == "d")
# n1_lats_w_dif<-n1_lats_w_f$value - n1_lats_w_d$value
# 
# 
# n1_amps_l_f<-filter(.data = n1_amps_l, n1_amps_l$Fluency == "f")
# n1_amps_l_d<-filter(.data = n1_amps_l, n1_amps_l$Fluency == "d")
# n1_amps_l_dif<-n1_amps_l_f$value - n1_amps_l_d$value
# 
# 
# n1_lats_l_f<-filter(.data = n1_lats_l, n1_lats_l$Fluency == "f")
# n1_lats_l_d<-filter(.data = n1_lats_l, n1_lats_l$Fluency == "d")
# n1_lats_l_dif<-n1_lats_l_f$value - n1_lats_l_d$value
# 
# 
# 
# 
# p3_amps_w_f<-filter(.data = p3_amps_w, p3_amps_w$Fluency == "f")
# p3_amps_w_d<-filter(.data = p3_amps_w, p3_amps_w$Fluency == "d")
# p3_amps_w_dif<-p3_amps_w_f$value - p3_amps_w_d$value
# 
# 
# p3_lats_w_f<-filter(.data = p3_lats_w, p3_lats_w$Fluency == "f")
# p3_lats_w_d<-filter(.data = p3_lats_w, p3_lats_w$Fluency == "d")
# p3_lats_w_dif<-p3_lats_w_f$value - p3_lats_w_d$value
# 
# 
# p3_amps_l_f<-filter(.data = p3_amps_l, p3_amps_l$Fluency == "f")
# p3_amps_l_d<-filter(.data = p3_amps_l, p3_amps_l$Fluency == "d")
# p3_amps_l_dif<-p3_amps_l_f$value - p3_amps_l_d$value
# 
# 
# p3_lats_l_f<-filter(.data = p3_lats_l, p3_lats_l$Fluency == "f")
# p3_lats_l_d<-filter(.data = p3_lats_l, p3_lats_l$Fluency == "d")
# p3_lats_l_dif<-p3_lats_l_f$value - p3_lats_l_d$value
# 
# 
# 
# sl_amps_w_f<-filter(.data = sl_amps_w, sl_amps_w$Fluency == "f")
# sl_amps_w_d<-filter(.data = sl_amps_w, sl_amps_w$Fluency == "d")
# sl_amps_w_dif<-sl_amps_w_f$value - sl_amps_w_d$value
# 
# sl_amps_l_f<-filter(.data = sl_amps_l, sl_amps_l$Fluency == "f")
# sl_amps_l_d<-filter(.data = sl_amps_l, sl_amps_l$Fluency == "d")
# sl_amps_l_dif<-sl_amps_l_f$value - sl_amps_l_d$value
# 
# 


test<-all_data[c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
dis_amps_w_l<-test[6:9]
dis_lats_l_l<-test[10:12]



## mean and sd for p3 lats
mean(p3_lats_l$value, na.rm = TRUE)
sd(p3_lats_l$value, na.rm = TRUE)

mean(p3_lats_w$value, na.rm = TRUE)
sd(p3_lats_w$value, na.rm = TRUE)