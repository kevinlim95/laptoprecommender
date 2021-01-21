DF <- read.csv(file = '3 - raw.csv', header = TRUE)
head(DF)

library(stringr)
library(qdapRegex)
library(tidyverse)
library(stringi)
library(gsubfn)
library(proto)

DF <- DF
head(DF)

#To remove characters and parse strings into numerics
DF$Weight_KG <- as.numeric(unlist(regmatches(DF$Weight,(gregexpr("[[:digit:]]+\\.*[[:digit:]]*",DF$Weight)))))

DF$Ram_GB <- as.numeric(unlist(regmatches(DF$Ram,(gregexpr("[[:digit:]]+\\.*[[:digit:]]*",DF$Ram)))))

#To assign dummy values to categorical variables
DF[grep("IPS",DF$ScreenResolution,value = F),"IPS"] <- as.numeric(1)

DF[grep("Full HD",DF$ScreenResolution,value = F),"Full HD"] <- 1

DF[grep("Touchscreen",DF$ScreenResolution,value = F),"Touchscreen"] <- 1

DF[grep("4K",DF$ScreenResolution,value = F),"Four_K"] <- 1

DF[grep("HDD",DF$Memory,value = F),"HDD"] <- 1

DF[grep("SSD",DF$Memory,value = F),"SSD"] <- 1

DF[grep("Flash Storage",DF$Memory,value = F),"Flash_Storage"] <- 1

DF$IPS <- replace(DF$IPS, is.na(DF$IPS), 0)

DF$`Full HD` <- replace(DF$`Full HD`, is.na(DF$`Full HD`), 0)

DF$Touchscreen <- replace(DF$`Touchscreen`, is.na(DF$`Touchscreen`), 0)

DF$Four_K <- replace(DF$FOur_K, is.na(DF$Four_K), 0)

DF$HDD <- replace(DF$HDD, is.na(DF$HDD), 0)

DF$SSD <- replace(DF$SSD, is.na(DF$SSD), 0)

DF$Flash_Storage <- replace(DF$Flash_Storage, is.na(DF$Flash_Storage), 0)

DF$Memory_GB <- str_remove_all(DF$Memory,"[a-zA-Z ]")

DF$Memory_GB <- as.numeric(str_remove_all(DF$Memory_GB,"\\+"))

DF$Price_RM <- DF$Price_euros*4.96


library(magrittr)
library(dplyr)

#To display resolution size into new column
DF<-DF%>%mutate(ResolutionSize=substr(ScreenResolution, nchar(ScreenResolution)-9+1,nchar(ScreenResolution)))

#To remove non-required columns for analysis
DF <- DF[,-c(1,6,8,9,12,13)]

write.csv(DF, "check.csv")


#To assign ranking for CPU (consists of number of CPU cores, version of processor and size of processor base)
DF<-DF
head(DF)

library(stringi)

DF$Cpu2<-str_remove(DF$Cpu,word(DF$Cpu,-1))

head(DF)  

DF<-arrange(DF,Cpu2)
DF
ss<-sort(unique(DF$Cpu2))
ss
class(ss)
ss<-data.frame(ss)
class(ss)

#remove duplication of CPUs in step above and import to csv to be ranked using domain expertise from official laptop company site
write.csv(ss, "ss.csv")

ScoreCPU<-read.csv(file = 'ScoreCPU.csv', header = TRUE)
class(ScoreCPU)

CPU_Ranked=cbind(ss,ScoreCPU$ï..Score)
CPU_Ranked

DF<-left_join(DF, CPU_Ranked, by = c("Cpu2" = "ss"),all.x=TRUE)
dim(DF)

Processor_base<-word(DF$Cpu,-1)
DF<-mutate(DF,Processor_base)
head(DF)
dim(DF)

DF<-arrange(DF,Processor_base)
DF
dim(DF)
ss2<-sort(unique(DF$Processor_base))
ss2
class(ss2)
ss2<-data.frame(ss2)
class(ss2)

#remove duplication of Processor Bases in step above and import to csv to be ranked using domain expertise from official laptop company site
write.csv(ss2, "ss2.csv")

ScorePB<-read.csv(file = 'ScorePB.csv', header = TRUE)

PB_Ranked=cbind(ss2,ScorePB$ï..Score)
PB_Ranked

DF<-left_join(DF, PB_Ranked, by = c("Processor_base" = "ss2"),all.x=TRUE)
head(DF)
dim(DF)

Total_CPU_Score<-DF$`ScoreCPU$ï..Score` + 0.5*DF$`ScorePB$ï..Score` - 0.5

DF=cbind(DF,Total_CPU_Score)

CPU_Purpose<-read.csv(file = 'CPU_Purpose.csv', header = TRUE)
CPU_Purpose

DF<-left_join(DF, CPU_Purpose, by = c("Total_CPU_Score" = "ï..CPU_Purpose"),all.x=TRUE)
head(DF)

write.csv(DF, "DF.csv")

#To assign ranking for Graphics Processing Unit (GPU)

DF<-arrange(DF,Gpu)
DF
ss3<-sort(unique(DF$Gpu))
ss3
class(ss3)
ss3<-data.frame(ss3)
class(ss3)

#remove duplication of GPUs in step above and import to csv to be ranked using domain expertise from official laptop company site
write.csv(ss3, "ss3.csv")

ScoreGPU<-read.csv(file = 'GPU_Score.csv', header = TRUE)
class(ScoreGPU)
head(ScoreGPU)

GPU_Ranked=cbind(ss3,ScoreGPU$ï..ScoreGPU)
GPU_Ranked

DF<-left_join(DF, GPU_Ranked, by = c("Gpu" = "ss3"),all.x=TRUE)
dim(DF)

write.csv(DF, "Final.csv")
