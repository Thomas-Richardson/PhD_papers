library(tidyverse)
library(magrittr)

d1=read.csv2("recalibration_sample1.csv",header = T,dec = ",",fill = TRUE,strip.white = TRUE, stringsAsFactors=TRUE)
d1=filter(d1,sex==1)

d2=read.csv2("recalibration_sample2.csv", header = T, dec = ",", fill = TRUE, strip.white = TRUE,stringsAsFactors=TRUE)

d2$Height=sqrt(d2$weight/d2$BMI)

d1b=select(d1, Height, fmasculinity,grip_max)

d2b=select(d2, Height, fac.masc_SebSil_Yasm,handgrMax)
d2b %<>% rename(fmasculinity=fac.masc_SebSil_Yasm,grip_max=handgrMax)

d3= rbind(d2b,d1b)

cor.test(d3$Height,d3$fmasculinity,method='spearman')
cor.test(d3$grip_max,d3$fmasculinity,method='spearman')

d3$formidability=scale(d3$Height)+scale(d3$grip_max)
cor.test(d3$formidability,d3$fmasculinity,method='spearman')
