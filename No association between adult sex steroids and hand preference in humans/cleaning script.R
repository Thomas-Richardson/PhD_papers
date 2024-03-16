library(tidyverse)
library(magrittr)
library(SASxport)

# =============================================================

# download 2011-2012 data

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/TST_G.XPT','NHANES2011_T.xpt',mod='wb')
data_T= read.xport('NHANES2011_T.xpt') %>% select(SEQN, LBXTST) %>% rename(T = LBXTST) # get their ID number and testosterone

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MGX_G.XPT','NHANES2011_grip.xpt',mod='wb')
data_handedness= read.xport('NHANES2011_grip.xpt') %>% select(SEQN,MGD130) %>% rename(handedness=MGD130) # get their ID number and handedness. The file is a grip strength test, which is why it's called '*_grip'

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT','NHANES2011_demographics.xpt',mod='wb')
data_demographics= read.xport('NHANES2011_demographics.xpt') %>% select(SEQN,RIAGENDR,RIDAGEYR) %>% rename(sex = RIAGENDR, age = RIDAGEYR)
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_H.htm#INDHHIN2 # get their ID number, sex and age

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/FASTQX_G.XPT','NHANES2011_session.xpt',mod='wb')
data_session= read.xport('NHANES2011_session.xpt') %>% select(SEQN,PHDSESN) %>% rename(session_time=PHDSESN) # get data on when their hormone samples were taken, as testosterone changes a lot during the day.

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/RHQ_G.XPT','NHANES2011_period.xpt',mod='wb')
data_periods = read.xport('NHANES2011_period.xpt') %>% select(SEQN,RHQ031) %>% rename(regular_periods=RHQ031) # we only want premenopausal women so get data on whether they have regular periods as a proxy.

data1 = data_demographics %>% inner_join(data_handedness,by='SEQN')%>% 
  inner_join(data_T,by='SEQN') %>% 
  inner_join(data_session,by='SEQN') %>% 
  left_join(data_periods,by='SEQN')

data1$year = 2011 # create a new column year, and set it equal to 2011
data1$E = -1 # set their estradiol E to -1
data1$regular_periods[data1$sex==1]= -1 # set men's regular periods data to -1 because it's not relevant 
data1$regular_periods[is.na(data1$regular_periods)|data1$regular_periods==9]= -1 # Those with regular period scores of NA or 9 (refused to answer) are set to -1
data1 = data1[data1$age>= 18,] # filter those under age 18
data1 %>% nrow # we have 5615 people

data1 %$% table(sex,regular_periods) # check this makes sense: all men have it set to -1, women it varies.

# download 2013-2014 data

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TST_H.XPT','NHANES2013_T.xpt',mod='wb')
data_T2= read.xport('NHANES2013_T.xpt') %>% select(SEQN, LBXTST,LBXEST) %>% rename(T=LBXTST,E=LBXEST)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MGX_H.XPT','NHANES2013_grip.xpt',mod='wb')
data_handedness2= read.xport('NHANES2013_grip.xpt') %>% select(SEQN,MGD130) %>% rename(handedness=MGD130)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT','NHANES2013_demographics.xpt',mod='wb')
data_demographics2= read.xport('NHANES2013_demographics.xpt') %>% select(SEQN,RIAGENDR,RIDAGEYR) %>% rename(sex = RIAGENDR, age = RIDAGEYR)
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm#INDHHIN2

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/FASTQX_H.XPT','NHANES2013_session.xpt',mod='wb')
data_session2= read.xport('NHANES2013_session.xpt') %>% select(SEQN,PHDSESN) %>% rename(session_time=PHDSESN)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/RHQ_H.XPT','NHANES2013_period.xpt',mod='wb')
data_periods2 = read.xport('NHANES2013_period.xpt') %>% select(SEQN,RHQ031) %>% rename(regular_periods=RHQ031)

data2 = data_demographics2 %>% 
  inner_join(data_handedness2,by='SEQN')%>% 
  inner_join(data_T2,by='SEQN') %>% 
  inner_join(data_session2,by='SEQN') %>% 
  left_join(data_periods2,by='SEQN')

data2$year = 2013
data2$regular_periods[data2$sex==1]= -1
data2$regular_periods[is.na(data2$regular_periods)|data2$regular_periods==9]= -1
data2 = data2[data2$age>= 18,]
data2 %>% nrow # 5924

data2 %$% table(sex,regular_periods) # check this makes sense: all men have it set to -1, women it varies.

# now bring them together

data = rbind(data1,data2) %>% na.omit # chuck out anyone who we don't have incomplete data for.
data %>% nrow # how much data do we have? 9708

data$sex %<>% factor
levels(data$sex)= c('male','female')

data$E[data$E<0]=NaN # now anyone whose E number is set below 1 gets an NaN

data$handedness %<>%  factor()
levels(data$handedness) = c('Right-handed','Left-handed','Mixed-handed') # give the handedness variable common sense labels

data %>% nrow # last check that we haven't accidentally lost data in the last few lines.

write.csv(data, 'handedness_hormone_data_cleaned.csv',row.names = F)
