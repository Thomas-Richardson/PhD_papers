library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(purrr)
library(ggplot2)

data = read_csv('ESS7.csv') %>% 
  select(cntry,gndr,agea,gincdif,hinctnta,lrscale,jbspv,eduyrs,height) # these are the columns we want

colnames(data) # take a look at the data so we can see if columns are named in an easy to understand way

data %<>% rename(country=cntry,
                 sex=gndr,
                 age=agea,
                 government_equality=gincdif,
                 household_income=hinctnta,
                 politics_lr=lrscale,
                 years_education=eduyrs,
                 supervisor=jbspv
                 ) 
# rename variables so we know what is going on


# plot all numeric variables as density plots to see if there are any weird values

data %>% 
  keep(is.numeric) %>% # keep numeric variables
  gather() %>%  # the rest of this code is a crude hack to get it to make dens plots for all numeric variables. 
  ggplot(aes(value))+facet_wrap(~key,scales='free')+
  geom_density(fill='darkolivegreen', alpha=1) 

# The density plots show weird tails, indicating that there are very extreme values. 
# This either means someone input the data wrong, or more likely that non responses are being coded as 99s or similar. 

# ==============================================================

# Many questions allowed participants to answer "don't know" or refuse to answer, or not respond. These need to be take out.
# here I code then all with NaNs so they get left out the models.

data$sex[data$sex>2]=NaN # people who aren't male or female

data$government_equality[data$government_equality>6]=NaN
data$government_equality=6-data$government_equality # recode support for goverment redistribution of wealth the other way around so it makes more sense

data$supervisor[data$supervisor>2]=NaN

data$height[data$height>210]=NaN
data$height[data$height<140]=NaN

data$politics_lr[data$politics_lr>10]=NaN

data$years_education[data$years_education>30]=NaN #  

data$age[data$age>114]=NaN

data$household_income[data$household_income>10]=NaN

data$sex[data$sex==1]= 'male' # 
data$sex[data$sex==2]= 'female'
data$sex=factor(data$sex)

data$sex_ec=data$sex %>% as.character # effects code sex
data$sex_ec[data$sex_ec=='male']=0.5
data$sex_ec[data$sex_ec=='female']=-0.5
data$sex_ec %<>% as.numeric()

data$supervisor_ec = (data$supervisor-1.5)*-1

# get country specific means and SDs in number of years of education. This is to allow years of education to be scaled within country, 
# because different countries have different education systems and we don't want that hassle.

data_ag=data %>% group_by(country) %>% summarise(years_education_m=mean(years_education,na.rm=T),
                                         years_education_sd=sd(years_education,na.rm=T))

data=inner_join(data,data_ag,by='country') # attach the aggregated data above to our main dataframe

data$years_education_scaled_country=(data$years_education-data$years_education_m)/data$years_education_sd

# Cleans up dataframe
data %<>% select(-years_education_m,-years_education_sd,-supervisor) %>% filter(sex %in% c('male','female'))

# Z score predictors so mixed effects models don't break

data$age_sc=data$age %>% scale
data$politics_lr_sc=data$politics_lr %>% scale
data$government_equality_sc  =data$government_equality %>% scale
data$household_income_sc=data$household_income %>% scale
data$height_sc=data$height %>% scale

# more density plots to check if the distributions are looking fine now.

data %>% 
    keep(is.numeric) %>% # keep numeric variables
    gather() %>%  # the rest of this code is a crude hack to get it to make dens plots for all numeric variables. 
    ggplot(aes(value))+facet_wrap(~key,scales='free')+
    geom_density(fill='darkolivegreen', alpha=1) 

# ======================================================

write.csv(data,'ESS7_height_redistribution_cleaned.csv',row.names = F)
