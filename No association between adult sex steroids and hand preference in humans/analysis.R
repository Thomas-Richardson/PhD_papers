library(tidyverse)
library(magrittr)
library(rstan)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(cowplot)
library(e1071) 
library(modelr)

set.seed(140293)

data = read.csv('handedness_hormone_data_cleaned.csv') %>% filter(regular_periods<2) %>% select(-regular_periods) # filter women who do not have regular periods

data$sex %>% table
data %>% group_by(sex) %>% summarise(mean_age = round(mean(age),2),sd_age = round(sd(age),3)) 
# age of sample. We've excluded older women as they're post menopausal so that explains why average age is lower. We must control for age in analyses as it is partially
# confounded with sex.

data$handedness %>% table # How many lefties and mixed handers do we have?

round(data$handedness %>% table()*100/nrow(data),2) # get table in percentages

men_T = data %>% filter(sex=='male')
women_T = data %>% filter(sex=='female')

men_T$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed")) # convert to factor
women_T$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed")) # convert to factor

# What transform should we use to address the skew and kurtosis issues of hormones for each sex separately?
data %>% 
  group_by(sex) %>%
  summarise(
    untransformed_T_kurtosis = kurtosis(T),
    ln_T_kurtosis = kurtosis(log(T)),
    sqroot_T_kurtosis = kurtosis(sqrt(T)),
    untransformed_T_skewness = skewness(T),
    ln_T_skewness = skewness(log(T)),
    sqroot_T_skewness = skewness(sqrt(T))
  ) %>% 
  t() %>%
  data.frame()

# Seems a natural log transform works best on the female data but a square root transform is best for male data

# how do testosterone levels change throughout the day?
men_T$session_time2  = factor(men_T$session_time)
levels(men_T$session_time2) = c('Morning','Midday','Afternoon')
men_T %>% ggplot(aes(x = session_time2, y = sqrt(T)))+geom_bar(stat='identity', colour = 'navy') + ylab('square root testosterone levels')+xlab('Time of day')

women_T$session_time2  = factor(women_T$session_time)
levels(women_T$session_time2) = c('Morning','Midday','Afternoon')
women_T %>% ggplot(aes(x = session_time2, y = log(T)))+geom_bar(stat='identity', colour = 'orange') + ylab('ln testosterone levels')+xlab('Time of day')

# plot our effects to get a first look. We want to plot T levels for each level of handedness while controlling for a variety of factors. We do this by plotting the
# *residual* hormone levels from a model containing all control variables except handedness. 

resid_men_T = lm(sqrt(T)~age+factor(year)+session_time,men_T )
men_T %<>% add_residuals(resid_men_T)
men_T_plot=ggplot(men_T,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual square root transformed Testosterone', x=  'Handedness')

resid_women_T = lm(log(T)~age+factor(year)+session_time,women_T )
women_T %<>% add_residuals(resid_women_T)
women_T_plot=ggplot(women_T,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual log10 transformed Testosterone', x=  'Handedness')

#====================== models =========================================================
# prior=normal(-1, 5, autoscale=FALSE) add this to model normally distrubted priors

men_T %>% stan_glm(scale(sqrt(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)
#mod %>% prior_summary() # for looking at priors to check everything worked.

women_T %>% stan_glm(scale(log(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

#stan_trace(model1) # all chains seem stable and well mixed
#pp_check(model1) # posterior predictive check shows our model fits the data pretty well 
#stan_hist(model1, pars=c("handedness2"), bins=50)

# expluding those who were tested in the morning (when testosterone peaks noticably) as a robustness check

men_T %>% filter(session_time>0) %>%  stan_glm(scale(sqrt(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

women_T %>% filter(session_time>0) %>% stan_glm(scale(log(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

#====================== what about Estadiol? ========================================

data_E = data %>% filter(is.na(E)==F) # get rid of anyone who doesn't have E data. That does include everyone from 2011.

data_E$sex %>% table # data split by sex
data_E$handedness %>% table # data split by handedness
round((data_E$handedness %>% table)*100/(data_E %>% nrow),2) # data split by handedness percentages
data_E %>% nrow # we have 3869 datapoints

# filter male and females
men_E = data_E %>% filter(sex=='male') 
women_E = data_E %>% filter(sex=='female')

men_E$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))
women_E$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))

men_E$handedness %>% table
women_E$handedness %>% table

# What transform should we use to address the skew and kurtosis issues of hormones for each sex separately?
data_E %>% 
  group_by(sex) %>%
  summarise(
    untransformed_E_kurtosis = kurtosis(E),
    ln_E_kurtosis = kurtosis(log(E)),
    sqroot_E_kurtosis = kurtosis(sqrt(E)),
    untransformed_E_skewness = skewness(E),
    ln_E_skewness = skewness(log(E)),
    sqroot_E_skewness = skewness(sqrt(E))
  ) %>% 
  t() %>%
  data.frame()
# again, seems like ln for females and sqrt for males

#====================== models =========================================================

men_E %>% stan_glm(scale(sqrt(E))~handedness+age+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

women_E$handedness2 = women_E$handedness %>% as.character() # for women we don't have enough lefties and mixed handers to model them as their own groups, so let's combine them
women_E$handedness2[women_E$handedness2=='Mixed-handed']='Non-Right-handed'
women_E$handedness2[women_E$handedness2=='Left-handed']='Non-Right-handed'
women_E$handedness2 %<>% factor(levels = c('Right-handed','Non-Right-handed') )

women_E %>% stan_glm(scale(log(E))~handedness2+age+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

# let's plot our estradiol effects too, and combined them with the testosterone plots to make a multipanel plot to go in the paper

resid_men_E = lm(sqrt(E)~age+session_time,men_E )
men_E %<>% add_residuals(resid_men_E)
men_E_plot=ggplot(men_E,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual square root transformed Oestradiol', x=  'Handedness')

resid_women_E = lm(log(E)~age+session_time,women_E )
women_E %<>% add_residuals(resid_women_E)
women_E_plot=ggplot(women_E,aes(x=handedness2,y=resid,fill=handedness2))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual log10 transformed Oestradiol', x=  'Handedness')

hormone_plot = plot_grid(men_T_plot,women_T_plot,men_E_plot,women_E_plot,ncol = 2,labels = c('Men T','Women T','Men E','Women E'))

plot_width = 25 # set the right dimensions for the paper
plot_height = plot_width

ggsave('hormone_plot.jpg',plot=hormone_plot,dpi = 500,width = plot_width,height=plot_height,units='cm') # save the plot.
