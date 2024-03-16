# robustness check on Richardson 2020

# Packages ============================================
library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)
library(visreg)
library(broom)
library(flextable)

#============ Lets stats! =======================================================================
cbPalette_ <- c("#E69F00", "#56B4E9", "#999999","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # define a colour palette for graph that is friendly to people with red-green colour blindness


data = read_csv('MMA_data_sexed_checked.csv')

data$Height=data$Height*2.54 # convert to more rational units (cm)
data$Armspan=data$Armspan*2.54 # convert to more rational units (cm)
data$Weight = data$Weight*0.544 # convert to more rational units (kg)

women = data %>% filter(sex=='female') # filter women only
men = data %>% filter(sex=='male')   # filter men only

mod = lm(Win_Percentage~Armspan*sex+Height+Weight,data) # multiple regression predicting win percentage from height weight armspan and sex, also armspan*sex interaction
mod %>% summary # interaction isn't sig, so we remove it
mod = lm(Win_Percentage~Armspan+sex+Height+Weight,data) 
results = tidy(mod,conf.int=T) %>% select(-statistic) %>%  rename(Predictor = term, b = estimate, se = std.error, `p-value`=p.value) # tidy up the regression output 
results[2:5,] # print out tidy regression output

win_plot=visreg(mod, "Armspan", gg=T)+ # residual plot
  ylim(0.35,1)+
  xlab('Arm span (cm)')+
  ylab('Residual proportion of fights won')
win_plot

# I can't figure out how to make the above plot colour blind friendly, sorry!
#ggsave('armspan_win_plot.jpg',plot=win_plot,dpi = 600,width = 12,height=9,units='cm') # save as jpeg

# new analyses

data %>% summarise(mean(Wins), var(Wins))

mod2 = glm(Wins ~ offset(log(Total_Fights) )+Armspan + sex + Height + Weight, data, family = quasipoisson)
(results = tidy(mod2,conf.int=T) %>% 
  select(-statistic) %>%  
  rename(Predictor = term, b = estimate, se = std.error, `p-value`=p.value) %>%
  mutate(b = exp(b)-1, conf.low=exp(conf.low)-1,conf.high = exp(conf.high)-1)) # subtracting 1 from the coefficients is just a hack to get them to show more sig figs

results %>% flextable() %>% theme_vanilla() %>% print( preview = "docx") # print a table to be formatted for the paper


# get R^2
glance(mod2) %>% summarise(pseudoR2 = 1-deviance/null.deviance)
glance(mod)$r.squared

# That's a wrap! ==========================================================================================================================



