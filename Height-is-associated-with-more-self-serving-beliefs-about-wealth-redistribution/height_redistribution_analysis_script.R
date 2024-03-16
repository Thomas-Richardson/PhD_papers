library(magrittr)
library(purrr)
library(readr)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(effsize)
library(broom)
library(tidyr)
library(flextable)
library(modelr)
library(knitr)
library(kableExtra)

data=read_csv('ESS7_height_redistribution_cleaned.csv') %>% na.omit()

data$sex %<>% factor

# === first explorations ===================

# get means, medians, sds and IQRs for each variable for the sample as a whole and each country.
data %>% select(height,household_income,age,politics_lr,years_education_scaled_country,government_equality) %>% map_dbl(mean) %>% round(2)
data %>% select(height,household_income,age,politics_lr,years_education_scaled_country,government_equality) %>% map_dbl(median) %>% round(2)
data %>% select(height,household_income,age,politics_lr,years_education_scaled_country,government_equality) %>% map_dbl(sd) %>% round(2)
data %>% select(height,household_income,age,politics_lr,years_education_scaled_country,government_equality) %>% map_dbl(IQR) %>% round(2)

data %>% group_by(country) %>% summarise(round(mean(height),1),round(sd(height),2), round(mean(age),1),round(sd(age),2),round(mean(years_education),1),
                                         round(sd(years_education),1), round(mean(government_equality),1),round(sd(government_equality),2),
                                         round(mean(politics_lr),1),round(sd(politics_lr),2),round(sum(supervisor_ec==0.5)/length(supervisor_ec),2),
                                         round(mean(household_income),1),round(sd(household_income),2)) -> country_descriptives

colnames(country_descriptives) = c("Code",'Height','Height sd','Age','Age sd','Years of education','Education sd','Support for government redistribution of wealth','Redistribution sd',
                                   'Political orientation', 'Pol sd','Authority position at work (%)', 'household income decile','Income sd')

country_descriptives %>% flextable() %>% theme_vanilla() %>% print( preview = "docx") # print a table to be formatted for the paper

# get general descriptives: distributions, tables etc

data$country %>% table %>% sort # how many of each country do we have?
data$sex %>% table

ag_data = data %>% group_by(country) %>% summarise(height = mean(height),equality = mean(government_equality)) # get country level height and equality support
ag_data$equality %>% summary
ag_data$height %>% hist

# Let's start testing!

t.test(government_equality~sex,data) # sex diffs in support for government redistribution of wealth
cohen.d(government_equality~sex,data) # get cohen's d

t.test(height~sex,data) # sex diffs in height
cohen.d(as.numeric(height)~sex,data) # effect size

data %$% cor.test(politics_lr_sc,government_equality,method = 'spearman') # correlation between political orientation and support for wealth redistribution

for (country_ in unique(data$country) ){ # correlation between political orientation and support for wealth redistribution for each country
  data_country = data %>% filter(country == country_)
  data_country %$% cor(politics_lr_sc,government_equality,method = 'spearman') %>% round(2) -> cor_
  paste(country_, cor_) %>% print
  
}

round(table(data$supervisor_ec)/sum(table(data$supervisor_ec)),2)

#========= main event =========================================================================

# Model A ===========================

lmer(government_equality~height_sc*household_income_sc*sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       (1+height_sc:household_income_sc:sex_ec|country),data,REML=F ) %>% summary() # 3 way interaction is non sig, so we remove it in the next model

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       household_income_sc:height_sc+height_sc:sex_ec+
       (1+height_sc:sex_ec||country),data ) %>% summary() # sex: height interaction non sig, so we remove that in the next model

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       household_income_sc:height_sc+
       (1+household_income_sc:height_sc||country),data ) %>% summary()
# though the random slope contributes nothing a reviewer preferred I left it in.

#=== Let's now make a nice table that can form the basis of the one in the paper ================

RR=lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+
          household_income_sc:height_sc+supervisor_ec+
          (1+household_income_sc:height_sc||country),data ) %>% summary()

results_table = RR$coefficients %>% data.frame %>% rename(beta=Estimate, Se = Std..Error, `p-value` = Pr...t..) %>% select(-df,-t.value)
results_table %<>% round(3)
results_table$beta %<>% round(2)
results_table$`p-value` %<>% as.character()
results_table$`p-value`[results_table$`p-value`=='0']='< 0.0001'

names = c('Intercept','Height','Income decile','Sex(male)','Age',
          'Political orientation (left - right)','Education',
          'Authority position','Height * household income')
row.names(results_table) =names

results_table %>% 
  kable() %>% 
  kable_styling('striped',full_width = T) %>% 
  column_spec(1,border_right=T) %>%
  add_header_above(c(" ","Support for government redistribution of wealth"=3)) %>% 
  footnote(general = "results of mixed effects models predicting support for government redistribution of wealth",
           general_title = 'Table 1:',footnote_as_chunk = T,title_format = "bold" )


#== let's visualise the interaction controlling for all else. ====

mod_control_vars = lmer(government_equality~sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+(1|country),data )
data %<>% add_residuals(mod_control_vars)

data$`Height Quintile` = factor(data$height %>% ntile(5))

interaction_plot=data %>% ggplot(aes(x=household_income, y = resid, group = `Height Quintile`, colour = `Height Quintile` ))+
  geom_smooth(method='lm', se=F)+
  labs(x = 'Income decile',y= 'Residual support for government redistribution of wealth')+
  scale_x_continuous(breaks=seq(1,10,1))+
  NULL

interaction_plot

data$`Income Decile` = data$household_income %>% factor

interaction_plot2=data %>% ggplot(aes(x=height, y = resid, group = `Income Decile`, colour = `Income Decile` ))+
  geom_smooth(method='lm', se=F)+
  labs(x = 'Height (cm)',y= 'Residual support for government redistribution of wealth')+
  #scale_x_continuous(breaks=seq(1,10,1))+
  NULL

interaction_plot2

Figure1 = cowplot::plot_grid(interaction_plot, interaction_plot2, labels = c('1A','1B'))

ggsave('Figure1.jpg',plot=Figure1,dpi = 600,width = 25,height=10,units='cm')

data %>% filter(household_income==10) %>% lmer(resid~height_sc+
                                                (1+height_sc|country),.,REML=F ) %>% summary()

# what about the hypothesis of a main effect of height? referred to as model B in the paper

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+height_sc:sex_ec+
       (1+height_sc:sex_ec|country),data ) %>% summary()

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       (1+height_sc||country),data ) %>% summary()

RR=lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
          (1+height_sc||country),data ) %>% summary()

results_table = RR$coefficients %>% data.frame %>% rename(beta=Estimate, Se = Std..Error, `p-value` = Pr...t..) %>% select(-df,-t.value)
results_table %<>% round(3)
results_table$beta %<>% round(2)
results_table$`p-value` %<>% as.character()
results_table$`p-value`[results_table$`p-value`=='0']='< 0.0001'

names = c('Intercept','Height','Income decile','Sex(male)','Age',
          'Political orientation (left - right)','Education',
          'Authority position')
row.names(results_table) =names

results_table %>% 
  kable() %>% 
  kable_styling('striped',full_width = T) %>% 
  column_spec(1,border_right=T)

# ======= compare AIC ===============================================

ModelA= lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+height_sc:household_income_sc+
       (1+height_sc:household_income_sc||country),data )

ModelB =lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       (1+height_sc||country),data )

AIC(ModelA)-AIC(ModelB)

#============= height on general political orientation ===========================================================

lmer(politics_lr_sc~height_sc*sex_ec*household_income_sc+age_sc+(1+height_sc:sex_ec:household_income_sc|country),data ) %>% summary()

lmer(politics_lr_sc~height_sc+sex_ec+household_income_sc+age_sc+
       height_sc:sex_ec+height_sc:household_income_sc+
       (1+height_sc:sex_ec|country)+(1+height_sc:household_income_sc|country),data, REML = F ) %>% summary()

# explore interaction effects

lmer(politics_lr_sc~height_sc+household_income_sc+age_sc+
       height_sc:household_income_sc+
       (1+height_sc:household_income_sc|country),filter(data,sex=='male'),REML=F ) %>% summary()

lmer(politics_lr_sc~height_sc+household_income_sc+age_sc+
       height_sc:household_income_sc+
       (1+height_sc:household_income_sc|country),filter(data,sex=='female'),REML=F ) %>% summary()

#============= Robustness checks =====================================

# interaction controls

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
               height_sc:household_income_sc+ sex:household_income_sc+politics_lr_sc:household_income_sc+age_sc:household_income_sc+years_education_scaled_country:household_income_sc+
       supervisor_ec:household_income_sc+
               (1+height_sc:household_income_sc|country),data,REML=F ) %>% summary


# use countries as fixed effects with clustered standard errors
library(plm)
library(lmtest)

mod1 = data %>% plm(government_equality~height_sc+household_income_sc+sex_ec+age_sc+
              politics_lr_sc+years_education_scaled_country+supervisor_ec+country,data=.,model = 'pooling', index = c('country') )

G <- length(unique(data$country))
N <- length(data$country)
dfa <- (G/(G - 1)) * (N - 1)/mod1$df.residual

results = coeftest(mod1, vcov=function(x) dfa*vcovHC(x, cluster="group", type="HC0")) %>% tidy
results= results[c(1:8),] # we don't need the country dummies

results %>% View

# test each sex separately to test for simpson's paradox

data %>% filter(sex=='male') %>% lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
       (1|country),. ) %>% summary()

data %>% filter(sex=='female') %>% lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
                                        (1|country),. ) %>% summary()

# model B

data %>% filter(sex=='male') %>% lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
                                        (1|country),. ) %>% summary()

data %>% filter(sex=='female') %>% lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
                                          (1|country),. ) %>% summary()


# test for collider bias: fit the model a bunch more times, each time excluding a different covariate.

# Model A

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
       (1|country),data ) %>% summary() # remove pol orientation

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc++household_income_sc:height_sc+supervisor_ec+
       (1|country),data ) %>% summary() # remove education

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+
       (1|country),data ) %>% summary() # remove supervisor status

lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
       (1|country),data ) %>% summary() # remove sex 

lmer(government_equality~height_sc+household_income_sc+sex_ec+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
       (1|country),data ) %>% summary() # remove age

# Model B

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+years_education_scaled_country+supervisor_ec+
       (1|country),data ) %>% summary() # remove pol orientation

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc++supervisor_ec+
       (1|country),data ) %>% summary() # remove education

lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+
       (1|country),data ) %>% summary() # remove supervisor status

lmer(government_equality~height_sc+household_income_sc+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       (1|country),data ) %>% summary() # remove sex 

lmer(government_equality~height_sc+household_income_sc+sex_ec+politics_lr_sc+years_education_scaled_country+supervisor_ec+
       (1|country),data ) %>% summary() # remove age

# are results confounded by European region?

data$region = 'x'
data$region[data$country %in% c('LT','DK','NO','SE','FI')]='north'
data$region[data$country %in% c('IL','CH','DE')]='south'
data$region[data$country %in% c('SI','PL','HU','AT','CZ')]='east'
data$region[data$country %in% c('GB','PT','ES','IE','FR','NL','BE')]='west'
data$region[data$country=='IL'] = 'other'
data$region %>% unique
lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
       (1|country)+(1|region),data ) %>% summary()

#======== let's get a result for each country separately ========================

# height: income effect ===========

by_country = data.frame(country = unique(data$country),ns = (data$country %>% table %>% data.frame %>% select(Freq)), coefficients = NaN,se = NaN,p_values = NaN) %>% 
  rename(N = Freq)

for (country_ in unique(data$country) ){
  data_country = data %>% filter(country == country_)
  model_fit = data_country %>% lm(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec,.) %>% tidy
  model_fit_coef=model_fit$estimate[model_fit$term=='height_sc:household_income_sc'] %>% unlist
  model_fit_se=model_fit$std.error[model_fit$term=='height_sc:household_income_sc'] %>% unlist
  model_fit_p=model_fit$p.value[model_fit$term=='height_sc:household_income_sc'] %>% unlist
  
  by_country$coefficients[by_country$country==country_]= model_fit_coef %>% round(2)
  by_country$p_values[by_country$country==country_]= model_fit_p %>% round(2)
  by_country$se[by_country$country==country_] = model_fit_se %>% round(3)

  }

Overall_effect = data.frame(country = 'Overall',N=26724,coefficients=.1,se=5.998e-03,p_values=5.61e-06)

by_country=rbind(by_country,Overall_effect)

by_country$country <- factor(by_country$country,levels = by_country$country[order(by_country$coefficients)])

by_country$coefficients[by_country$country=='Overall']=-2.724e-02

figure2a = by_country %>% ggplot(aes(x=country,y = coefficients,group = country))+
  geom_point()+
  geom_hline(yintercept = 0,col='red')+
  geom_errorbar(aes(ymin=coefficients-1.96*se,ymax=coefficients+1.96*se),size=0.8) +
  ylim(-.21,0.12)+
  labs(y = expression(paste(beta, " coefficient for height * income interaction")))
  


# ========= height main effect============================================

by_country = data.frame(country = unique(data$country),ns = (data$country %>% table %>% data.frame %>% select(Freq)), coefficients = NaN,se = NaN,p_values = NaN) %>% 
  rename(N = Freq)
 
for (country_ in unique(data$country) ){
  data_country = data %>% filter(country == country_)
  model_fit = data_country %>% lm(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec,.) %>% tidy
  model_fit_coef=model_fit$estimate[model_fit$term=='height_sc'] %>% unlist
  model_fit_se=model_fit$std.error[model_fit$term=='height_sc'] %>% unlist
  model_fit_p=model_fit$p.value[model_fit$term=='height_sc'] %>% unlist
  
  by_country$coefficients[by_country$country==country_]= model_fit_coef %>% round(2)
  by_country$p_values[by_country$country==country_]= model_fit_p %>% round(2)
  by_country$se[by_country$country==country_] = model_fit_se %>% round(3)
  
}

Overall_effect = data.frame(country = 'Overall',N=26724,coefficients=.1,se=8.865e-03,p_values=1.95e-07)

by_country=rbind(by_country,Overall_effect)

by_country$country <- factor(by_country$country,levels = by_country$country[order(by_country$coefficients)])

by_country$coefficients[by_country$country=='Overall']=-4.615e-02

figure2b = by_country %>% 
  ggplot(aes(x=country,y = coefficients,group = country))+
  geom_point()+
  geom_hline(yintercept = 0,col='red')+
  geom_errorbar(aes(ymin=coefficients-1.96*se,ymax=coefficients+1.96*se),size=0.8) + 
  ylim(-.21,0.12)+
  labs(y = expression(paste(beta, " coefficient for height effect")))


Figure2 = cowplot::plot_grid(figure2a, figure2b,nrow = 2, labels = c('2A','2B'))

ggsave('Figure2.jpg',plot=Figure2,dpi = 600,width = 20,height=20,units='cm')

binom.test(16,20) #  binomial test 

# re-run analyses removing the sig effects ================================

data %>% filter(!(country %in% c('GB','FI') ) ) %>%
  lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+household_income_sc:height_sc+supervisor_ec+
         (1|country),. ) %>% summary()

data %>% filter(!(country %in% c('DK','AT')) ) %>%
  lmer(government_equality~height_sc+household_income_sc+sex_ec+age_sc+politics_lr_sc+years_education_scaled_country+supervisor_ec+
         (1|country),. ) %>% summary()
