library(tidyverse)
library(lmerTest)
library(magrittr)

#==================================================================================================================
#=========================================== Study 1 =================================================================

data1 = read_csv("data_s1.csv")
data1$rating=data1$rating %>% factor()

#data1 %>% str()

data1 %>% select(ppt_index) %>% n_distinct() # we have 81 ppts
data1 %>% select(stimulus_identity) %>% n_distinct() # we have 20 stims

data1_ag=data1 %>% group_by(ppt_index) %>% summarise(
  arm=mean(upper_arm_circumference,na.rm=T),
  height=mean(height,na.rm=T),
  age=mean(age,na.rm=T)
)

# correlation between height and upper arm circumference
cor.test(data1_ag$height,data1_ag$arm,method = 'spearman')

# plot all numeric variables as density plots
data1 %>% 
  keep(is.numeric) %>% 
  select(-ppt_index) %>% 
  gather() %>%  
  ggplot(aes(value))+facet_wrap(~key,scales='free')+
  geom_density(fill='darkolivegreen', alpha=1) 

age_sd_1= sd(data1$age)

# height
height=glmer('rating ~ age_sc*stim_age_sc+height_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + height_sc|stimulus_identity)',
             family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
height %>% summary()

heightb=glmer('rating ~ age_sc+stim_age_sc+height_sc+
           (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + height_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
heightb %>% summary()

heightc=glmer('rating ~ age_sc+stim_age_sc+
           (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
heightc %>% summary()

heightd=glmer('rating ~ age_sc+height_sc+
           (1|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + height_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
heightd %>% summary()

heighte=glmer('rating ~ age_sc+
           (1|ppt_index) + (1 + age_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
heighte %>% summary()

heightf=glmer('rating ~ age_sc*stim_age_sc+
           (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
heightf %>% summary()

AIC(heightb)
AIC(heightc)
AIC(heightd)
AIC(heighte)
AIC(heightf)

confint.merMod(heighte,method='Wald')

exp( (-0.4275*10)/age_sd_1) # 10 year odds ratio
exp( (-0.05442417*10)/age_sd_1) # lower CI
exp( (-0.8006628*10)/age_sd_1) # higher CI

# upper arm circumference
arm=glmer('rating ~ age_sc*stim_age_sc+upper_arm_circumference_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + upper_arm_circumference_sc|stimulus_identity)',
          family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
arm %>% summary()

armb=glmer('rating ~ age_sc+stim_age_sc+upper_arm_circumference_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + upper_arm_circumference_sc|stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
armb %>% summary()

armc=glmer('rating ~ age_sc+stim_age_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
armc %>% summary()

armd=glmer('rating ~ age_sc+upper_arm_circumference_sc+
      (1|ppt_index) + (1 + age_sc|stimulus_identity)+(0 + upper_arm_circumference_sc|stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
armd %>% summary()

arme=glmer('rating ~ age_sc+
      (1|ppt_index) + (1 + age_sc|stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data1) 
arme %>% summary()

AIC(armb)
AIC(armc)
AIC(armd)
AIC(arme)

#==================================================================================================================
#========================================== Study 2 ================================================================

data2=read.csv('data_s2.csv')
data2$rating=data2$rating %>% factor()

#data2 %>% str() # look at the data

# plot all numeric variables as density plots
data2 %>% keep(is.numeric) %>% 
  select(-ppt_index,-rating,-(surpass:pressure.others)) %>% 
  gather() %>%  
  ggplot(aes(value))+facet_wrap(~key,scales='free')+
  geom_density(fill='navy', alpha=0.8) 

data2 %>% select(ppt_index) %>% n_distinct() # we have 93 ppts
data2 %>% select(stimulus_identity) %>% n_distinct() # we have 32 stims

data2_ag=data2 %>% group_by(ppt_index) %>% summarise(
  grip_strength=mean(grip_strength,na.rm=T),
  height=mean(height,na.rm=T),
  IPIP=mean(IPIP,na.rm=T),
  age=mean(age,na.rm=T))

cor.test(data2_ag$height,data2_ag$grip_strength) # 0.26
cor.test(data2_ag$height,data2_ag$age) #-.12
cor.test(data2_ag$height,data2_ag$IPIP) # 0.1
cor.test(data2_ag$age,data2_ag$grip_strength) # - 0.2
cor.test(data2_ag$age,data2_ag$IPIP) # -0.1
cor.test(data2_ag$IPIP,data2_ag$grip_strength) # 0 

age_sd_2= sd(data2$age)
stim_age_sd_2 = sd(data2$stim_age)
grip_strength_sd_2 = sd(data2$grip_strength)

#============================== Models ===============================================

# Height

heighta=glmer('rating ~ age_sc*stim_age_sc+height_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
      (0 + height_sc|stimulus_identity)+(0 + IPIP_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)

heightb=glmer('rating ~ age_sc+stim_age_sc+height_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
                   (0 + height_sc|stimulus_identity)+(0 + IPIP_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)

heightc=glmer('rating ~ age_sc+stim_age_sc+height_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
              (0 + height_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)

heightd=glmer('rating ~ age_sc+stim_age_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
                   (0 + IPIP_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)

heighte=glmer('rating ~ age_sc+stim_age_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)

heightf=glmer('rating ~ age_sc*stim_age_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
              family=binomial,control = glmerControl(optimizer = "bobyqa"),data2)
heightf %>% summary()

AIC(heightb)
AIC(heightc)
AIC(heightd)
AIC(heighte)
AIC(heightf)
AIC(heightg)

heighte %>% summary()
confint.merMod(heighte,method='Wald')

exp( (-0.15579*10)/age_sd_2) # 10 year odds ratio
exp( (-0.3018114*10)/age_sd_2) # lower CI
exp( (-0.009763203*10)/age_sd_2) # higher CI

exp( (-0.24539*10)/stim_age_sd_2) # 10 year odds ratio
exp( (-0.3440965*10)/stim_age_sd_2) # lower CI
exp( (-0.146691369*10)/stim_age_sd_2) # higher CI

# Grip
grip_model=glmer('rating ~ age_sc*stim_age_sc+grip_strength_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
      (0 + grip_strength_sc|stimulus_identity)+(0 + IPIP_sc|stimulus_identity)',
                 family=binomial,control = glmerControl(optimizer = "bobyqa"),data2) 
grip_model %>% summary()

grip_modelb=glmer('rating ~ age_sc+stim_age_sc+grip_strength_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
                 (0 + grip_strength_sc|stimulus_identity)+(0 + IPIP_sc|stimulus_identity)',
                  family=binomial,control = glmerControl(optimizer = "bobyqa"),data2) 
grip_modelb %>% summary()

grip_modelc=glmer('rating ~ age_sc+stim_age_sc+grip_strength_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
                 (0 + grip_strength_sc|stimulus_identity)',
                  family=binomial,control = glmerControl(optimizer = "bobyqa"),data2) 
grip_modelc %>% summary()

grip_modeld=glmer('rating ~ age_sc+stim_age_sc+IPIP_sc+
      (1 + stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)+
                 (0 + IPIP_sc|stimulus_identity)',
                  family=binomial,control = glmerControl(optimizer = "bobyqa"),data2) 
grip_modeld %>% summary()

AIC(grip_modelb)
AIC(grip_modelc)
AIC(grip_modeld)
AIC(heighte)

grip_modelc %>% summary()
confint.merMod(grip_modelc,method='Wald')

exp( (-0.13009*10)/age_sd_2) # 10 year odds ratio
exp( (-0.27756129*10)/age_sd_2) # lower CI
exp( (0.01738305*10)/age_sd_2) # higher CI

exp( (-0.24588*10)/stim_age_sd_2)
exp( (-0.34474055*10)/stim_age_sd_2)
exp( (-0.14701068*10)/stim_age_sd_2) # 10 year effect of stim age

exp((0.12434*10)/grip_strength_sd_2)
exp((-0.27756129*10)/grip_strength_sd_2)
exp((0.27778453*10)/grip_strength_sd_2) # grip strength

#==================================================================================================================
#========================================== Study 3 ================================================================

data3=read.csv('data_s3.csv')
data3 %>% str() #look at data

# plot all numeric variables as density plots
data3 %>% keep(is.numeric) %>% 
  select(-stimulus_identity,-ppt_index,-rating) %>% 
  gather() %>%  
  ggplot(aes(value))+facet_wrap(~key,scales='free')+
  geom_density(fill='navy', alpha=0.8) # plot all numeric variables as histograms

data3$rating=data3$rating %>% factor()
data3$transform_type %<>% as.character
data3$transform_type[data3$transform_type=='young']='-0.5' # effect code transform type
data3$transform_type[data3$transform_type=='old']='0.5'
data3$transform_type %<>%as.numeric 

data3 %>% select(ppt_index) %>% n_distinct() # we have 98 ppts
data3 %>% select(stimulus_identity) %>% n_distinct() # we have 30 stims

data3_ag=data3 %>% group_by(ppt_index) %>% summarise(
  upper_arm_circumference=mean(upper_arm_circumference,na.rm=T),
  grip_strength=mean(grip_strength,na.rm=T),
  IPIP=mean(IPIP,na.rm=T),
  height=mean(height,na.rm=T),
  age=mean(age,na.rm=T)
)

age_sd_3= sd(data3$age)
upper_arm_sd_3 = sd(data3$upper_arm_circumference)

#========= full model =================================

full_model=glmer('rating ~ age_sc*stimulus_age_sc*transform_type + 
    (1 + stimulus_age_sc:transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                 family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

#========== remove 3 way interaction ==================
all_2way_interactions_model=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + 
    age_sc:stimulus_age_sc + stimulus_age_sc:transform_type + age_sc:transform_type + 
    (1 + stimulus_age_sc:transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                                  family=binomial,control = glmerControl(optimizer = "bobyqa"),data3) 

#========= 2 way interaction effects =======================

interaction_model_a=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + 
    age_sc:stimulus_age_sc + stimulus_age_sc:transform_type + 
    (1 + stimulus_age_sc:transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3) 

interaction_model_b=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + 
    age_sc:stimulus_age_sc + age_sc:transform_type + 
    (1 + stimulus_age_sc|ppt_index) + (0 + transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_c=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + 
    stimulus_age_sc:transform_type + age_sc:transform_type + 
    (1 + stimulus_age_sc:transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

# =============== 1 interaction effect =========================

interaction_model_d=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + age_sc:stimulus_age_sc +
        (1 + stimulus_age_sc|ppt_index) + (0 + transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_e=glmer('rating~age_sc + stimulus_age_sc + transform_type + age_sc:transform_type + 
        (1 + stimulus_age_sc|ppt_index)+(0 + transform_type|ppt_index)+(1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_f=glmer('rating ~ age_sc + stimulus_age_sc + transform_type + transform_type:stimulus_age_sc +
        (1 + stimulus_age_sc:transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_g=glmer('rating ~ age_sc + stimulus_age_sc + age_sc:stimulus_age_sc +
        (1 + stimulus_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_h=glmer('rating~age_sc + transform_type + age_sc:transform_type + 
                          (1 + transform_type|ppt_index)+(1 + age_sc|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

interaction_model_i=glmer('rating ~ stimulus_age_sc + transform_type + transform_type:stimulus_age_sc +
                          (1 + stimulus_age_sc:transform_type|ppt_index) + (1|stimulus_identity)',
                          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)


#======== no interaction effects ==================
no_interaction_modela=glmer('rating ~ age_sc + stimulus_age_sc + transform_type +
      (1 + stimulus_age_sc|ppt_index) + (0 + transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modelb=glmer('rating ~ stimulus_age_sc + transform_type +
      (1 + stimulus_age_sc|ppt_index) + (0 + transform_type|ppt_index) + (1|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modelc=glmer('rating ~ age_sc + transform_type +
      (1 + transform_type|ppt_index) + (1 + age_sc|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modeld=glmer('rating ~ age_sc + stimulus_age_sc +
      (1 + stimulus_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modele=glmer('rating ~ transform_type +
      (1 + transform_type|ppt_index) + (1|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modelf=glmer('rating ~ stimulus_age_sc +
      (1 + stimulus_age_sc|ppt_index) + (1|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

no_interaction_modelg=glmer('rating ~ age_sc + (1|ppt_index) + (1 + age_sc|stimulus_identity)',
                            family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)


AIC(full_model)#
AIC(all_2way_interactions_model)#
AIC(interaction_model_a) # 
AIC(interaction_model_b) # 7387
AIC(interaction_model_c) # 
AIC(interaction_model_d) # 7389.174
AIC(interaction_model_e) # 7389.306
AIC(interaction_model_f) # 
AIC(interaction_model_g) # 
AIC(interaction_model_h) # 
AIC(interaction_model_i) # 

AIC(no_interaction_modela) # 7387.6
AIC(no_interaction_modelb)
AIC(no_interaction_modelc)
AIC(no_interaction_modeld)
AIC(no_interaction_modele)
AIC(no_interaction_modelf)
AIC(no_interaction_modelg)


no_interaction_modela %>% summary()
confint.merMod(no_interaction_modela,method='Wald')

exp( (-0.11694*10)/age_sd_3) # 10 year odds ratio
exp( (-0.2233205*10)/age_sd_3) # lower CI
exp( (-0.01056926*10)/age_sd_3) # higher CI

#=========== other models =============================================

height=glmer('rating ~ age_sc + height_sc+
      (1|ppt_index) + (1 + age_sc||stimulus_identity)+(0 + height_sc||stimulus_identity)',
             family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

height %>% summary()
confint.merMod(height,method='Wald')

exp( (-0.10993*10)/age_sd_3) # 10 year odds ratio
exp( (-0.2192456*10)/age_sd_3) # lower CI
exp( (-0.0006159224*10)/age_sd_3) # higher CI


grip=glmer('rating ~ age_sc + grip_strength_sc+
      (1|ppt_index) + (1 + age_sc||stimulus_identity)+(0 + grip_strength_sc||stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

grip %>% summary()
confint.merMod(grip,method='Wald')

exp( (-0.08844*10)/age_sd_3) # 10 year odds ratio
exp( (-0.19085614*10)/age_sd_3) # lower CI
exp( (0.01398043*10)/age_sd_3) # higher CI

arm=glmer('rating ~ age_sc + upper_arm_circumference_sc+
      (1|ppt_index) + (1 + age_sc||stimulus_identity)+(0 + upper_arm_circumference_sc||stimulus_identity)',
          family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

arm %>% summary()
confint.merMod(arm,method='Wald')

exp( (-0.08569*10)/age_sd_3) # 10 year odds ratio
exp( (-0.18230724*10)/age_sd_3) # lower CI
exp( (0.0109338*10)/age_sd_3) # higher CI

exp((0.16085*3.5)/upper_arm_sd_3)
exp((0.06794874*3.5)/upper_arm_sd_3)
exp((0.2537426*3.5)/upper_arm_sd_3)

IPIP=glmer('rating ~ age_sc + IPIP_sc+
      (1|ppt_index) + (1 + age_sc||stimulus_identity)+(0 + IPIP_sc||stimulus_identity)',
           family=binomial,control = glmerControl(optimizer = "bobyqa"),data3)

IPIP %>% summary()
confint.merMod(IPIP,method='Wald')

exp( (-0.10435*10)/age_sd_3) # 10 year odds ratio
exp( (-0.2072512*10)/age_sd_3) # lower CI
exp( (-0.001457035*10)/age_sd_3) # higher CI

#==================================================================================================================
#==== combined analysis =================================================================================================

data1=read.csv('data_s1.csv')
data2=read.csv('data_s2.csv')
data3=read.csv('data_s3.csv')

data1=data1 %>% select(ppt_index:upper_arm_circumference)
data2=data2 %>% select(ppt_index:grip_strength,stim_age,IPIP)
data3=data3 %>% select(ppt_index:stimulus_age,-stimulus_name,IPIP,grip_strength)

data1 %>% colnames # check all columns are the same
data2 %>% colnames
data3 %>% colnames

data3 =data3 %>% rename(stim_age=stimulus_age)
data2$ppt_index=data2$ppt_index+81 # participant 1 in datasets 1 2 and 3 are obviously not the sample people so we give them new numbers here
data3$ppt_index=data3$ppt_index+81+93
data1$grip_strength=NaN # study 1 didn't measure grip
data1$IPIP=NaN # or social dominance
data2$upper_arm_circumference=NaN #study 2 didn't measure upper arm

# stimuli in studies 2 and 3 are the same but have different names, so need to give them the same names.
data2$stimulus_identity=data2$stimulus_identity %>%as.character 
data2$stimulus_identity %<>% str_replace("\\)_masc.jpg","") %>% str_replace("m\\(","") %>% factor
data3$stimulus_identity=data3$stimulus_identity %>% factor

data_full=bind_rows(data1,data2,data3)

nrow(data1)+nrow(data2)+nrow(data3)==nrow(data_full) # check we've not lost any rows in the join

data_full$height %<>% c() 
data_full$rating=factor(data_full$rating)
data_full %<>% filter(is.na(rating)==F) # get rid of non responses

data_full %<>% mutate(height_sc=scale(height),
                      age_sc=scale(age),
                      stim_age_sc=scale(stim_age),
                      IPIP_sc=scale(IPIP),
                      grip_strength_sc=scale(grip_strength),
                      upper_arm_circumference_sc=scale(upper_arm_circumference))

data_full %>% select(ppt_index) %>% n_distinct() # we have 272 ppts
data_full %>% select(stimulus_identity) %>% n_distinct() # we have 52 stims    

# plot all numeric variables as density plots
data_full %>% 
  keep(is.numeric) %>% 
  select(-ppt_index) %>% 
  gather() %>%  
  ggplot(aes(value))+facet_wrap(~key,scales='free')+
  geom_density(fill='darkolivegreen', alpha=1) 

data_full_ag=data_full %>% group_by(ppt_index) %>% summarise(
  arm=mean(upper_arm_circumference,na.rm=T),
  grip=mean(grip_strength,na.rm=T),
  height=mean(height,na.rm=T),
  age=mean(age,na.rm=T)
) 

cor.test(data_full_ag$arm,data_full_ag$height)
cor.test(data_full_ag$grip,data_full_ag$height)
cor.test(data_full_ag$arm,data_full_ag$grip)

age_sd_full = sd(data_full$age)
stim_age_sd_full = sd(data_full$stim_age)
height_sd_full = sd(data_full$height)
grip_sd_full = sd(data_full$grip_strength,na.rm=T)
upper_arm_sd_full = sd(data_full$upper_arm_circumference,na.rm=T)

#============================== Models =========================================== ===============================================

# age and stim age
age_mod=glmer('rating ~ age_sc+stim_age_sc+(1+stim_age_sc|ppt_index) + (1 + age_sc|stimulus_identity)',
      family=binomial,control = glmerControl(optimizer = "bobyqa"),data_full)

age_mod %>% summary()
confint.merMod(age_mod,method='Wald')

exp( (-0.20824*10)/age_sd_full) # 10 year odds ratio
exp( (-0.3016562*10)/age_sd_full) # lower CI
exp( (-0.1148150*10)/age_sd_full) # higher CI

exp((-0.27232*10)/stim_age_sd_full)
exp((-0.3713529*10)/stim_age_sd_full)
exp((-0.1732876*10)/stim_age_sd_full)


# upper_arm 
upper_arm_mod=glmer('rating ~ age_sc+stim_age_sc+upper_arm_circumference_sc+ (1+stim_age_sc|ppt_index) +
      (1 + upper_arm_circumference_sc|stimulus_identity)+(1 + age_sc|stimulus_identity)',
      family=binomial,control = glmerControl(optimizer = "bobyqa"),data_full)

upper_arm_mod %>% summary()
confint.merMod(upper_arm_mod,method='Wald')

exp( (-0.19707*10)/age_sd_full) # 10 year odds ratio
exp( (-0.30768175*10)/age_sd_full) # lower CI
exp( (-0.08645428*10)/age_sd_full) # higher CI

exp((-0.30741*10)/stim_age_sd_full)
exp((-0.46695492*10)/stim_age_sd_full)
exp((-0.14785794*10)/stim_age_sd_full) # stimulus age

exp((0.13901*3.5)/upper_arm_sd_full)
exp((0.03617881*3.5)/upper_arm_sd_full)
exp((0.24185097*3.5)/upper_arm_sd_full) # upper arm

# grip strength
grip_mod= glmer('rating ~ age_sc+stim_age_sc+grip_strength_sc+(1+stim_age_sc|ppt_index) +
      (1 + grip_strength_sc|stimulus_identity)+(1 + age_sc|stimulus_identity)',
      family=binomial,control = glmerControl(optimizer = "bobyqa"),data_full)

grip_mod %>% summary()
confint.merMod(grip_mod,method='Wald')

exp( (-0.10305*10)/age_sd_full) # 10 year odds ratio
exp( (-0.18842929*10)/age_sd_full) # lower CI
exp( (-0.01766717*10)/age_sd_full) # higher CI

exp((-0.12572*10)/stim_age_sd_full)
exp((-0.19453456*10)/stim_age_sd_full)
exp((-0.05690535*10)/stim_age_sd_full) # stimulus age

exp((0.10977*10)/grip_sd_full)
exp((0.02445193*10)/grip_sd_full)
exp((0.19507901*10)/grip_sd_full) # grip


# height 
height_mod= glmer('rating ~ age_sc+stim_age_sc+height_sc+(1+stim_age_sc|ppt_index) +
      (1 + height_sc|stimulus_identity)+(1 + age_sc|stimulus_identity)',
      family=binomial,control = glmerControl(optimizer = "bobyqa"),data_full)

height_mod %>% summary()
confint.merMod(height_mod,method='Wald')

exp( (-0.17731*10)/age_sd_full) # 10 year odds ratio
exp( (-0.275211548*10)/age_sd_full) # lower CI
exp( (-0.07940312*10)/age_sd_full) # higher CI

exp((-0.26364*10)/stim_age_sd_full)
exp((-0.356484459*10)/stim_age_sd_full)
exp((-0.17080084*10)/stim_age_sd_full) # stimulus age

exp((0.10228*5)/height_sd_full)
exp((0.004310777*5)/height_sd_full)
exp((0.20025733*5)/height_sd_full)# height


# self-percieved social dominance
social_dominance_mod=glmer('rating ~ age_sc+stim_age_sc+IPIP_sc+(1+stim_age_sc|ppt_index) + (1 + IPIP_sc|stimulus_identity)+ (1 + age_sc|stimulus_identity)',
      family=binomial,control = glmerControl(optimizer = "bobyqa"),data_full)

social_dominance_mod %>% summary()
confint.merMod(social_dominance_mod,method='Wald')

exp( (-0.12224*10)/age_sd_full) # 10 year odds ratio
exp( (-0.20759895*10)/age_sd_full) # lower CI
exp( (-0.03687469*10)/age_sd_full) # higher CI

exp((-0.12332*10)/stim_age_sd_full)
exp((-0.19260530*10)/stim_age_sd_full)
exp((-0.05403950*10)/stim_age_sd_full) # stimulus age

#================= END ===============================================================================================================