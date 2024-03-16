# Is arm length a sexually selected trait in humans? Evidence from mixed martial arts

# Packages ============================================
library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)
library(visreg)
library(broom)
library(purrr)
library(tidyr)

# ======================================================

# how to sex MMA fighters using the gender package ====================
# NOTE This is already done in the data, but I include the code for transparency and for the use of my colleagues on other projects. As you can see it's pretty easy!

library(gender)
data_MMA =read_csv('MMA_data.csv') # read in unsexed data

data_MMA$sex_weight = 'unknown' # create a column of fighter's sex based on their weight. We know there are no female fighters >145lbs and no males <116. We can use this to 'sanity check' the sex later
data_MMA$sex_weight[data_MMA$Weight<116] = 'female' # classify anyone at 115lb as female as there are no male weight classes this low
data_MMA$sex_weight[data_MMA$Weight>145] = 'male' # classify anyone above 145lb as male as there are no female weight classes this high

sex = data_MMA$First_name %>% gender(years = c(1969,2001)) # classify sex from first name using name databases from 1969-2001
sex %<>% dplyr::select(name, gender) # only pull out the name and gender columns as this is what we're interested in. I use dplyr::select because there is another select function in another package that supercedes dyplr

colnames(sex) = c('First_name','sex') # rename columns

sex=sex[duplicated(sex$First_name)==F,] # get rid of duplicate names 

data_MMA=left_join(data_MMA,sex,by='First_name') # add sex data to main dataframe. Left_join means that fighters whose names were not sexed now have an 'NA'

data_MMA %<>% mutate(sex = ifelse(is.na(sex),'unknown',sex))

#write_csv(data_MMA,'MMA_data_sexed_.csv')

# note that it will fail to identify around 200 fighters which whose sex I then googled... Better than doing over 1000!

#============ Lets stats! =======================================================================
cbPalette_ <- c("#E69F00", "#56B4E9", "#999999","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # define a colour palette for graph that is friendly to people with red-green colour blindness


data = read_csv('MMA_data_sexed_checked.csv')

## Engineer Features

data$Height=data$Height*2.54 # convert to more rational units (cm)
data$Armspan=data$Armspan*2.54 # convert to more rational units (cm)
data$Weight = data$Weight*0.544 # convert to more rational units (kg)

data$AHR = data$Armspan/data$Height # calculate armspan to height ratio

women = data %>% filter(sex=='female') # filter women only
men = data %>% filter(sex=='male')   # filter men only

## Explore the data

data$sex %>% table # how many men vs women

# plot density plots for each numeric variable to look for anomolies.
data %>%                                        # plot all numeric variables as density plots. 
  keep(is.numeric) %>%  # keep numeric columns
  gather() %>%       # stack them on top of each other (just a hack to make the next line work)
  ggplot(aes(value))+facet_wrap(~key,scales='free')+ # make a plot, facetwrap means make a different plot for each level (in this case variable)
  geom_density(fill='darkolivegreen') # make dark olive green density plots 
# Fun fact, this code will work on any dataframe, and will make a density plot for every numeric variable in the dataframe specified at the start. 
# Feel free to use it on your own data and share, I didn't write it :)

cor.test(women$Armspan,women$Height) # correlation of armspan and height in women
cor.test(men$Armspan,men$Height)       # correlation of armspan and height in men
cor.test(data$AHR,data$Weight,method = 'spearman')      # correlation between armspan to height ratio and weight, to test for positive allometry 
cor.test(data$AHR,data$Height)

## Are there sex differences in arm length even controlling for the fact that men are taller and heavier?

sex_diffs_mod = lm(Armspan~Height+sex+Weight,data) %>% summary() # multiple regression testing sex differences in armspan controlling for other variables

results = tidy(sex_diffs_mod,conf.int=T) %>% select(-statistic) %>%  rename(Predictor = term, b = estimate, se = std.error, `p-value`=p.value) # tidy up the regression output 
results[2:4,] # print out tidy regression output

dimorphism_plot = ggplot(data,aes(x=Height, y=Armspan,colour=sex,group=sex))+
  geom_jitter(alpha =0.2)+ # jitter the data points 
  geom_smooth(method = 'lm',se=F,size=1)+ # plot a trend lines
  scale_colour_manual(values=cbPalette_)+ # add colour pallette defined above
  xlab('Height (cm)')+ # label x axis
  ylab('Arm span (cm)') # label y axis

dimorphism_plot # show me the plot I just made

#ggsave('sexual_dimorphism_armspan.tiff',plot=dimorphism_plot,dpi = 600,width = 15,height=15,units='cm',compression = 'lzw') # save graphs as .tiff file as some journals like .tiffs
ggsave('sexual_dimorphism_armspan.jpg',plot=dimorphism_plot,dpi = 600,width = 15,height=9,units='cm') # save as jpeg

## Do fighters with longer arms win more fight, controlling for their greater height and weight

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
ggsave('armspan_win_plot.jpg',plot=win_plot,dpi = 600,width = 12,height=9,units='cm') # save as jpeg


# That's a wrap! ==========================================================================================================================
