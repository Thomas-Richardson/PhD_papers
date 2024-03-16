# Left handedness analysis script

# ======= Left handed fighters are overrepresented in combat sports and are better fighters =================

# Make sure you have the 3 datafiles for the 3 studies respectively!

# ====================== Packages ====================================================

library(ggplot2)
library(dplyr)
set.seed(5) # so we get the same results every time

#=========== This is the function we'll be using to compare variances =======================

variance_comparison = function(measure, data){
  d2s=data[,measure]
  d2s.sp=d2s[which(data$Stance=="left-handed")]
  d2s.or=d2s[which(data$Stance=="right-handed")]
  obs_diff=var(d2s.sp)-var(d2s.or)
  #create distribution of bootstrapped differences in variance
  bsd=NULL
  boots=10000
  for (i in 1:boots){
    bsd=c(bsd,var(sample(d2s.sp,replace=TRUE))-var(sample(d2s.or,replace=TRUE)))
  }
  #calculate the bias parameter
  z0hat=qnorm(sum(bsd<obs_diff)/boots)
  #conduct jackknife analysis for acceleration parameter
  theta.is=NULL
  for (i in 1:length(d2s)){
    d2s.jk=d2s[-i]
    Stance.jk=data$Stance[-i]
    d2s.sp.jk=d2s.jk[which(Stance.jk=="left-handed")]
    d2s.or.jk=d2s.jk[which(Stance.jk=="right-handed")]
    theta.is=c(theta.is,var(d2s.sp.jk)-var(d2s.or.jk))
  }
  U.i=(length(d2s)-1)*(mean(theta.is)-theta.is)
  a.hat=sum(U.i^3)/(6*sum(U.i^2)^(3/2))
  #obtain the BCa p from the observed sample
  a.obs=sum(bsd<0)/boots
  p=pnorm((qnorm(a.obs)-z0hat)/(1+a.hat*(qnorm(a.obs)-z0hat))-z0hat)
  return(p)
}

#==============================================================================================================================
# ====================== Study 1: male boxers =================================================================================
#==============================================================================================================================

# ====================== Read data ====================================================

data= read.csv('Male_boxers.csv')

data=filter(data,Total_Fights>4,Win_Percentage>0.2) # get rid of fighters with 4 or fewer fights

right=data[data$Stance=="right-handed",c('Score','Win_Percentage')] 
left=data[data$Stance=="left-handed",c('Score','Win_Percentage')] 

# ====================== Are left handed fighters different in age or number of fights? ===================

wilcox.test(Total_Fights~Stance,data) 
wilcox.test(Age~Stance,data)

# ====================== Are left handers overrepresented? ====================================================

table(data$Stance)
binom.test(dim(data[data$Stance=='left-handed',])[1], dim(data)[1], p = 0.126,conf.level = 0.95,alternative = 'greater') # because the upper CI is 1 for some reason, I reported the 2 tailed confidence intervals in the paper

# ====================== Do they have better records? ====================================================
median(left$Score)
median(right$Score)

r1=wilcox.test(Score~Stance,data,alternative='greater')
r1
round(r1$statistic/(dim(right)[1]*dim(left)[1]),3)*100

median(left$Win_Percentage)
median(right$Win_Percentage)

r2=wilcox.test(Win_Percentage~Stance,data,alternative='greater')
r2
round(r2$statistic/(dim(left)[1]*dim(right)[1]),3)*100 # calculate common language effect size 

# ====================== Do left handers have greater variance in ability? ====================================================

variance_comparison('Win_Percentage',data)
variance_comparison('Score',data)

null_score=c() # create blank variables that we then fill
null_win_percentage=c()

for (i in 1:10000){
  null_sample1= sample(data$Score,replace = T) # shuffle the dataset
  null_sample2= sample(data$Win_Percentage,replace = T) # shuffle the dataset again for Win Percentage
  
  null_score[i]= var(null_sample1[1:table(data$Stance)[1]])-var(null_sample1[(table(data$Stance)[1]+1):nrow(data)]) # taking the first 1137 of a shuffled list is equivalent to randomly assigning the label "left-handed" to the unshuffled dataset.
  null_win_percentage[i]= var(null_sample2[1:table(data$Stance)[1]])-var(null_sample2[(table(data$Stance)[1]+1):nrow(data)]) # do this for both wn percentage and BoxRec score.
}

observed_diff_in_score_variances= var(left$Score)-var(right$Score) # the real difference in variance
observed_diff_in_wp_variances= var(left$Win_Percentage)-var(right$Win_Percentage) # the real difference in variance

1-(sum(observed_diff_in_score_variances>null_score)/length(null_score)) # p-value, one tailed, or 1- % null samples the observed difference was greater than
1-(sum(observed_diff_in_wp_variances>null_win_percentage)/length(null_win_percentage)) # p-value, one tailed

#==============================================================================================================================
#====================== Study 2: female boxers ===============================================================================
#==============================================================================================================================

data=read.csv('Female_boxers.csv')
data=filter(data,Win_Percentage>0.2,Total_Fights>4) # filter out unproven and "tomato cans"

right=filter(data,Stance=='right-handed')
left=filter(data,Stance=='left-handed') 

# ====================== Do left handed fighters differ in number fights?  ====================================================

wilcox.test(Total_Fights~Stance,data) 

# ====================== Are left handers overrepresented?  ====================================================

table(data$Stance)
# p = 0.099 because 9.9% of female general population are lefties
binom.test(dim(data[data$Stance=='left-handed',])[1], dim(data)[1], p = 0.099,conf.level = 0.95,alternative = 'greater')

# ====================== Are left handed fighters better?  ====================================================

median(left$Score)
median(right$Score)

r1=wilcox.test(Score~Stance,data,alternative='greater')
r1
round(r1$statistic/(dim(right)[1]*dim(left)[1]),3)*100

median(left$Win_Percentage)
median(right$Win_Percentage)

r2=wilcox.test(Win_Percentage~Stance,data,alternative='greater')
r2
round(r2$statistic/(dim(left)[1]*dim(right)[1]),3)*100 # calculate common language effect size 

# ====================== Do left handed fighters show higher variance in fighting sucess? ============
variance_comparison('Win_Percentage',data)
variance_comparison('Score',data)

#==============================================================================================================================
# ====================== Study 3: MMA fighters ================================================================================
#==============================================================================================================================
data= read.csv('MMA_fighters.csv')
data=filter(data,Total_Fights>4,Win_Percentage>.2) # filter out unproven and "tomato cans"

right=filter(data,Stance=='right-handed') 
left=filter(data,Stance=='left-handed') 

# ======== Do left handed fighters differ on other variables? ==================================

ggplot(right, aes(Reach))+geom_histogram()
ggplot(left, aes(Reach))+geom_histogram()
#looks normal enough for a t-test
t.test(right$Reach,left$Reach)

ggplot(right, aes(Weight))+geom_histogram()
ggplot(left, aes(Weight))+geom_histogram()
#looks normal enough for a t-test
t.test(right$Weight,left$Weight)

ggplot(right, aes(Height))+geom_histogram()
ggplot(left, aes(Height))+geom_histogram()
#looks normal enough for a t-test
t.test(right$Height,left$Height)

median(data$Total_Fights[data$Stance=='left-handed']) #median number of fights among lefties
median(data$Total_Fights[data$Stance=='right-handed']) #median number of fights among righties
# They differ, but is this significant?
wilcox.test(Total_Fights~Stance,data) # yes. We'll need to address this later.

# are left-handers over-represented? ==================================================
table(data$Stance)

#p is set to 12.6 because that's the % of left handed men in the general pop

binom.test(dim(data[data$Stance=='left-handed',])[1], dim(data)[1], p = 0.126,conf.level = 0.95) # compare to prevalence of L.handedness in men in gen pop

# ========= Do left handed fighters have better win:loss records? ===================================

r1=wilcox.test(Win_Percentage~Stance,data,alternative='greater') # one sided mann-whitney U
r1
round(r1$statistic/(dim(right)[1]*dim(left)[1]),3)*100 # common language effect size statistic

# ========= Do left handed MMA fighters show greater variance in fight records? =======================

variance_comparison('Win_Percentage',data)

#==============================================================================================================================
# ===== Study 4: evidence for frequency dependence? comparing female and male boxers ========================================== 
#==============================================================================================================================

men= read.csv('Male_Boxers.csv')
men=filter(men,Total_Fights>4&Win_Percentage>0.2) 
men.l=filter(men,Stance=="left-handed")
men.r=filter(men,Stance=="right-handed")

women= read.csv('Female_Boxers.csv')
women=filter(women,Total_Fights>4&Win_Percentage>0.2) 
women.l=filter(women,Stance=="left-handed")
women.r=filter(women,Stance=="right-handed")

men.w=wilcox.test(men.r$Score,men.l$Score,alternative="less")
women.w=wilcox.test(women.r$Score,women.l$Score,alternative="less")
ob.dif=women.w$statistic/((nrow(women.l))*nrow(women.r))-men.w$statistic/((nrow(men.l))*nrow(men.r))

#bootstrap
ss=10000
bs.dif=NULL
for (i in 1:ss){
    m.l.bs=sample(men.l$Score,nrow(men.l),replace=TRUE)
    m.r.bs=sample(men.r$Score,nrow(men.r),replace=TRUE)
    w.l.bs=sample(women.l$Score,nrow(women.l),replace=TRUE)
    w.r.bs=sample(women.r$Score,nrow(women.r),replace=TRUE)
    m.w.bs=wilcox.test(m.r.bs,m.l.bs,alternative="less")
    w.w.bs=wilcox.test(w.r.bs,w.l.bs,alternative="less")
    bs.dif=c(bs.dif,w.w.bs$statistic/((nrow(women.l))*nrow(women.r))-m.w.bs$statistic/((nrow(men.l))*nrow(men.r)))
}

#calculate bias parameter
z0hat=qnorm(sum(bs.dif>ob.dif)/ss)

#conduct jackknife analysis for acceleration parameter
theta.is=NULL
for (i in 1:nrow(men.l)){
    m.l.jk=men.l$Score[-i]
    w.jk=wilcox.test(men.r$Score,m.l.jk,alternative="less")
    theta.is=c(theta.is,women.w$statistic/((nrow(women.l))*nrow(women.r))-w.jk$statistic/((nrow(men.l)-1)*nrow(men.r)))
}
for (i in 1:nrow(men.r)){
    m.r.jk=men.r$Score[-i]
    w.jk=wilcox.test(m.r.jk,men.l$Score,alternative="less")
    theta.is=c(theta.is,women.w$statistic/((nrow(women.l))*nrow(women.r))-w.jk$statistic/((nrow(men.l))*nrow(men.r)-1))
}
for (i in 1:nrow(women.l)){
    w.l.jk=women.l$Score[-i]
    w.jk=wilcox.test(women.r$Score,w.l.jk,alternative="less")
    theta.is=c(theta.is,w.jk$statistic/((nrow(women.l)-1)*nrow(women.r))-men.w$statistic/((nrow(men.l))*nrow(men.r)))
}
for (i in 1:nrow(women.r)){
    w.r.jk=men.r$Score[-i]
    w.jk=wilcox.test(w.r.jk,women.l$Score,alternative="less")
    theta.is=c(theta.is,w.jk$statistic/((nrow(women.l))*nrow(women.r)-1)-men.w$statistic/((nrow(men.l))*nrow(men.r)))
}
U.i=(nrow(men)+nrow(women)-1)*(mean(theta.is)-theta.is)
a.hat=sum(U.i^3)/(6*sum(U.i^2)^(3/2))

a.obs=sum(bs.dif<0)/ss
p=1-pnorm((qnorm(a.obs)-z0hat)/(1+a.hat*(qnorm(a.obs)-z0hat))-z0hat)
p


#=========================================== END OF SCRIPT ====================================================================
#==============================================================================================================================


