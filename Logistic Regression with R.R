
setwd("\\Users\\pushpraj singh\\Desktop\\R Lang\\Data")
## ----
rg_train=read.csv("rg_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("rg_test.csv",stringsAsFactors = FALSE)
table(rg_train$Revenue.Grid)

library(dplyr)
glimpse(rg_train)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

rg_test$Revenue.Grid=NA

rg_train$data='train'
rg_test$data='test'

rg=rbind(rg_train,rg_test)

## ----
table(rg$children)

## ------------------------------------------------------------------------
rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,
                         substr(children,1,1)),
         children=as.numeric(children))



## ------------------------------------------------------------------------
table(rg$age_band)

## ------------------------------------------------------------------------

## ----
rg=rg %>%
mutate(a1=as.numeric(substr(age_band,1,2)),
       a2=as.numeric(substr(age_band,4,5)),
      age=ifelse(substr(age_band,1,2)=="71",71,
                 ifelse(age_band=="Unknown",NA,
                        0.5*(a1+a2)))
       ) %>%
  select(-a1,-a2,-age_band)

## we could have done something similar for variable family income also

## ------------------------------------------------------------------------
glimpse(rg)

lapply(rg,function(x) length(unique(x)))

names(rg)[sapply(rg,function(x) is.character(x))]

# we'll exclude column named data as it simply represent which dataset the observation is from

cat_cols=c("status","occupation","occupation_partner",
           "home_status","family_income","self_employed",
           "self_employed_partner","TVarea","gender","region")

for(cat in cat_cols){
  rg=CreateDummies(rg,cat,50)
}

glimpse(rg)
## ------------------------------------------------------------------------
rg=rg %>%
  select(-post_code,-post_area)

sum(sapply(rg,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

lapply(rg,function(x) sum(is.na(x)))

for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-Revenue.Grid)
## ------------------------------------------------------------------------

set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]


## ----
library(car)
##we will only look at the VIF value not the P value
#Will remove the predictor which do not make any sense
for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

## from here we'll remove vars with high vif one by one, code below is arrived
## at after multiple iterations

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast-TVarea_Central,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast-TVarea_Central-occupation_Professional
           ,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast-TVarea_Central-occupation_Professional
           -family_income_GT_EQ_35000,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-region_SouthEast-TVarea_Central-occupation_Professional
           -family_income_GT_EQ_35000-region_Scotland,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]


for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
           -Investment.in.Derivative-Investment.in.Equity
           -region_SouthEast-TVarea_Central
           -occupation_Professional-family_income_GT_EQ_35000
           -region_Scotland-Portfolio.Balance,
           data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]
###

log_fit=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
            -Investment.in.Derivative-Investment.in.Equity
            -region_SouthEast-TVarea_Central
            -occupation_Professional-family_income_GT_EQ_35000
            -region_Scotland-Portfolio.Balance,
            data=rg_train1,family = "binomial")

log_fit
log_fit=step(log_fit)
# this might take 5-6 minutes to finish 
formula(log_fit)

log_fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
              Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
              Personal.Loan  + Investment.Tax.Saving.Bond + 
              Home.Loan + Online.Purchase.Amount   +  self_employed_partner_No + TVarea_ScottishTV + TVarea_Meridian ,
            data=rg_train,family='binomial')
summary(log_fit)

# from here we can drop vars one by one which had higher p-value
# code given below is result of multiple iterations


#### performance of score model on validation data
##TO GET PREDICTION ON Probability score

##the response will give the Probability score
install.packages("pROC")
library(pROC)
?pROC
##val.score will give the predicted probability for outcome 1

val.score=predict(log_fit,newdata = rg_train2,type='response')

##this will give the tentative value for auc/Performce for our model
auc(roc(rg_train2$Revenue.Grid,val.score))

# so the tentative score performance of logistic regression is going to be around 0.95
# now lets build the model on entire training data

# code given below is result of multiple iterations

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
           -Investment.in.Derivative-Investment.in.Equity
           -region_SouthEast-TVarea_Central-occupation_Professional
           -family_income_GT_EQ_35000-region_Scotland-Portfolio.Balance
           ,data=rg_train)

sort(vif(for_vif),decreasing = T)[1:3]

log.fit.final=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
                  -Investment.in.Derivative-Investment.in.Equity
                  -region_SouthEast-TVarea_Central-occupation_Professional
                  -family_income_GT_EQ_35000-region_Scotland-Portfolio.Balance,
                  data=rg_train,family='binomial')

log.fit.final=step(log.fit.final)
# this will again take a long time to run

summary(log.fit.final)

formula(log.fit.final)

log.fit.final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
                    Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                    Personal.Loan + Investment.Tax.Saving.Bond + 
                    Home.Loan + Online.Purchase.Amount + status_Partner + occupation_partner_Retired + 
                     TVarea_Meridian ,
                  data=rg_train,family='binomial')

summary(log.fit.final)

# now if we needed to submit probability scores for the test data we can do at this point
#we can only check the tentative performace on training data not on test data
#like for the test data we have calculated the performace of our model by auc score
#and we suppose the same performance for the test data but there is no exact way to do this

test.prob.score= predict(log_fit,newdata = rg_test,type='response')
write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)

# however if we need to submit hard classes, we'll need to determine cutoff score
#like we need the output as 0 and 1 or yes and no
train.score=predict(log.fit.final,newdata = rg_train,type='response')

##checking the cut ff value from .001 to .999

real=rg_train$Revenue.Grid
real
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  ##caculating true positive/True Negative/False Positive/False Negative
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  #Sn is Sensitivity means how much positive you are able to capture accurately
  Sn=TP/P
  #Sp is Specifity means how much negative you are able to capture accuratey
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  ##calculating F beta when Beta is 5 an .1
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  ##calculting custom matrix which penlizes false matrix
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
##the above loop will give the value for different cut off

#### visualise how these measures move across cutoffs
###if we want to visualise indivadaul value like for which
##cutoff the value of KS is Highest
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=M))+geom_line()
ggplot(cutoff_data,aes(x=cutoff,y=KS))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)


##it will plot the different performave matrix with the cut off value
ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()

##let suppose i want tot determine the cut off where KS is maximum
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes(in 1 and 0)

test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name_1.csv",row.names = F)


