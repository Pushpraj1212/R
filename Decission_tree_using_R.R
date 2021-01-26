library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)
##cvtools=to again cross validate the data
## Regression Tree

## Same DataPrep Steps As earlier module
## Copying as is from linear regression
setwd("\\Users\\pushpraj singh\\Desktop\\R Lang\\Data")
ld_train=read.csv("loan_data_train.csv",stringsAsFactors = F)

ld_test= read.csv("loan_data_test.csv",stringsAsFactors = F)

ld_test$Interest.Rate=NA
table(ld_train$Inquiries.in.the.Last.6.Months)
ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

library(dplyr)

glimpse(ld_all)

ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )

ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

ld_all=ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")),
         lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),
         lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")),
         lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
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

ld_all=CreateDummies(ld_all ,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)

library(tidyr)

ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_all=CreateDummies(ld_all,"Employment.Length",100)

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='train',col],na.rm=T)
  }
  
}

## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

##

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

### Building A Deecison Tree

ld.tree=tree(Interest.Rate~.-ID,data=ld_train1)

## Tree in text format

ld.tree

## Visual Format

plot(ld.tree)
text(ld.tree)

## Performance on validation set

val.IR=predict(ld.tree,newdata = ld_train2)

rmse_val=((val.IR)-(ld_train2$Interest.Rate))^2 %>% mean() %>% sqrt()
rmse_val


## Making final model on entire training
## and prediction on test/production data

ld.tree.final=tree(Interest.Rate~.-ID,data=ld_train)
test.pred=predict(ld.tree.final,newdata=ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

## Classification tree

## Data prep same as earlier module

rg_train=read.csv("rg_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("rg_test.csv",stringsAsFactors = FALSE)

rg_test$Revenue.Grid=NA

rg_train$data='train'
rg_test$data='test'

rg=rbind(rg_train,rg_test)

rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))

rg=rg %>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
  ) %>%
  select(-a1,-a2,-age_band)

cat_cols=c("status","occupation","occupation_partner","home_status",
           "family_income","self_employed",
           "self_employed_partner","TVarea","gender","region")

for(cat in cat_cols){
  rg=CreateDummies(rg,cat,50)
}

rg=rg %>%
  select(-post_code,-post_area)

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}



## For classification tree we'll need to convert response to factor type

rg$Revenue.Grid=as.factor(rg$Revenue.Grid)

## Rest of the data prep steps


for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-Revenue.Grid)

set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]

## building tree 

rg.tree=tree(Revenue.Grid~.-REF_NO,data=rg_train1)
rg.tree
## Performance on validation set

val.score=predict(rg.tree,newdata = rg_train2,type='vector')[,2]
pROC::roc(rg_train2$Revenue.Grid,val.score)$auc

## build model on entire data

rg.tree.final=tree(Revenue.Grid~.-REF_NO,
                   data=rg_train)

## Probability score prediciton on test/production data

test.score=predict(rg.tree.final,newdata=rg_test,type='vector')[,2]
write.csv(test.score,"mysubmission.csv",row.names = F)

## For hardclass prediciton we'll need to find a cutoff on score
## Process is same as for logistic regression

train.score=predict(rg.tree.final,newdata=rg_train,type='vector')[,2]
real=rg_train$Revenue.Grid

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

View(cutoff_data)
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)

## Regression Random Forest with Parameter Tuning
##for random forest we have different parameters like mtry(no of varaibles 
##which hare selected randomly at each node)
##ntree(number of tree in the forest),maxnode(no of maximum
##node to control size of tree),nodesize(total no of observation minimum 
##in the node so that it can split further it control overfitting to some extend)

## function for getting all possible combinations : expand.grid
params=list(mtry=c(5,10),ntree=c(100,500),
            maxnodes=c(15,20),nodesize=(c(2,5)))

expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999
my_params

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  p=my_params[i,]
  
  k=cvTuning(randomForest,Interest.Rate~.-ID,
             data =ld_train,
             tuning =p,
             folds = cvFolds(nrow(ld_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(p)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=p
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

best_params

## from another run following values were obtained
3
# myerror=1.870957
best_params=data.frame(mtry=20,
                       ntree=100,
                       maxnodes=50,
                       nodesize=10)

## Final model with obtained best parameters

ld.rf.final=randomForest(Interest.Rate~.-ID,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train)


auc(roc(rg_train$,val.score))

test.pred=predict(ld.rf.final,newdata = ld_test)
write.csv(test.pred,"new_sub.csv",row.names = F)

ld.rf.final

## Variable IMportance

d=importance(ld.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d=d %>% arrange(desc(IncNodePurity))

## Varimp Plot

varImpPlot(ld.rf.final)

## Partial Dependence Plot

var='fico'

pred.resp = predict(ld.rf.final,newdata=ld_train)
myvar = ld_train[,var]
myvar
trend.data=data.frame(Response=pred.resp,myvar=myvar)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_point()+geom_smooth()

## Classfication with Random Forest
setwd("\\Users\\pushpraj singh\\Desktop\\R Lang\\Data")
ci_train=read.csv("census_income.csv",
                  stringsAsFactors =F)
glimpse(ci_train)
table(ci_train$education,ci_train$education.num)

ci_train=ci_train %>% select(-education)
ci_train$Y=as.numeric(ci_train$Y==" >50K")
ci_train$Y=as.factor(ci_train$Y)

cat_var=names(ci_train)[sapply(ci_train,is.character)]

cat_var
for(var in cat_var){
  ci_train=CreateDummies(ci_train,var,500)
}

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
library(subselect)
num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Y~., 
             data =ci_train,
             tuning =params,
             folds = cvFolds(nrow(ci_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}

## Values obtained from an earlier run 

myauc=0.8945485
best_params=data.frame(mtry=5,
                       ntree=500,
                       maxnodes=100,
                       nodesize=10)

## Model on the entire training data

ci.rf.final=randomForest(Y~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ci_train
)

## Follow the same process as earlier for prediction on
## test production data
##since no test data 
# test.score=predict(ci.rf.final,newdata = ci_test,type='prob')[,2]
# write.csv(test.score,'mysubmission.csv',row.names = F)     

ci.rf.final

## Variable IMportance

d=importance(ci.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

## Varimp Plot

varImpPlot(ci.rf.final)

## Partial Dependence Plot

var='age'

pred.resp = predict(ci.rf.final,newdata=ci_train,type='prob')[,2]
myvar = ci_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()+geom_point()
