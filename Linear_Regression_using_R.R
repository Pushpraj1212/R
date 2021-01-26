setwd("\\Users\\pushpraj singh\\Desktop\\R Lang\\Data")
getwd()


ld_train=read.csv("loan_data_train.csv",stringsAsFactors = F)

ld_test= read.csv("loan_data_test.csv",stringsAsFactors = F)
View(ld_train)

#adding new column to the test data since it does not have interest rate
ld_test$Interest.Rate=NA

#to identify the data we are adding the new column data and assigning the test and train

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL
View(ld_all)

library(dplyr)

glimpse(ld_all)

ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )

glimpse(ld_all)

table(ld_all$Loan.Length)

ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)
View(ld_all)

table(ld_all$Loan.Purpose)
#the way category impact on the repose is the average response.
#so we take the average of all the categories and then group it
#so we are clubing category according to the average response on Interest rate

round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T))

View(ld_all)



# 10
# 11
# 12
# 13
# 14

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)

ld_all=CreateDummies(ld_all ,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)


library(tidyr)

ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

which(is.na(ld_all$Employment.Length_LT_1year))

ld_all=CreateDummies(ld_all,"Employment.Length",100)

## NA values

lapply(ld_all,function(x) sum(is.na(x)))

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=="train",col],na.rm=T)
  }
  
}

## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

##to check the tentative performace of the model breakin the train data to 
##into two part to check if it is giving correct result or not

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

fit=lm(Interest.Rate~.-ID,data=ld_train1)
summary(fit)
library(car)
# we'll take vif cutoff as 05
?vif
View(ld_train1)
sort(vif(fit),decreasing = T)

fit=lm(Interest.Rate~.-ID-lp_14,data=ld_train1)

sort(vif(fit),decreasing = T)

# p-value take the cutoff .05
#drop the vaiable whose p value is greater than 0.05
#if the value is greter than .05 means beta is near to zero and discard the variable
#we are droping becasue those variable are not contributing enough
summary(fit)

#we drop the variable according the AIC score using step function

fit=step(fit)

## AIC score 
?AIC
summary(fit)

#the formula function will give the predicted variable used
formula(fit)

fit=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36 + lp_10 + State_TX + 
         Home.Ownership_MORTGAGE + fico + Employment.Length_5years + 
         Employment.Length_10years,data=ld_train1)

summary(fit)

##droping the Employment length 5 year as its p value is grater the 0.05

fit=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36 + lp_10 + State_TX + 
         Home.Ownership_MORTGAGE + fico + Employment.Length_10years,data=ld_train1)
summary(fit)

##remove monthly income as it p value is greater than 0.05
fit=lm(Interest.Rate ~ Amount.Requested  + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36 + lp_10 + State_TX + 
         Home.Ownership_MORTGAGE + fico + Employment.Length_10years,data=ld_train1)
summary(fit)

##drop lp_10 as it p value is greater than .05
fit=lm(Interest.Rate ~ Amount.Requested  + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36 + State_TX + 
         Home.Ownership_MORTGAGE + fico + Employment.Length_10years,data=ld_train1)
summary(fit)

###Now we do prediction for the train2 data using the function predict
##to get the tentative performance of data

val.pred=predict(fit,newdata=ld_train2)

val.pred

errors=ld_train2$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

fit.final=fit=lm(Interest.Rate ~ .-ID-lp_14,
                 data=ld_train)

sort(vif(fit.final),decreasing = T)
sum(is.na(ld_all$Interest.Rate))

fit.final=step(fit.final)

summary(fit.final)

formula(fit.final)

fit.final=lm()

fit.final=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
               Inquiries.in.the.Last.6.Months + ll_36 + State_TX + Home.Ownership_MORTGAGE + 
               fico + Employment.Length_5years + Employment.Length_10years,data=ld_all)

summary(fit.final)

fit.final=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
               Inquiries.in.the.Last.6.Months + ll_36 + State_TX + Home.Ownership_MORTGAGE + 
               fico + Employment.Length_10years,data=ld_all)

summary(fit.final)

fit.final=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
               Inquiries.in.the.Last.6.Months + ll_36 + State_TX + Home.Ownership_MORTGAGE + 
               fico ,data=ld_all)

summary(fit.final)

fit.final=lm(Interest.Rate ~ Amount.Requested + Open.CREDIT.Lines + 
               Inquiries.in.the.Last.6.Months + ll_36 + State_TX + Home.Ownership_MORTGAGE + 
               fico ,data=ld_all)

summary(fit.final)

test.pred=predict(fit.final,newdata=ld_test)
test.pred

write.csv(test.pred,"submision1.csv",row.names = F)

### 

plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not
#it shows non linearity exist in data or not
#Ideally it should be straight line
#since it is not flat red line means there are some varible which has non linear relation 
#so this tell if we try other model we might get better performance

plot(fit.final,2) # errors are normal or not
#if the value is falling in the dotted line mean the error is following
#the normal distribution and this is important for the reliable p value
#as p value we assue that the error follow the normal distribution

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1
#to check if there exsist any extreme value in data ser or not
#using cook distnace the value should not be more than 1

#### 

output=summary(fit.final)
names(output)

output$coefficients[,4]


