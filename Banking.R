setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files\\project data")
bd_train = read.csv("bank-full_train.csv")
bd_test =  read.csv("bank-full_test.csv")
glimpse(bd_train)
bd_test$y = NA
bd_test$data = 'test'
bd_train$data = 'train'

#combining train and test for data prep
bd_all = rbind(bd_train,bd_test)

table(bd_all$y)
bd_all$y = as.numeric(bd_all$y == "no") 
glimpse(bd_all)

bd_all = bd_all %>% select(-ID)

round(prop.table(table(bd_all$poutcome,bd_all$y),1),2)
#contact,educationn,marital,job

round(prop.table(table(bd_all$contact,bd_all$y),1),2)
bd_all = bd_all %>% mutate(
  bd_contact_1 =as.numeric(contact %in% c("cellular","telephone")),
  bd_contact_2 = as.numeric(contact == "unknown")
) %>% select(-contact)

round(prop.table(table(bd_all$education,bd_all$y),1),2)
bd_all = bd_all %>% mutate(
  bd_education_1 =as.numeric(education %in% c("secondary","tertiary")),
  bd_education_2 = as.numeric(education == "unknown")
) %>% select(-education)

round(prop.table(table(bd_all$marital,bd_all$y),1),2)
bd_all = bd_all %>% mutate(
  bd_marital =as.numeric(marital %in% c("divorced","married"))
) %>% select(-marital)

round(prop.table(table(bd_all$job,bd_all$y),1),2)
bd_all = bd_all %>% mutate(
  bd_job_1 =as.numeric(job %in% c("blue-collar","entrepreneur","housemaid","services")),
  bd_job_2 = as.numeric(job %in% c("admin.","management","self-employed",
                                         "technician","unemployed")),
  bd_job_3 = as.numeric(job == "unknown")
) %>% select(-job)


bd_all = bd_all %>% select(-bd_contact_2)

bd_all = bd_all %>% mutate(
  bd_poutcome_1 =as.numeric(poutcome %in% c("failure","other")),
  bd_potcome_2 = as.numeric(poutcome == "unknown")
) %>% select(-poutcome)

# prop.table(bd_all$y,bd_all$loan)
bd_all$loan = as.numeric(bd_all$loan == "no")
unique(bd_all$default)
bd_all$housing = as.numeric(bd_all$housing == "no")
bd_all$default = as.numeric(bd_all$default == "no")

bd_all = bd_all %>% mutate(
  q1 = as.numeric(month %in% c("jan","feb","mar")),
  q2 = as.numeric(month %in% c("apr","may","jun")),
  q3 = as.numeric(month %in% c("jul","aug","sep")),
) %>% select(-month)

train = bd_all %>% filter (data == 'train') %>% select(-data)
test = bd_all %>% filter(data == 'test') %>% select(-data,-y)


set.seed(2)
s=sample(1:nrow(train),0.75*nrow(train))

train_data=train[s,]
test_data=train[-s,]

## develope logistic regression model for train

# take care of vif first 
library(car)
for_vif = lm(y ~., data = train_data) 
vif(for_vif)

fit_train=glm(y~.,data=train_data,family="binomial")

fit_train=step(fit_train)


score.train=predict(fit_train,train_data,type = "response")
real=train_data$y

# finding cutoff based on KS from train data

cutoffs=round(seq(0,1,length=100),3)
cutoff_data=data.frame(cutoff=999,KS=999)

for(cutoff in cutoffs){
  pred=as.numeric(score.train>cutoff)
  TP=sum(real==pred & real==1)
  TN=sum(real==pred & real==0)
  FP=sum(real!=pred & real==0)
  FN=sum(real!=pred & real==1)
  P=TP+FN
  N=TN+FP
  KS=(TP/P)-(FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}

cutoff_data=cutoff_data[-1,]
View(cutoff_data)
KS.cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]

# performance of this logistic model on the test data

test$score=predict(fit_train,newdata=test,type="response")

test$predicted=as.numeric(test$score>KS.cutoff)

table(test$target,test$predicted)
# Performance
Acc=(4816+1654)/8141=0.795
Sn=1654/1963 =0.843
Sp=4816/6178 =0.779
## DTree
library(tree)

train$target=as.factor(train$target)
single.tree=tree(target~.,data=train)


cv.single.tree=cv.tree(single.tree,FUN=prune.misclass)

plot(cv.single.tree$size,cv.single.tree$dev,type='b')

pruned.tree=prune.misclass(single.tree,best=5)

plot(pruned.tree)
text(pruned.tree,pretty = 0)

target.tree=predict(pruned.tree,newdata=test,type="class")

table(test$target,target.tree)
# Performance
Acc=(5862+1025)/8141=0.846
Sn=1025/1963= 0.522
Sp=5862/6178=0.949
## Random Forest

library(randomForest)
rf=randomForest(target~.,data=train,do.trace=T)

test.rf=predict(rf,newdata=test)

varImpPlot(rf)

table(test$target,test.rf)

# Performance
Acc=(1228+5796)/8141 =0.863
Sn=1228/1963 =0.626
Sp=5796/6178 =0.938
# AUC ROC for logistic

library(pROC)
roccurve = roc(train$target ~ score.train)
plot(roccurve)
auc(roccurve)

x