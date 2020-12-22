setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files\\project data")
getwd()
hr_train<- read.csv("hr_train.csv",header = T,sep=",")
hr_test<- read.csv("hr_test.csv",header = T,sep=",")

library(dplyr)
glimpse(hr_train)
# View(hr_train)
# class(hr_train$left)

# c <- tapply(hr_train$left, hr_train$salary, FUN=median)
# sort(b,decreasing = T)
# cat <- unique(hr_train$salary)
# table(hr_train$salary,hr_train$left)
# 
# cor_matrix = data.frame(cor(hr_train[,c("last_evaluation","average_montly_hours")]),method=c("pearson"))
# tab1 = xtabs(~left+Work_accident,data=hr_train)
# prop.table(tab1)
# b <- tapply(hr_train$average_montly_hours, hr_train$sales, FUN=median)
# sort(b,decreasing = T)
# c <- tapply(hr_train$Work_accident == 1, hr_train$left, FUN=prop.table)
# sort(b,decreasing = T)
# table(hr_train$Work_accident)/nrow(hr_train$left)
# var(hr_train$satisfaction_level)
# hr_train1 = hr_train %>% filter(left == 0) %>% select(satisfaction_level)
# hr_train2 = hr_train %>%  filter(left == 1) %>% select(salary)
# cor(last_evaluation,hr_train$average_monthly_hours,method = "pearson")
# hr_train$time_spend_company = as.numeric(hr_train$time_spend_company)
# hr_train3 = hr_train %>% filter(left == 1) %>% select(time_spend_company)
# median(hr_train3$time_spend_company)
# table(hr_train$number_project,hr_train$left)
# 
# View(hr_test)
# round(var(hr_train1),4)
# ggplot(aes(x=average_montly_hours),data = hr_train) + geom_density()


lapply(hr_train,function(x) length(unique(x)))

#Creating addition column
hr_test$left = NA
hr_test$data = 'test'
hr_train$data = 'train'

#combining train and test for data prep
hr_all = rbind(hr_train,hr_test)

glimpse(hr_all)

#finding null values
sapply(hr_all,function(x) sum(is.na(x)))
sapply(hr_train,function(x) sum(is.na(x)))
sapply(hr_test,function(x) sum(is.na(x)))

unique(hr_all$salary)
unique(hr_all$sales)
table(hr_all$salary)
table(hr_all$sales)
table(hr_all$salary)

hr_all$left= as.factor(hr_all$left)

#Creating dummy variables for SALES AND SALARY
hr_all1=hr_all

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

hr_all1=CreateDummies(hr_all1,"sales",750)
hr_all1=CreateDummies(hr_all1,"salary",1000)


# dumvarsala <- dummyVars("~ salary",data=hr_all1,fullRank = T)
# hr_all2 <- data.frame(predict(dumvarsala,newdata = hr_all1))
# glimpse(hr_all2)
# hr_all1 = cbind(hr_all1,hr_all2)
# hr_all1 = hr_all1 %>% select(-salary)
# glimpse(hr_all1)
# 
# dumvarsale <- dummyVars("~ sales",data=hr_all1,fullRank = TRUE)
# hr_all3 <- data.frame(predict(dumvarsale,newdata = hr_all1))
# glimpse(hr_all3)
# hr_all1 = cbind(hr_all1,hr_all3)
# hr_all1 = hr_all1 %>% select(-sales)

# glimpse(hr_all1)
# 
hr_all1 = hr_all1 %>% mutate(Work_accident = as.factor(Work_accident),
                             promotion_last_5years = as.factor(promotion_last_5years),
                             # sales = as.factor(sales),salary=as.factor(salary),
                             number_project=as.numeric(number_project),
                             average_montly_hours = as.numeric(average_montly_hours),
                             time_spend_company = as.numeric(time_spend_company))

table(hr_train$sales)
glimpse(hr_all1)
## seperating test and train data
train = hr_all1 %>% filter (data == 'train') %>% select(-data)
test = hr_all1 %>% filter(data == 'test') %>% select(-data,-left)
# View(train)
# View(test)
sum(is.na(train))

for_vif = lm(left ~., data = train) 
vif(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

#Only training data for modelling 
set.seed(20)
s = sample(1:nrow(train),0.75*nrow(train))
train1=train[s,]
train2=train[-s,]

hr.rantree=randomForest(as.factor(left)~.,data=train1)
val.score2=predict(hr.rantree,newdata=train2,type='prob')[,1]
score = pROC::roc(train2$left,val.score2)$auc
round(score,2)
auc(roc(train2$left,val.score2))

hr.rantree.final = randomForest(as.factor(left)~.,data = train)
test.score=predict(hr.rantree.final,newdata = test,type = 'prob')[,1]
write.csv(test.score,"Gayatri_Srinivasan_P4_part2.csv",row.names = F)

################################
train_index = createDataPartition(train$left,p=0.75,list = F)
train1=train[train_index,]
train2=train[-train_index,]









library(car)
# fit_lm = lm(left~.,data=train1)
# vif(fit_lm)
# sort(vif(fit_lm),decreasing = TRUE)
# fit = glm(left~.-saleshr-salesmarketing-salesproduct_mng-salestechnical-salessupport
#           ,data=train1,family="binomial")
# fit = glm(left ~ satisfaction_level + last_evaluation + number_project + 
#           average_montly_hours + time_spend_company + Work_accident + 
#           promotion_last_5years + salarylow + salarymedium + saleshr + 
#           salesIT + salesmanagement + salesmarketing + salesproduct_mng + 
#           salesRandD + salessales + salessupport + salestechnical - 
#          saleshr - salesmarketing - salesproduct_mng - salestechnical - salessupport
#        ,data=train1,family="binomial")
# fit=step(fit)
# formula(fit)
# summary(fit)
# 
# #predicting for validation train2
# val.score = predict(fit,newdata=train2,type="response")
# auc(roc(train2$left,val.score))
# 
# # 
hr.tree=tree(left~.,data=train1)
val.score1=predict(hr.tree,newdata=train2,type='vector')[,2]
pROC::roc(train2$left,val.score1)$auc


