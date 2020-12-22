setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files\\project data")
getwd()
store_train<- read.csv("store_train.csv",header = T,sep=",")
store_test<- read.csv("store_test.csv",header = T,sep=",")
glimpse(store_train)
i = unique(store_train$store_Type)
t1=round(tapply(store_train$sales,store_train$state_alpha,mean,na.rm=T),0)
prop.table(xtabs(~store_train$store+store_train$state_alpha,mean()))

length(unique(store_train$Areaname))
store_train$sales[unique(store_train$store_Type) ==  "Supermarket Type1" & unique(store_train$Areaname) == "Kennebec County, ME"]
store_train$sales = store_train$sales0 + store_train$sales1 +store_train$sales2 + store_train$sales3 + store_train$sales4
q = xtabs(~store_train$store+store_train$store_Type,data=store_train)
round(prop.table(q),2)
hist(store_train$sales)
summary(store_train$sales)
sapply(unique(store_train$store_Type),var(store_train$sales))
d = xtabs(~store_train$store_Type+store_train$sales,data= store_train)
prop.table(store_train$store_Type,store_train$sales)
c  = var(store_train$sales)
3422-(1.5*(4969-3422))
boxplot.stats(store_train$sales)$out
a =tapply(store_train$sales,store_train$store_Type,fun=var())

b = var(store_train$sales)



unique(store_train$state_alpha)


bank-full_test
setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files\\project data")
getwd()
b_test<- read.csv("bank-full_test.csv",header = T,sep=",")
b_train<- read.csv("bank-full_train.csv",header = T,sep=",")
glimpse(b_train)
round(mean(b_train$age),2)
summary(b_train$balance)
lower= 72 - (1.5*(1414-72))
high = 1414 + (1.5 * (1414-72))
?boxplot()
boxplot.stats(b_train$balance)$out

var(b_train$balance)
boxplot(b_train$balance)


glimpse(store_train)
unique(store_train$store_Type)

store_test$store = NA
store_test$data = 'test'
store_train$data = 'train'

#combining train and test for data prep
store_all = rbind(store_train,store_test)

glimpse(store_all)

sapply(store_all,function(x) sum(is.na(x)))
unique(store_all$storecode)
unique(store_all$population)
sum(is.na(store_all$population))
store_all$population[is.na(store_all$population)]=round(mean(store_all$population,na.rm=T),0)
store_all$country[is.na(store_all$country)]=round(mean(store_all$country,na.rm=T),0)

store_all$store= as.factor(store_all$store)
store_all1 = store_all

store_all1$storecode = substring(store_all$storecode,1,5)
store_all1 = store_all1 %>% select(-countyname,-countytownname,-Areaname,-state_alpha)
store_all1 = store_all1 %>% select(-state_alpha)

store_all1 = store_all1 %>% mutate(storecode = as.factor(storecode),
                                   # state_alpha = as.factor(state_alpha),
                                   store_Type = as.factor(store_Type))

train = store_all1 %>% filter (data == 'train') %>% select(-data)
test = store_all1 %>% filter(data == 'test') %>% select(-data,-store)
# View(train)
# View(test)
sum(is.na(train))

#Only training data for modelling 
set.seed(20)
s = sample(1:nrow(train),0.75*nrow(train))
train1=train[s,]
train2=train[-s,]

store.rantree=randomForest(as.factor(store)~.-Id,data=train1)
val.score2=predict(store.rantree,newdata=train2,type='prob')[,2]
score = pROC::roc(train2$store,val.score2)$auc
round(score,2)
auc(roc(train2$store,val.score2))

store.rantree.final = randomForest(as.factor(store)~.-Id,data = train)
test.score=predict(store.rantree.final,newdata = test,type = 'prob')[,1]
write.csv(test.score,"Gayatri_Srinivasan_P2_part2.csv",row.names = F)

