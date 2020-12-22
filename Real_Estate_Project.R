setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files\\project data")
setwd("C:\\Users\\acer\\Desktop\\Data\\Data Files")

getwd()
real_train<- read.csv("housing_train.csv",header = T,sep=",",stringsAsFactors = F)
real_test<- read.csv("housing_test.csv",header = T,sep=",",stringsAsFactors = F)

library(dplyr)
summary(real_train)
# a <- mean(real_train[real_train$Type == "h",c("Price")])
# b <- mean(real_train[real_train$Type == "t",c("Price")])
# a-b
# View(real_train)
# View(real_test)
# sapply(real_test,function(x) sum(is.na(x)))
# sapply(real_train,function(x) sum(is.na(x)))
# glimpse(real_train)
# lapply(real_train,function(x) length(unique(x)))
# names(real_train)[sapply(real_train,function(x) is.character(x))]
# unique(real_train$Subur) 
# unique(real_train$Address) #omit
# unique(real_train$Type)
# unique(real_train$Method)
# unique(real_train$SellerG)
# unique(real_train$CouncilArea)
# unique(real_train$Postcode)
# hist(as.numeric(real_train$Distance))
# xtabs(~SellerG+Price,data=real_train)
# library(ggplot2)
# ggplot(aes(x=),data = real_train) + geom_density()
# a <- tapply(real_train$Price, real_train$SellerG, FUN=sum)
# sort(a,decreasing = T)
# 
# b <- tapply(real_train$Price, real_train$CouncilArea, FUN=var)
# sort(b,decreasing = T)
# var(real_train$Price)
# 
# real_train = real_train %>% select(unique(real_train$SellerG))
#Creating addition column
real_test$Price = NA
real_test$data = 'test'
real_train$data = 'train'

#combining train and test for data prep
real_all = rbind(real_train,real_test)

glimpse(real_all)

#Removing address field
real_all1 = real_all %>% select(-Address)

## Removing NA values
# sapply(real_train,function(x) sum(is.na(x)))
sapply(real_all1,function(x) sum(is.na(x)))

# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# var_na <- colnames(real_all)[apply(is.na(real_all), 2, any)]
 
real_all1$Car[is.na(real_all1$Car)]=round(mean(real_all1$Car,na.rm=T),0)
real_all1$Bedroom2[is.na(real_all1$Bedroom2)]=round(mean(real_all1$Bedroom2,na.rm=T),0)
real_all1$Bathroom[is.na(real_all1$Bathroom)]=round(mean(real_all1$Bathroom,na.rm=T),0)
real_all1$Landsize[is.na(real_all1$Landsize)]=round(mean(real_all1$Landsize,na.rm=T),0)
real_all1$BuildingArea[is.na(real_all1$BuildingArea)]=round(mean(real_all1$BuildingArea,na.rm=T),0)
real_all1$YearBuilt[is.na(real_all1$YearBuilt)]=round(mean(real_all1$YearBuilt,na.rm=T),0)

summary(real_all1)
##########Outliers
ucland <- mean(real_all1$Landsize) + (3 * (sd(real_all1$Landsize)))
real_all1$Landsize[(real_all1$Landsize > ucland)] <- ucland
#Converting Categirocal to numerical
names(real_all1)[sapply(real_all1,function(x) is.character(x))]
real_all2 = real_all1

# install.packages("qdapTools")
# library(qdapTools)
install.packages("caret")
library(caret)
#Type
dumvart <- dummyVars("~ Type",data=real_all2,fullRank = TRUE)
real_all3 <- data.frame(predict(dumvart,newdata = real_all2))
glimpse(real_all3)
real_all2 = cbind(real_all2,real_all3)

#Method
dumvarm <- dummyVars("~ Method",data=real_all2,fullRank = TRUE)
real_all4 <- data.frame(predict(dumvarm,newdata = real_all2))
glimpse(real_all4)
real_all2 = cbind(real_all2,real_all4)

#SellerG
# dumvars <- dummyVars("~ SellerG",data=real_all2,fullRank = TRUE)
# real_all5 <- data.frame(predict(dumvars,newdata = real_all2))
# glimpse(real_all5)
# real_all2 = cbind(real_all2,real_all5)
t=sort(table(real_all2$SellerG))
t1=round(tapply(real_all2$Price,real_all2$SellerG,mean,na.rm=T),0)
sort(t1)

real_all2 = real_all2 %>%
mutate(
Seller_1 = as.numeric(SellerG%in%c("hockingstuart/Advantage","hockingstuart/Village","Iconek","Oak")),
Seller_2 = as.numeric(SellerG%in%c("Calder","Pagan","D'Aprano","Ross","Homes","Trimson","iTRAK","Jason")),
Seller_3 = as.numeric(SellerG%in%c("Redina","Wood","CASTRAN","Lucas","Dingle","Luxton","Anderson","Buxton/Advantage","Morleys","Luxe","LITTLE")),
Seller_4 = as.numeric(SellerG%in%c("Walshe","Compton","iOne","Ken","FN","Dixon","LJ","Nguyen","W.B.","Buxton/Find","Douglas",
                                   "Grantham","Elite","YPA","Coventry","Eview","Burnham","Joseph","Love","Geoff","Prowse","Stockdale","MICM","William")),
Seller_5 = as.numeric(SellerG%in%c("Direct","Brace","Haughton","GL","S&L","Alexkarbon","Hunter","VICPROP","Purplebricks","Harcourts","Beller",
                                   "Bells","Prof.","Thomas","Moonee","Crane","Propertyau","Darren","Buckingham","Nardella","Sweeney","Nicholson","Del","Australian")),
Seller_6 = as.numeric(SellerG%in%c("Brad","Whiting","Thomson","Besser","Raine&Horne","Barlow","Jas","Century","Barry","New","Edward",
                                   "Raine","Re","First","Ray","Galldon","Gunn&Co","Charlton","Peter","Melbourne","Harrington","Win","JMRE","Meadows")),
Seller_7 = as.numeric(SellerG%in%c("Karen","O'Brien","Bayside","Allan","Professionals","RE","Village","Williams","Nelson","Ham","Considine",
                                   "Johnston","Maddison","Scott","hockingstuart","Owen","Biggin","Woodards","Rendina","Chisholm","Gary","Win")),
Seller_8 = as.numeric(SellerG%in%c("McDonald","Naison","Changing","McGrath","Wilson","Airport","Miles","Domain","Chambers","Weda","HAR",
                                   "Garvey","Parkes")),
Seller_9 = as.numeric(SellerG%in%c("Vic","Allens","Greg","C21","Leased","Holland","Paul","Parkinson","Buxton","Philip")),
Seller_10 = as.numeric(SellerG%in%c("David","Zahn","Frank","Tim","RW","Noel","hockingstuart/Buxton","Christopher","Hodges","J","Lindellas","Fletchers","Collins","Cayzer")),
Seller_11 = as.numeric(SellerG%in%c("Appleby","Rodney","Bekdon","Hamilton","Jellis","Matthew","O'Donoghues")),
Seller_12 = as.numeric(SellerG%in%c("RT","Caine")),
Seller_13 = as.numeric(SellerG%in%c("Ascend","Castran","Walsh","Morrison")),
Seller_14 = as.numeric(SellerG%in%c("Sotheby's")),
Seller_15 = as.numeric(SellerG%in%c("Abercromby's","Nick")),
Seller_16 = as.numeric(SellerG%in%c("Fletchers/One","Marshall")),
Seller_17 = as.numeric(SellerG%in%c("One","Red","Kay")),
Seller_18 = as.numeric(SellerG%in%c("Kelly","R&H")),
Seller_19 = as.numeric(SellerG%in%c("Assisi","Blue")),
Seller_20 = as.numeric(SellerG%in%c("Hooper")),
Seller_21 = as.numeric(SellerG%in%c("Weast"))
)

#CouncilArea
dumvarc <- dummyVars("~ CouncilArea",data=real_all2,fullRank = TRUE)
real_all6 <- data.frame(predict(dumvarc,newdata = real_all2))
glimpse(real_all6)
real_all2 = cbind(real_all2,real_all6)

#Suburb
t=table(real_all2$Suburb)
t1=round(tapply(real_all2$Price,real_all2$Suburb,mean,na.rm=T),0)
t1=sort(t1)

real_all2=real_all2 %>% 
mutate(
sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
select(-Suburb,-Type,-Method,-SellerG,-CouncilArea)

#Look on data
glimpse(real_all2)
View(real_all2)
summary(real_all2)
lapply(real_all2,function(x) sum(is.na(x)))


## seperating test and train data
train = real_all2 %>% filter (data == 'train') %>% select(-data)
test = real_all2 %>% filter(data == 'test') %>% select(-data,-Price)
View(train)
View(test)
sum(is.na(train))

#Only training data for modelling
set.seed(2)
s = sample(1:nrow(train),0.7*nrow(train))
train1=train[s,]
train2=train[-s,]

fit = lm(Price~.-Seller_7-sub_3-Postcode,data=train1)
fit = lm(Price~.-Seller_7-sub_3-Postcode+Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize +
           BuildingArea + YearBuilt + Typet + Typeu + MethodS  +
           MethodSP + Seller_5 + Seller_9 + Seller_10  +
           Seller_13   + Seller_16 +
           Seller_17  + Seller_19 + Seller_20 + Seller_21 +
           CouncilAreaBayside + CouncilAreaBoroondara + CouncilAreaDarebin +
           CouncilAreaGlen.Eira + CouncilAreaMelbourne  +
           CouncilAreaPort.Phillip  + CouncilAreaWhitehorse + sub_1 + sub_2 + sub_5 + sub_6 + sub_7 +
           sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + sub_14 +
           sub_15 + sub_16 - Seller_12 - Seller_1-CouncilAreaKingston - Seller_15 -CouncilAreaHume
          -MethodVB-CouncilAreaBanyule-Seller_14-CouncilAreaBoroondara-CouncilAreaStonnington
          -CouncilAreaYarra -CouncilAreaMoreland-Seller_2-CouncilAreaManningham-Seller_11
          -CouncilAreaMaribyrnong -Seller_8 -CouncilAreaHobsons.Bay-CouncilAreaBrimbank
          -CouncilAreaMonash -MethodSA-CouncilAreaMoonee.Valley-Seller_18,
          data=train1)
fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
         BuildingArea + YearBuilt + Typet + Typeu + MethodS + MethodSA + 
         MethodSP + Seller_5 + Seller_8 + Seller_9 + Seller_10 + Seller_11 + 
         Seller_12 + Seller_13 + Seller_14 + Seller_15 + Seller_16 + 
         Seller_17 + Seller_18 + Seller_19 + Seller_20 + Seller_21 + 
         CouncilAreaBayside + CouncilAreaBoroondara + CouncilAreaDarebin + 
         CouncilAreaGlen.Eira + CouncilAreaMelbourne + CouncilAreaMonash + 
         CouncilAreaPort.Phillip + CouncilAreaStonnington + CouncilAreaWhitehorse + 
         CouncilAreaYarra + sub_1 + sub_2 + sub_5 + sub_6 + sub_7 + 
         sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + sub_14 + 
         sub_15 + sub_16,data=train1)
summary(fit)
install.packages("car")
library(car)
vif(fit)
sort(vif(fit),decreasing = TRUE)
?vif
linearHypothesis(fit)
fit=step(fit)
formula(fit)

val.pre = predict(fit,newdata = train2)
errors = train2$Price - val.pre
rmse = errors**2 %>% mean() %>% sqrt() 

#Calculating score
score = 212467/rmse
round(score,2)

fit.final = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
                 BuildingArea + YearBuilt + Typet + Typeu + MethodS + MethodSA + 
                 MethodSP + Seller_5 + Seller_8 + Seller_9 + Seller_10 + Seller_11 + 
                 Seller_12 + Seller_13 + Seller_14 + Seller_15 + Seller_16 + 
                 Seller_17 + Seller_18 + Seller_19 + Seller_20 + Seller_21 + 
                 CouncilAreaBayside + CouncilAreaBoroondara + CouncilAreaDarebin + 
                 CouncilAreaGlen.Eira + CouncilAreaMelbourne + CouncilAreaMonash + 
                 CouncilAreaPort.Phillip + CouncilAreaStonnington + CouncilAreaWhitehorse + 
                 CouncilAreaYarra + sub_1 + sub_2 + sub_5 + sub_6 + sub_7 + 
                 sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + sub_14 + 
                 sub_15 + sub_16,
               data=train)

test.pred = predict(fit.final,newdata = test)

# errors1 = train$Price - test.pred
# rmse1 = errors1**2 %>% mean() %>% sqrt() 

#Calculating score
score1 = 212467/rmse
score1
write.csv(test.pred,"Submission_Real_Estate.csv",row.names = FALSE)
?write.csv
plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

