#조건문 기반 예측 모델 생성
#if Sex==male then 0
#else then 1

#모델평가
#890중 500명 예측
#test.csv에 모델 적용-> 예측(사망/생존)->예측결과를 submission.csv에 저장
#-> 캐글 제출 -> 점수 및 등수 확인

setwd('C:/rwork')
train <- read.csv('train.csv')
test <- read.csv('test.csv')
#남자는0 여자는1로 제출

class(train)
train$Survived <- ifelse(train$Sex=='male',0,1)
head(train)


my.test <- read.csv('test.csv')
my.test$Survived <- ifelse(my.test$Sex=='male',0,1)
my.test <- my.test[c('PassengerId','Survived')]
head(my.test)
#정확도

write.csv(my.test,'submission.csv',row.names=F)


#----생존자 예측------

str(train)
str(test)
summary(train)
head(train)
head(test)

#EDA--------
library(ggplot2)
titanic_fare <- ggplot(train, aes(x=Fare,y=..density..),main='',
                xlab='Fare Distribution')+
                geom_density(fill='blue',alpha=0.2)
titanic_age <- ggplot(train, aes(x=Age,y=..density..),main='',
                xlab='Fair Distribution')+
                geom_density(fill='blue',alpha=0.2)
grid.arrange(titanic_fare, titanic_age, nrow=1,ncol=2)

#mosaic plot #성별과 티켓등급
par(mfrow=c(1,2))
mosaicplot(table(ifelse(train$Survived==1,'Survived','Dead'),train$Sex),
           main='',
           cex=1.2,
           color=T)
mosaicplot(table(ifelse(train$Survived==1,'Survived','Dead'),train$Pclass),
           main='',
           cex=1.2,
           color=T)

#box plot / jitter plot #나이
par(mfrow=c(1,2))
boxplot(Age~Survived, train, xlab='Survival',ylab='Age',cex=1.2)
plot(Age~jitter(Survived), train, xlab='Survival',cex=1.2)

#scatter plot
ggplot(train, aes(Age, log(Fare), 
                  color=factor(Survived), 
                  shape=factor(Sex)))+
  geom_point()+
  geom_jitter()
  
#데이터 전처리----------------

#isTrain열 만들어서 표시
train$isTrain <- TRUE
test$isTrain <- FALSE
#test에 생존여부 열 만들기
test$Survived <- NA
head(test)
#train+test 셋
ttnc <- rbind(train,test)
table(ttnc$isTrain)

#train과 test를 합쳐서 결측치 값 임의 설정
#Embarked NA값 S로 임의설정
table(ttnc$Embarked)
ttnc[ttnc$Embarked=='','Embarked'] <- 'S'

#Age 중간값으로 임의 설정
table(is.na(ttnc$Age))
age.median <- median(ttnc$Age,na.rm=T)
ttnc[is.na(ttnc$Age),'Age'] <- age.median

#Fare 임의 설정
table(is.na(ttnc$Fare))
fare.median <- median(ttnc$Fare,na.rm=T)
ttnc[is.na(ttnc$Fare),'Fare'] <- fare.median #나중에 정교화 할것



#범주형으로 변환
ttnc$Pclass <- as.factor(ttnc$Pclass)
ttnc$Sex <- as.factor(ttnc$Sex)
ttnc$Embarked <- as.factor(ttnc$Embarked)

#데이터 셋 다시 분리
train <- ttnc[ttnc$isTrain==TRUE,]
test <- ttnc[ttnc$isTrain==FALSE,]

#생존여부 범주형
train$Survived <- as.factor(train$Survived)
str(train)

#랜덤포레스트 예측 모델=============
install.packages('randomForest')
library(randomForest)

#modeling
survived.equation <- 'Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked'
survived.formula <- as.formula(survived.equation)
class(survived.formula)

titanic.model <- randomForest(survived.formula,train,ntree=500,mtry=3,nodesize=0.01*nrow(test))

#test셋으로 예측
Survived <- predict(titanic.model,newdata=test)
PassengerId <- test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- Survived

output #결과로 제출하면됨.
write.csv(output,'submissions.csv',row.names=F)

-----# 결측값---------

train <- read.csv('train.csv')
test <- read.csv('test.csv')

head(train)
summary(train)
summary(test)
library(dplyr)
summarise(train)

proportions(table(train$Pclass,train$Survived),2)

train$isTrain <- TRUE
test$isTrain <- FALSE
test$Survived <- NA
head(test)
#train+test 셋
ttnc <- rbind(train,test)
table(ttnc$isTrain)
#Embarked 결측값 임의 지정
table(ttnc$Embarked)
ttnc[ttnc$Embarked=='','Embarked'] <- 'S'

#age를 title별 평균값으로 임의설정
lst <- unlist(strsplit(ttnc$Name,split=' '))
lst
title <- grep('.{2,}\\.$',lst,value=T)
table(title)

title <- gsub('Mlle|Ms|Lady|Dona','Miss',title)
title <- gsub('Mme','Mrs',title)
title <- gsub('Capt|Col|Major|Dr|Rev|Don|Sir|Countess|Jonkheer','Officer',title)
title <- gsub('Master','Others',title)
#title[title %in% c('Mlle.','Ms.','Lady.','Dona.')] <- 'Miss.'

ttnc$name2 <- title
factor(ttnc$name2)
lev <- levels(factor(ttnc$name2))
lev
table(title)

for(i in 1:length(lev)){
  mean.age <- mean(ttnc$Age[ttnc$name2==lev[i]],na.rm=T)
  ttnc$Age[ttnc$name2==lev[i]&is.na(ttnc$Age)] <- mean.age
}

#Fare 결측값 평균값으로 대체
colSums(is.na(ttnc)) #각 변수별 결측값 개수 확인
ttnc$Fare[is.na(ttnc$Fare)] <- mean(ttnc$Fare,na.rm=T) #옳지않음

#범주형으로 변환
ttnc$Pclass <- as.factor(ttnc$Pclass)
ttnc$Sex <- as.factor(ttnc$Sex)
ttnc$Embarked <- as.factor(ttnc$Embarked)

#데이터 셋 다시 분리
train <- ttnc[ttnc$isTrain==TRUE,]
test <- ttnc[ttnc$isTrain==FALSE,]

#생존여부 범주형
train$Survived <- as.factor(train$Survived)
str(train)





#랜덤포레스트 예측 모델=============
install.packages('randomForest')
library(randomForest)

#modeling
survived.equation <- 'Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked'
survived.formula <- as.formula(survived.equation)
class(survived.formula)

titanic.model <- randomForest(survived.formula,train,ntree=500,mtry=3,nodesize=0.01*nrow(test))

#test셋으로 예측
Survived <- predict(titanic.model,newdata=test)
PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df,'submission.csv',row.names=F)



library(dplyr)
library(MASS)
select <- dplyr::select
str(Cars93)
a1 <- group_by(Cars93,Origin,Type,Cylinders)
a2 <- select(a1,Price,MPG.highway)
a3 <- summarise(a2,
                Price_m=mean(Price,na.rm=T),
                MPG.highway_m=mean(MPG.highway,na.rm=T))
a4 <- filter(a3, Price_m>10|MPG.highway_m>25)
a4

Cars93 %>%
  group_by(Origin, Type, Cylinders) %>%  # group_by()
  select(Price, MPG.highway) %>%  # select() columns
  summarise(
    Price_m = mean(Price, na.rm = TRUE),
    MPG.highway_m = mean(MPG.highway, na.rm = TRUE)  # summarise()
  ) %>%
  filter(Price_m > 10 | MPG.highway_m > 25)  # filter() condition


#------타이타닉 조건으로 생존 구하기------------------------
train <- read.csv('train.csv',na.string='')
test <- read.csv('test.csv',na.string='')
summary(train)
summary(test)
str(train)
str(test)
head(train)


library(Amelia)
missmap(train,col=c('red','grey'))
missmap(test,col=c('red','grey'))

library(ggplot2)
library(GGally)
ggcorr(train,label=T) #Pclass 낮을수록(등급이 높을수록),
                      #Fare가 높을수록
                      #연령이 젊을수록
                      #Parch가 높을수록 많이 살아 남음

colSums(is.na(train)) #Age 177건, Embarked 2건
colSums(is.na(test)) #Age 86건, Fare 1건
                    #Cabin은 고려 안함

---#데이터 분석-----

#Fare와 생존 상관관계
plot(train[,c('Survived','Fare')]) #생존자가 약간 높지만 차이 거의 없음


#Embarked와 생존(결측치 무시)
prop.table(table(train$Embarked,train$Survived),1)
#c 생존율 : 55%, Q 생존율: 39%, S 생존율: 34%
prop.table(table(train$Embarked,train$Survived),2)
#생존자 비율 C:Q:S=27:9:63


#성별과 생존 상관관계
xtabs(~Sex+Survived,train)
prop.table(xtabs(~Sex+Survived,train),1) #여성 생존율 74%, 남성생존율 19%
prop.table(xtabs(~Sex+Survived,train),2) #생존자 비율 여:남 0.68:0.32, #사망자 비율 여:남 0.15:0.85

#Pclass와 생존
prop.table(table(train$Pclass,train$Survived),1)
#1등급 생존율:63%, 2등급 생존율:47%, 3등급 생존율:24%
prop.table(table(train$Pclass,train$Survived),2)
#생존자 비율 1:2:3=40:25:35


#----결측값 채우기-------

#--Embarked 결측치--
library(dplyr)
train %>% 
  filter(Embarked=='') #티켓번호가 113572인 두 여성

na.embarked <- train %>% 
  filter(Ticket>113500,Ticket<114000) %>% 
  group_by(Embarked) %>% 
  summarise(cnt=n())
na.embarked <- na.embarked %>% 
  filter(cnt==max(cnt)) %>% 
  select(Embarked)
na.embarked
            #가까운 티켓 번호들의 탑승지역 통계: S가 많음

train[train$Embarked=='',]$Embarked <- na.embarked


#-----나이 결측값----

#이름에서 타이틀만 분리
lst <- unlist(strsplit(train$Name,split=' '))
lst2 <- unlist(strsplit(test$Name,split=' '))
title <- grep('.{2,}\\.$',lst,value=T)
title2 <- grep('.{2,}\\.$',lst2,value=T)
table(title)
table(title2)
#타이틀 단순화
title <- gsub('Mlle|Ms|Lady|Dona','Miss',title)
title <- gsub('Mme','Mrs',title)
title <- gsub('Capt|Col|Major|Dr|Rev|Don|Sir|Countess|Jonkheer','Officer',title)
title <- gsub('Master','Others',title)
train$title <- title
title2 <- gsub('Mlle|Ms|Lady|Dona','Miss',title2)
title2 <- gsub('Mme','Mrs',title2)
title2 <- gsub('Capt|Col|Major|Dr|Rev|Don|Sir|Countess|Jonkheer','Officer',title2)
title2 <- gsub('Master','Others',title2)
test$title <- title2
library(gmodels)
CrossTable(train$title) #개수 확인
CrossTable(test$title)

#타이틀별 나이 평균으로 결측값 채우기
train_age <- aggregate(data=train,Age~title,mean,na.rm=T)
test_age <- aggregate(data=test,Age~title,mean,na.rm=T)
train_ave <- ave(train$Age,train$title,FUN=function(x) mean(x,na.rm=T))
test_ave <- ave(test$Age,test$title,FUN=function(x) mean(x,na.rm=T))
train$Age <- ifelse(is.na(train$Age),train_ave,train$Age)
test$Age <- ifelse(is.na(test$Age),test_ave,test$Age)


#나이대별 분류
train <- train %>% 
  mutate(Age.ctg=cut(Age,breaks=c(0,10,20,30,40,50,100),
                     labels=c('under10','10s','20s','30s','40s','over50'),
                     right=F,
                     include.lowest=T))
test <- test %>% 
  mutate(Age.ctg=cut(Age,breaks=c(0,10,20,30,40,50,100),
                     labels=c('under10','10s','20s','30s','40s','over50'),
                     right=F,
                     include.lowest=T))

#나이대별 생존율
prop.table(table(train$Age.ctg,train$Survived),2)
#생존자비율 : 12%:12%:29%:30%:10%:8%
with(train,tapply(Survived,Age.ctg,mean))
#생존확률 : 10대미만: 60%, 10대: 40%, 20대:38% 30대:33%, 40대:37% 50이상: 36%


#-------Fare 결측값--------
train[which(is.na(train$Fare)),] #없음
test[which(is.na(test$Fare)),]

#등급,성별에 따른 운임
with(train,tapply(Fare,list(Pclass,Sex),mean))
with(test,tapply(Fare,list(Pclass,Sex),mean))

#등급별, 나이별 남여 운임료 비교
train %>% 
  group_by(Pclass,Sex,Age.ctg) %>% 
  summarise(mean.fare=mean(Fare))
test %>% 
  group_by(Pclass,Sex,Age.ctg) %>% 
  summarise(mean.fare=mean(Fare,na.rm=T))

#티켓번호
length(unique(train$Ticket)) #681
summary(train$Ticket)
train %>% 
  filter(Ticket=='CA. 2343') %>% 
  select(Pclass, Name, Age, Ticket, SibSp, Parch, Survived)
# 중복되는 티켓 번호는 가족

#가족 열 = SibSp+ParCh
train <- train %>% 
  mutate(Family=SibSp+Parch)

with(train,tapply(Survived,Family,mean)) 
#Family가 1,2,3일때 생존율 50%이상



#이제 조건식으로 예측... 으악악




#-----타이타닉 down to bottom-----
# Data input, assesment : 데이터 불러들이기, 확인하는 과정 
library(readr)           # Data input with readr::read_csv()
library(descr)           # descr::CrossTable() - 범주별 빈도수, 비율 수치로 확인

# Visualization
library(VIM)             # Missing values assesment used by VIM::aggr()
library(ggplot2)         # Used in almost visualization 
library(RColorBrewer)    # plot의 color 설정 
library(scales)          # plot setting - x, y 축 설정

# Feature engineering, Data Pre-processing
# library(tidyverse)     # dplyr, ggplot2, purrr, etc... 
library(dplyr)           # Feature Engineering & Data Pre-processing 
library(purrr)           # Check missing values 
library(tidyr)           # tidyr::gather() 

# Model generation  
library(randomForest)    # For Random Forest Modeling

# Model validation : 원래는 하는게 맞지만 이번 과정에서는 생략
# library(caret)           # caret::confusionMatrix() 
# library(ROCR)            # Plotting ROC Curve
