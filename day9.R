iris.copy <- iris
class(iris.copy$Species)
as.character(iris.copy$Species)

transform(iris.copy,
          Species=as.character(Species),
          Sepal.Ratio=Sepal.Length/Sepal.Width)

#데이터에 함수를 적용하는 함수:
#apply family: apply(), lapply(), sapply()

#apply() : 적용하려는 데이터가 행렬이나 배열일 때 사용가능
#결과값이 벡터, 행렬
x <- 1:24
dim(x) <- c(4,3,2)
x
apply(x,1,max)
apply(x,1,paste,collapse=',') #char
apply(x,2,paste,collapse=',')
apply(x,3,paste,collapse=',') #3:면

apply(x,c(1,2),paste,collapse=',') #행과 열이 교차하는 부분에 함수적용
#결과값 : 행렬


#4차원 데이터 Titanic
str(Titanic)

#등급별 탑승 인원 파악
apply(Titanic,1,sum)

#승객 등급별 생존자 통계
apply(Titanic,c(1,4),sum)

#lapply(데이터,함수)
#리스트형식
#sapply() : 길이가 모두 1이면 벡터, 
#           길이가 2이상이면서 모두 같은경우 행렬
#           길이가 2이상이면서 서로 다른경우 리스트로 출력
#둘다 데이터프레임에 적용 가능
exams <- list(s1=c(80,70,60,50,100),
     s2=c(80,40,60,50),
     s3=c(30,70,60,50,100,90),
     s4=c(80,60,60,50,100))
lapply(exams,length)
lapply(exams,mean)
lapply(exams,sd)

sapply(exams,length)
sapply(exams,mean)
sapply(exams,sd)

s4=c(80,60,60,50,100)
range(s4)
sapply(exams,range)

lapply(iris,class)
sapply(iris,class)

lapply(iris,mean)
sapply(iris,mean)
sapply(iris,function(x) ifelse(is.numeric(x),mean(x),NA)) #경고안나오게

#mapply(): 함수가 벡터 연산을 지원하지 않을때 유용
mapply(rep,1:4,4:1)

#-------집단 요약------------------
#벡터를 집단별로 분할(split, unstack)
data(mtcars)
car <- mtcars
car <- within(car,
       am <- factor(am,
              levels=c(0,1),
              labels=c('Auto','Manual'))
      )
car

#split(x,factor,drop=F)
g <- split(car$mpg,car$am) #list
mean(g[['Manual']])
data.frame(split(iris$Sepal.Length,iris$Species))

#unstack(x,f,...)
#그룹별로 분할된 데이터의 길이가 동일하면 데이터프레임
#다르면 리스트로 출력
df <- data.frame(car$mpg,car$am)
g1 <- unstack(df) #list
g1
summary(g1) #리스트는 요약 안됨
class(iris$Species)
g2 <- unstack(data.frame(iris$Sepal.Length,iris$Species)) #dataframe
g2
summary(g2) #데이터프레임은 요약됨

#그룹 분할 작업 -> 그룹별 연산 작업 동시 진행 가능
#tapply(), aggregate(), by()
?tapply
#tapply(x,팩터(리스트도 가능),함수)
tapply(iris$Sepal.Length,iris$Species,mean)

tapply(iris$Sepal.Length,iris$Species,length)
car
with(car,tapply(mpg,list(cyl,am),mean))
#group명 저절로 들어감

?aggregate
#aggregate(벡터,by=집단변수를 리스트형식으로,함수)
with(car,aggregate(mpg,list(Cyl=cyl,Transmission=am),mean))
#group명이 없음

#car[c(1:6)] 변수의 평균, 실린더 개수와 변속기 유형의 조합에 따라
car[c(1:6)]
aggregate(car[c(1:6)],list(car$cyl,car$am),mean)

#꽃 종류별 측정 변수별 요약
aggregate(iris[-5],list(Species=iris$Species),mean)

#by(data,팩터나 리스트,함수)
?by
by(iris, iris$Species,summary)
by(iris, iris$Species,function(x) mean(x$Sepal.Length))

#rowsum(x,group)
rowsum(iris[-5],iris$Species)

#범주별 관측값 개수
tabulate()
gc <- tabulate(car$gear)
table(car$gear)
names(gc) <- 1:length(tabulate(car$gear))
gc
table(car$am, car$gear)

#xtabs :table함수와 동일한 기능 
#포물려사용 : 데이터 처리시 어떤 열 사용할 건지 나타낸 수식
xtabs(formula=~., data=parent.frame(), subset)
xtabs(~am+gear,car) #am과 gear 조합한 테이블

#포뮬러 이용하여
#cyl와 am 열로 mpg열의 평균 구하기
aggregate(mpg~cyl+am, car, mean)
with(car,aggregate(mpg,list(Cyl=cyl,am=am),mean)) #포뮬려 없이

#분할(split)-적용(apply)-결합(combine) 
#dplyr: SAC작업 도와주는 패키지
library(dplyr)

#filter()

airquality
air <- filter(airquality,Month==6,Temp>90) #,는 and 연산
air <- filter(airquality,Month==6&Temp>90) # &도 가능
head(air)
subset(airquality,Month==6)

#---prop.table--------

mydata<-matrix(sample(100,15), ncol=3)
colnames(mydata)<-LETTERS[seq(1,3)]
mydata
#sprintf()
rownames(mydata)<-sprintf("s-%d",seq(5))
mydata

prop.table(mydata)
sum(prop.table(mydata))
library(dplyr)
prop.table(mydata) %>% sum

prop.table(mydata, 1) #행 기준으로 비율
prop.table(mydata, 2) #열 기준으로 비율

rowSums(prop.table(mydata, 1))
prop.table(mydata, 1) %>% rowSums

colSums(prop.table(mydata, 2))
prop.table(mydata, 2) %>% colSums


#연습
#1.
ttnc <- read.csv('train.csv')
head(ttnc)
str(ttnc)
ttnc <- within(ttnc,
       Survived <- factor(Survived,
              levels=c(1,0),
              labels=c('survived','fatality')))

table(ttnc$Survived) %>% prop.table

proportions(table(ttnc$Survived))


#1-2
table(ttnc$Sex,ttnc$Survived)
xtabs(~Sex+Survived,ttnc)

#2.
proportions(table(ttnc$Pclass))
prop.table(table(ttnc$Pclass))

#proportions(table(ttnc[c('Survived','Pclass')]),1)
proportions(table(ttnc[c('Survived','Pclass')]),2)



#3.
table(ttnc$Age)
ttnc$Age
ttnc$Age
lab <- sprintf("%d대",seq(10,80,10))
ttnc$Ages <- cut(ttnc$Age,seq(0,90,10),labels=c('10세미만',lab),right=F)
head(ttnc)
#proportions(table(ttnc[c('Survived','Ages')]),1)
proportions(table(ttnc[c('Survived','Ages')]),2)

#4.
ttnc$Faretype <- cut(ttnc$Fare,5,labels=c('lowest','low','mid','high','highest'))
#proportions(table(ttnc[c('Survived','Faretype')]),1)
proportions(table(ttnc[c('Survived','Faretype')]),2)

#5.

ttnc$Name[891]
names <- toupper(gsub(' .*\\.','',ttnc$Name))
names <- gsub('[^[:alpha:]]','',names)
names <- strsplit(names,'')
names
vowel <- lapply(names,grep,pattern='[AEIOU]',value=T)
ttnc$vowel <- unlist(lapply(vowel,length))
cons <- lapply(names,grep,pattern='[^AEIOU]',value=T)
ttnc$cons <- unlist(lapply(cons,length))

ratio <- round(proportions(as.matrix(ttnc[c('vowel','cons')]),margin=1),2)
ttnc$ratio <- (ratio)
head(ttnc)
str(ttnc)
ttnc$ratio

#여자/남자 ratio

vowel.all <- tapply(ttnc$Sex,ttnc$vowel,sum)
cons.all <- tapply(ttnc$cons,ttnc$Sex,sum)
proportions(rbind(vowel.all,cons.all),1)





ratio <- with(train, 
              rbind(consonant=tapply(consonant, Sex, sum),
                    vowel=tapply(vowel,Sex, sum)))
ratio
prop.table(ratio, 2)


#자음/모음 -> consonant.vowel.ratio
for (i in 1:nrow(titanic)){
  titanic$realname[i] <- gsub('\\s{1}.{2,}[.]',replacement = '',titanic$Name[i])
  lst<-unlist(strsplit(titanic$realname[i],split=''))
  lst<-grep('[a-zA-Z]{1}',lst,value=TRUE)
  vowel <- grep('[aeiouAEIOU]',lst,value=TRUE)
  titanic$consonant.vowel.ratio[i] <- (length(lst)-length(vowel))/length(vowel)
}

titanic


#자음/전체-> consonant.ratio, 모음/전체->vowel.ratio

for (i in 1:nrow(titanic)){
  titanic$realname[i] <- gsub('\\s{1}.{2,}[.]',replacement = '',titanic$Name[i])
  lst<-unlist(strsplit(titanic$realname[i],split=''))
  lst<-grep('[a-zA-Z]{1}',lst,value=TRUE)
  vowel <- grep('[aeiouAEIOU]',lst,value=TRUE)
  titanic$vowel.ratio[i] <- length(vowel)/length(lst)
  titanic$consonant.ratio[i] <- (length(lst)-length(vowel))/length(lst)
}

str(titanic)

# - 성별에 따른 자음과 모음의 비율

#자음/모음
with(titanic,tapply(consonant.vowel.ratio,list(Sex),mean))

#자음/전체, 모음/전체
aggregate(titanic[c('consonant.ratio','vowel.ratio')],list(titanic$Sex),mean)


#6.
proportions(table(ttnc[c('Survived','Embarked')]),2)
proportions(xtabs(~Survived+Embarked,ttnc),2)
