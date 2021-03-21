df <- read.csv('train.csv',na.string='')
View(df)

#기술 통계량 함수
install.packages('psych')
library(psych)

#결측맵 함수가 포함된 패키지
install.packages('Amelia')
library(Amelia)
missmap(df,col=c('red','grey'))
#na가 너무 많으면 자료를 삭제하기보다 대체하거나 열을 제거하는 방향 고려

#필요한 열만 추출
library(dplyr)
df <- select(df,Survived,Pclass,Age,Sex,SibSp,Parch)
missmap(df,col=c('red','grey'))

#na제거(포함된 행 전체 삭제)
na.omit(df)

#범주지정
df$Survived <- factor(df$Survived)
df$Survived
#순서형 범주 지정
df$Pclass <- factor(df$Pclass,levels=c(3,2,1),ordered=T)
df$Pclass

str(df)

#시각화
install.packages('ggplot2')
library(ggplot2)

#ggplot2 확장
install.packages('GGally')
library(GGally)

#상관계수 확인 #숫자형 데이터만 적용가능(범주형도 제외)
ggcorr(df, nbreaks=6,        #수치 구간을 6개로 나눔
       label=T, 
       label_size=3, color='grey50') 
#운임이 높을수록 생존율 높음, Pclass가 작을수록(높은등급) 생존율 높음


#---------------------------

#match함수로 데이터셋 결합
#match() : 첫번째 인수가 두번째 인수의 어느 위치에 있는지 인덱스
a <- c(1,2,3,4,5)
b <- c(2,4,5,7,9)
match(a,b,nomatch=0) #없는건 0으로

#행이름을 열 이름으로 변경
car <- mtcars
car$name <- row.names(car)
head(car)
row.names(car) <- NULL
head(car)

#145마력 넘는 자동차 모델만 df로
highhp.car <- car[car$hp>145,]
highhp.car

#wt가 3.200 미만인 모델만 df로
lightwt.car <- car[car$wt<3.2,]
lightwt.car

#무게가 가벼우면서 힘이 센 자동차 모델
index <- match(highhp.car$name,lightwt.car$name)
lightwt.car[na.omit(index),]

#match함수 대신 %in% 연산자 가능
vec <- 1:5
c(3,5,7)%in%vec
!is.na(match(c(3,5,7),vec))

index2 <- highhp.car$name%in% lightwt.car$name
highhp.car[index2,]

#-------------------
mtcars
mtcars$mpg
mtcars[[1]] #1열의 내용이 벡터로
mtcars[['mpg']] #하나의 원소만 가능
mtcars[1] #1열의 내용이 데이터프레임으로
mtcars['mpg'] #여러 원소 가능
mtcars[c(1,4)]
mtcars[-c(1,2,5:7)]
mtcars[1] <- NULL

iris[1:4]
iris[c(1,2,3,4)]
iris[c(1:4)]
iris[,-5]
iris[,'Sepal.Length',drop=F] #데이터프레임 유지
iris[iris$Sepal.Length>7,]

#모델링과정

#1)데이터를 훈련용(70%)/테스트용(30%)으로 분할 
#(샘플함수로 무작위 표본추출)
#sample(x,size,replace=F) 
#x=벡터형식 표본, size=표본의 개수 ,replace=복원추출 여부

#2) 훈련용 데이터만으로 모델(예측/분류)을 생성
#과적합(오버피팅) : 훈련데이터 평가결과>>테스트 데이터 평가결과
#원인 :  데이터부족 ->보충 필요, 변수가 너무 많음 -> 차원 축소

#3)테스트

#------------동일한 난수 추출---------

#seed값을 동일하게 주면 매번 난수가 동일하게 추출
set.seed(20210303)
sample(1:10)
#1~10 데이터로 할때
#전처리 -> 랜덤 : 124579번 추출해서 훈련데이터 ->모델-> 평가(3,8,10)
#정확도를 같은 데이터로 계속 알아봐야함
set.seed(1)
sample(1:10,5,replace=T)
#데이터프레임 열로부터 표본 추출
sample(iris,3)
#데이터프레임 행으로부터 표본 추출
set.seed(1)
index <- sample(1:nrow(iris),5) #행 번호 추출
iris[index,]

#------------중복제거---------------
#행 정보가 완전히 같은 중복값이 있을 수 있어서 제거해줘야함
#duplicated(): 중복 여부를 논리값으로 출력
#which로 위치(인덱스) 파악
duplicated(c(1,2,3,1,1,3,5))

id <- c('a001','a002','a003')
name <- c('mouse','keyboard','usb')
price <- c(3000,9000,5000)
product <- data.frame(id,name,price)
product <- rbind(product,c('a001','mouse','3000'))
duplicated(product)
which(duplicated(product)) #중복자료 위치
product[!duplicated(product),] #중복 아닌것만 출력
product[-which(duplicated(product)),]
unique(product) #중복제거되지만 중복자료 파악 안됨

#--------na 제거-----------
#complete.cases(): 결측값 여부. 결측값 있는 경우 false
#na.omit() : 결측값 행 전체 제거
airquality
airquality[complete.cases(airquality),]
na.omit(airquality)

#-------구간화----------------
#cut(x,구간) : 데이터를 구간에 따라 범주화
#():개구간 (3,10)->3초과 10미만
#[]:폐구간 (3,10]->3초과 10이하
iris$Sepal.Width
min(iris$Sepal.Width)
max(iris$Sepal.Width)
iris.cut <- cut(iris$Sepal.Width,c(1,2,3,4,5),right=FALSE) #right=F-->왼쪽 폐구간[)
table(iris.cut)
#구간에 이름 붙이기
iris.cut2 <- cut(iris$Sepal.Width,
    breaks=c(0,1,2,3,4,5),
    labels=c('smallest','small','medium','big','biggest'))
table(iris.cut2)
summary(iris.cut2)

#구간 자동으로 분리
cut(iris$Sepal.Width,5) #최대 최소 사이를 5구간으로 범주화

cut(iris$Sepal.Width,5, include.lowest=T) #최소값 포함
iris.cut3 <- cut(iris$Sepal.Width,
    breaks=c(0,1,2,3,4,5),
    labels=c('smallest','small','medium','big','biggest'),
    include.lowest=T)
iris.cut3


#----------정렬---------
x <- c(1,2,3,4,5)
sort(x)
sort(x,decreasing = T)
length(x)
length(x) <- 7 #NA 추가
sort(x) #NA 포함 안됨
sort(x,na.last=T) #NA가 마지막에 포함
sort(x,na.last=F) #NA가 처음에 포함

order(y) #인덱스번호 나옴
y <- c(11,22,33,66,55)
z <- c('ss','bb','ii','aa','pp')
df <- data.frame(y,z)
df[order(y),]
df[order(y,decreasing=T),]

#xtfrm() : 동일하지 않은 정렬 방법 적용할때
df[order(-xtfrm(df$y),df$z),]

iris[order(-xtfrm(iris$Species),iris$Sepal.Length),]

#==========연습문제============
#1.
mtcars$weight <- ifelse(mtcars$wt>median(mtcars$wt),'heavy','light')
mtcars
table(mtcars)
summary(mtcars)
proportions(table(mtcars))

#2.
mtcars[order(colnames(mtcars))]

col.order <- with(mtcars,order(cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb,weight))
mtcars[col.order,]

#3.
index <- sample(1:round(nrow(iris)*0.7))
iris[index,]

#4.
ggplot2::midwest
mdw <- as.data.frame(midwest)
library(Amelia)
missmap(mdw,col=c('red','grey'))
str(mdw)
class(mdw)
mode(mdw)
head(mdw)
tail(mdw)
View(mdw)
mdw <- rename(mdw,total=poptotal,asian=popasian)
hist(mdw$percasian)
mdw$percasian.ls <- ifelse(mdw$percasian>mean(mdw$percasian),'large','small')
table(mdw$percasian.ls)
max(mdw$percasian)

mdw$ls <- cut(mdw$percasian,c(0,mean(mdw$percasian),max(mdw$percasian)+1),
    label=c('small','large'),
    include.lowest=T)
mdw$ls
table(mdw$ls)

#5.
ttnc <- read.csv('train.csv')
head(ttnc)
sum(ttnc$Survived)
sum(!ttnc$Survived)
table(ttnc$Pclass,ttnc$Survived)
proportions(table(ttnc$Pclass,ttnc$Survived),2)

proportions(ttnc$Survived)
#Pclass별 생존자 비율
sum(ifelse(ttnc$Pclass==1,proportions(ttnc$Survived),0))
sum(ifelse(ttnc$Pclass==2,proportions(ttnc$Survived),0))
sum(ifelse(ttnc$Pclass==3,proportions(ttnc$Survived),0))

table(ttnc$Embarked)

ttnc$Name
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

head(ttnc)
ttnc$Age
age.cut <- cut(ttnc$Age,
    breaks=c(0,10,20,30,40,50,max(ttnc$Age)+1),
    right=F)
table(age.cut)
summary(age.cut)

substr(unlist(strsplit(ttnc$Cabin,split=' ')),1,1)

summary(ttnc$Fare)
sd(ttnc$Fare)

ttnc$family <- ttnc$SibSp+ttnc$Parch
