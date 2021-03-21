train <- read.csv('train.csv')
test <- read.csv('test.csv')

#Name, Ticket, Cabin 컬럼 제거
train <- train[, -c(4,9,11)]
test <- test[, -c(3,8,10)]

#성별 0,1 변환
#switch()
x <- c(5,6,7,8)
switch(1,mean(x),sum(x),var(x)) #mean(x)
switch(2,mean(x),sum(x),var(x)) #sum(x)
switch(3,mean(x),sum(x),var(x)) #var(X)
train$Sex <- sapply(train$Sex,switch,'male'=0,'female'=1)
test$Sex <- sapply(train$Sex,switch,'male'=0,'female'=1)

#승선 항구 C=0, Q=2, S=2 변환
train$Embarked <- sapply(train$Embarked,switch,'C'=0,'Q'=1,'S'=2)
test$Embarked <- sapply(train$Embarked,switch,'C'=0,'Q'=1,'S'=2)

#나이와 운임 열 결측값 제거
train_age <- na.omit(train$Age)
train_age_avg <- mean(train_age) #na 제외 나머지 나이 평균
train$Age[is.na(train$Age)] <- train_age_avg
test_age <- na.omit(test$Age)
test_age_avg <- mean(test_age) 
test$Age[is.na(test$Age)] <- test_age_avg

train_fare <- na.omit(train$Fare)
train_fare_avg <- median(train_fare) #na 제외 Fare 중앙값
train$Fare[is.na(train$Fare)] <- train_fare_avg
test_fare <- na.omit(test$Fare)
test_fare_avg <- median(test_fare) 
test$Fare[is.na(test$Fare)] <- test_fare_avg

#나이와 생존여부 관계 그래프(시각화)
#나이가 18세 이상이면 0, 아니면 1
train$Age <- ifelse(train$Age<18,1,0)
test$Age <- ifelse(test$Age<18,1,0)

#정규화(운임, 등실)
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}
train$Pclass <- normalize(train$Pclass)
test$Pclass <- normalize(test$Pclass)

test_length <- length(test$Fare)
fare <- normalize(c(train$Fare,test$Fare))
fare
train$Fare <- fare[1:(length(fare)-test_length)]
test$Fare <- fare[(length(fare)-test_length+1):length(fare)]

#시각화
install.packages('ggvis')
library(ggvis)
library(dplyr)

iris %>% 
  ggvis(~Petal.Length,~Petal.Width,fill=~factor(Species)) %>% 
  layer_points()

#상관관계 나타남. 색상으로 종 분류

#정규화
iris_n <- as.data.frame(lapply(iris[1:4],normalize))
summary(iris_n)

#train,test 67:33으로 나누기
set.seed(1234)

rs <- sample(2,nrow(iris),replace=T,prob=c(0.67,0.33))
#2까지 숫자를 nrow(iris)개 만큼 복원추출, 1과2의 비율은 67:33

train <- iris[rs==1,1:4] #iris 데이터의 67% 
test <- iris[rs==2,1:4] #33%=test데이터

train.label <- iris[rs==1,5]
test.label <- iris[rs==2,5]

library(class)
pred <- knn(train,test,cl=train.label,k=10)
sum(pred==test.label)/length(test.label)*100
library(gmodels)
CrossTable(pred,test.label)

faithful

plot(faithful,
     main='main title',
     sub='sub title',
     xlab='eruption time(m)',
     ylab='waiting time till next eruption')
#x,y축 숫자 방향
plot(faithful,
     las=3) #0기본값, 1:y축 숫자 돌아감, 2:x축 숫자 돌아감 3:둘다 돌아감
#박스 모양(그래프 외곽선 모양)
plot(faithful, bty='o')
plot(faithful, bty='n')
plot(faithful, bty='l')
plot(faithful, bty=']')
plot(faithful, pch=24, col='green2',bg='blue')

plot(LakeHuron)
str(LakeHuron)

plot(LakeHuron, lty='solid')
plot(LakeHuron, lty='dashed')
plot(LakeHuron, lty='dotted')
plot(LakeHuron, lty='twodash')

pressure
plot(pressure, type='p') #점으로 표시 (기본값)
plot(pressure, type='l') #선으로
plot(pressure, type='b') #both
plot(pressure, type='o') #겹쳐서
plot(pressure, type='h') #히스토그램
plot(pressure, type='s') #계단식
plot(pressure, type='n') #nothing
#사용자 정의 형식 그래프
x <- 1:10
y1 <- exp(1:10)
y2 <- exp(10:1)
plot(x,y1,type='n',ylab='y')
lines(x,y1,type='o',col='red')
lines(x,y2,type='o',col='blue')

plot(faithful,type='n') #틀 만들고
grid() #격자선 추가
points(faithful, pch=18, col='blue') #포인트찍기
dev.off()
plot(faithful, pch=18, col='blue') #처음부터 찍기

colors() #색 이름들
palette('default') #기본색상 파레트
palette()
pie(rep(1,12), col=1:12) #기본색상 파레트
palette(rainbow(6)) #변경 파레트
palette()
pie(rep(1,12),col=1:12)
pie(rep(1,12),col=gray(level=seq(0,1,length=12))) #그라데이션
pie(rep(1,12),col=rainbow(12,alpha=seq(0,1,length=12))) #그라데이션
#alpha: 투명도
pie(rep(1,12),col=heat.colors(12))
pie(rep(1,12),col=terrain.colors(12))
pie(rep(1,12),col=cm.colors(12))

install.packages('mlbench')
library(mlbench)
data("Ozone")
head(Ozone)
plot(Ozone$V8,Ozone$V9,
     xlab='Sandburg',
     ylab='El Monte', 
     main='Ozone',  #제목
     pch=19,        #점 모양
     cex=0.5,       #점 크기
     col='red',
     xlim=c(0,150), #x축범위
     ylim=c(0,150)) #y축범위

head(cars)
plot(cars,type='o',cex=0.5)
plot(cars,type='s',cex=0.5)


#갑자기 스피드별 거리평균 구하라함
mean.dist <- cars %>% 
  group_by(speed,) %>% 
  summarise(mean=mean(dist)) 
t(mean.dist)

dist_mean <- with(cars,tapply(dist,speed,mean))

tapply(1:10,rep(1,10),sum)

#1~10까지 숫자를 짝수, 홀수 별 합계
tapply(1:10,1:10%%2==1,sum) 

#iris 종별 sepal.length평균
with(iris,tapply(Sepal.Length,Species,mean))

plot(dist_mean, type='o',
     xlab='speed',
     ylab='dist',
     cex=0.7)

#그래프 칸 개수
opar <- par(mfrow=c(1,2)) #2칸으로 나누기
par(mfcol=c(2,1)) #세로방향 두칸으로 나누기
plot(Ozone$V8,Ozone$V9)
plot(Ozone$V8,Ozone$V9)

par(opar) #설정 불러오기
dev.off() #설정 초기화

