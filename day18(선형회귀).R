


#임의의 피보험자 정보가 입력되었을때 의료보험비 예측하기

read.csv('insurance.csv')




mydata <- read.csv('examscore.csv')
x <- 6:10
sum(x)/length(x)
mean(x)

mean(mydata$midterm)
hist(mydata$midterm,
     xlab='중간고사 성적',
     ylab='빈도',
     main='중간고사 성적분포포')
abline(v=mean(mydata$midterm),col='red')
abline(v=median(mydata$midterm),col='blue')

mydata$midterm[22] <- 100
hist(mydata$midterm,
     xlab='중간고사 성적',
     ylab='빈도',
     main='중간고사 성적분포포')
abline(v=mean(mydata$midterm),col='red')
abline(v=median(mydata$midterm),col='blue')

quantile(mydata$midterm)

#표본분산
sum((x-mean(x))^2)/(length(x)-1)
var(x)
#표준편차
sqrt(sum((x-mean(x))^2)/(length(x)-1))
sd(x)

#최빈수 :....
x <- c(1,3,9,7,1,2,2,5,3,3,3)
table(x)

#상관계수 함수
mycorr <- cor(mydata$midterm,mydata$final)


#상관계수 직접 구해보기
n <- length(mydata$midterm)
xBar <- mean(mydata$midterm)
yBar <- mean(mydata$final)
sx <- sd(mydata$midterm)
sy <- sd(mydata$final)
zx <- (mydata$midterm-xBar)/sx
zy <- (mydata$final-yBar)/sy
rxy <- sum(zx*zy)/(n-1)

rxy=mycorr

#점수 점으로 시각화
plot(mydata$midterm, mydata$final,
     main='시험점수',
     xlab='중간',
     ylab='기말')
title(sub=paste('상관계수 : ',round(mycorr,4)), adj=1, col.sub='red')
abline(v=xBar,h=yBar) #평균으로 4분면으로 나누기

#표준화된 점수로 점찍기 (평균이 0)
plot(zx, zy,
     main='시험점수',
     xlab='중간',
     ylab='기말',
     col='red')
title(sub=paste('상관계수 : ',round(mycorr,4)), adj=1, col.sub='red')
abline(v=0,h=0) #평균으로 4분면으로 나누기

#사분면에 따라 색 다르게
zx*zy #양수는 1,3사분면, 음수는 2,4사분면에 위치
sign(zx*zy) #양수는 1, 음수는 -1
as.factor(sign(zx*zy))
color <- c('blue','red')[as.factor(sign(zx*zy))] #-1이면 파랑, 1이면 빨강
plot(zx, zy,
     main='시험점수',
     xlab='중간',
     ylab='기말',
     col=color)
abline(v=0,h=0) #평균으로 4분면으로 나누기

#중심에서 멀어질수록 원의 크기 크게
abs(zx*zy)
plot(zx, zy,
     main='시험점수',
     xlab='중간',
     ylab='기말',
     col=color,
     cex=abs(zx*zy))


#회귀분석
#회귀선 그리기
#주어진 y데이터를 x데이터로 예측했을 경우, 예측값과 실제값 사이에서
#차이의 제곱들의 합이 가장 작은 직선

#잔차(residual): 예측값과 실제값 사이의 차이
#잔차제곱의합 : residual sum of square (Rss)

#중간고사(x) ->모델 -> 기말고사 점수 예측 (yhat)
#잔차= y - yhat
#잔차 제곱의 합
#중 최소값인 선 찾기

rss(mydata[,3:4],c(5,1))
#y=x+5를 회귀직선으로, mydata[,3:4]에 대한 rss구하기


#잔차 제곱의 합 구하는 함수
rss <- function(data,lineInfo){
  x <- data[,1] #mid
  y <- data[,2] #final
  intercept <- lineInfo[1] #y절편
  slope <- lineInfo[2] #기울기
  yhat <- intercept+slope*x
  result <- sum((y-yhat)^2)
  result
}

rss(mydata[,3:4],c(-15,1.5))

#회귀모델 만들기 1 : optim()
#optim() : y절편과 기울기를 변경하면서 최적의 값 찾아냄
#par=y절편과 기울기, value=rss
result <- optim(par=c(1,1), fn=rss, data=mydata[,3:4])

#for문 돌듯이 반복해서 돌아서 결과 나옴

# $par
# [1] 13.8833436  0.8967623
# 
# $value
# [1] 5712.789

# 따라서, yhat=13.88x+0.896


plot(mydata$midterm, mydata$final,
     main='시험점수',
     xlab='중간',
     ylab='기말',
     col=color)
abline(result$par[1],result$par[2]) #회귀선

#회귀모델 만들기 2 : lm함수

#lm(final~midterm, mydata)

# Coefficients:
#   (Intercept)      midterm  
# 13.8866       0.8967 

# ex)
# s1 - mid:57, fin:69
# s2 - mid:57, fin:64
# s3 - mid:57, fin:?
# 
# yhat=기울기 * 중간(x) + 절편 + 노이즈
#기말고사 점수들에 잡음이 섞여있다고 가정하여
#대략적인 y 예측값을 구하는것

res <- lm(final~midterm, mydata)
attributes(res)

#직선 정보
res$coefficients
coef(res)

plot(mydata$midterm, mydata$final,
     main='시험점수',
     xlab='중간',
     ylab='기말',
     col=color)
abline(res$coefficients)

#lm에 대한 정보
summary(res)

#입력한 코드 확인
res$call

#중간고사 점수를 회귀모델에 입력한 예측결과(yhat)
res$fitted.values

#잔차(예측값과 실제값 차이 -> y-yhat)
res$residuals
#=> mydata$final-res$fitted.values

summary(res$residuals)

par(mfrow=c(2,2))
plot(res)


#회귀모델 만들기 3 : 공식으로

# 기울기 = 상관계수 * 표준편차(y)/표준편차(x)
x <- mydata$midterm
y <- mydata$final
a <- cor(x,y)*sd(y)/sd(x)
#0.896

# 절편 = 평균(y) - 기울기*평균(x)
b <- mean(y)-a*mean(x)
#13.88




#성별도 고려
dev.off()
par(mfrow=c(1,1))
plot(mydata$midterm, mydata$final,
     col=c('red','blue')[as.factor(mydata$gender)],
     pch=c(16,17)[as.factor(mydata$gender)],
     main='시험점수',
     xlab='중간',
     ylab='기말')
legend(15,85,
       legend=c('여자','남자'),
       col=c('red','blue'),
       pch=c(16,17))

#데이터 남녀로 나누기
dataFemale <- mydata[mydata$gender=='F',]
dataMale <- mydata[mydata$gender=='M',]


modelf <- lm(final~midterm,dataFemale)
modelm <- lm(final~midterm,dataMale)

#남여 회귀선
abline(modelf$coefficient,col='red')
abline(modelm$coefficient,col='blue')



#기말고사 점수 예측하기

#예측함수 : predict(모델, 데이터프레임 형태 피예측값)
#중간고사 50점인 여학생의 기말고사 점수 예측

predict(modelf, data.frame(midterm=50))

#다른방법
param <- as.numeric(modelf$coefficients)
param
param[1]+50*param[2]


#변수 여러개에 대한 회귀분석(다중회귀분석)
model3 <- lm(final~midterm+gender,mydata)
model3

#yhat=intercep t+ midterm*중간점수 + genderM*성별값 + 노이즈
#성별을 자동으로 더미화해서 수치로 바꿈
#c(0,1)[as.factor(mydata$gender)] 과정으로
#F->0, M->1로 수치변환

predict(model3, data.frame(midterm=40, gender='F'))


#1. cars 임의의 속도가 입력되었을때 제동거리 출력하기
1)optim()
#잔차제곱의합
rss <- function(data,line){
  x=data[,1] #speed
  y=data[,2] #dist
  intercept=line[1] 
  slope=line[2]
  yhat=slope*x+intercept
  result=sum((y-yhat)^2)
  result
}
#잔차제곱의합이 최소값인 최적의 기울기와 y절편
optimCoef <- optim(par=c(1,1), fn=rss, data=cars)
corline <- optimCoef$par
#[1] -17.582847   3.932929


2)lm()
cars.lm <- lm(dist~speed,cars)
lineinfo <- coef(cars.lm)
# (Intercept)       speed 
# -17.579095    3.932409 

3)대입
#기울기
a <- with(cars, cor(speed,dist)*sd(dist)/sd(speed))
#3.932409
#절편
b <- mean(cars$dist)-a*mean(cars$speed)
b
#-17.57909


4)시각화
plot(cars, main='속력에 따른 제동거리')
abline(lineinfo,col='red')


5)회귀 모델
dist_pred <- function(x){
  predict(cars.lm, data.frame(speed=x))
}


#2. marketing (train:test, 7:3)
#유투브, 페이스북, 신문 데이터가 입력되었을때 예상되는 sales

install.packages('datarium') #marketing 검색
library(datarium)
data("marketing")
str(marketing)
head(marketing)

set.seed(2021)
idx <- sample(1:nrow(marketing),nrow(marketing)*0.7)
marketTrain <- marketing[idx,]
marketTest <- marketing[-idx,]

modelTrain <- lm(sales~youtube+facebook+newspaper,marketTrain)

summary(modelTrain)



par(mfrow=c(2,2))
plot(modelTrain)

#예측값(yhat)
marketPred <- predict(modelTrain,data.frame(marketTest[-4]))


par(mfrow=c(2,1))
plot(marketPred,type='l',col='red',
     ylab='sales',
     main='sales prediction')
plot(marketTest$sales,type='l',col='green',
     ylab='sales',
     main='sales answer')

par(mfrow=c(1,1))
plot(marketTest$sales,type='l',col='#aabccf',
     ylab='sales',border='white',
     main='sales accuracy')
lines(marketPred,col='red')
legend(x='topright',legend=c('answer','prediction'),
       col=c('#aabccf','red'),
       box.lty=0,cex=0.7,border='white',
       lty=1)





