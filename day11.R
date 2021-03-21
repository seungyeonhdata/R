#graphics패키지 :고수준, 저수준
#고수준 : 완전한 하나의 그래프 생성
#plot(), boxplot(),hist(),crve(),...
#저수준 :  독자적으로 그래프 생성 불가능
#완성된 그래프에 요소를 첨가하는 역할
#points(),lines(),abline(),text(),polygon(),...
install.packages('graphics')
library(graphics)

#시계열 분석-> 시간의 흐름에 따라 변하는 데이터 분석


#산점도 :  각 데이터 점으로 찍음 상관관계 보임.
#분출시간이 길수록 대기시간도 김
plot(faithful)

eruptions.long <- faithful %>% filter(eruptions>3)
eruptions.long
#점 색깔과 모양 바꾸기
points(eruptions.long,col='red',pch=19)#심볼
par(mar=c(5.1,4.1,4.1,2.1))

#dev.off() : 그래프 창 닫기
dev.off()

#선형 회귀 모델
#lm(종속변수~독립변수,데이터)
faith.lm <- lm(waiting~eruptions,data=faithful) #분출시간에 따른 대기시간의 변화
coef(faith.lm)[1]
coef(faith.lm)[2]
#y=ax+b
#a: 기울기(coefficient), b: y절편(intercept)

#fitted : 회귀모델에서 예측값 추출
fitted(faith.lm) #y값
#데이터의 ~뒤 값들을 만들어진 선형회귀 모델에 대입한 것

#회귀선 (rmse값이 최소인 값)
lines(x=faithful$eruptions,y=fitted(faith.lm),col='blue')

#선 긋기
abline(v=3,col='purple')
abline(h=mean(faithful$waiting),col='green')
abline(a=coef(faith.lm)[1]+10,b=coef(faith.lm)[2],col='green')
abline(faith.lm,col='red')

#plot(x) : x 타입에 따라 출력되는 그래프가 달라짐
#벡터, 데이터프레임, 팩터, 시계열, 테이블, 선형회귀 모델...

str(cars)
plot(cars) 
#벡터는 산점도 그래프

str(ToothGrowth)

plot(ToothGrowth$supp,ToothGrowth$len)
#x값이 팩터. 팩터는 박스 그래프

plot(iris[,1:4])
#변수가 2개 넘으면 행렬식 구성

nhtemp
str(nhtemp)
plot(nhtemp)
#시계열은 연결선

UCBAdmissions
str(UCBAdmissions)
plot(UCBAdmissions)
#테이블 데이터는 모자이크 도표로 출력
#모자이크 조각의 크기로 교차표의 상대적 빈도 확인

png('myplot.png',width=800,height=400)

windows()
savePlot('myplot.pdf',type='pdf')

----#머신러닝 #KNN--------------
#컴퓨터비전(얼굴 매칭, 물체식별)
#영화, 음악 추천
#질병 분류, 유전자 데이터 패턴 인식
# 장점 :  쉽다, 훈련이 빠르다, 데이터 분포에 대한 가정 안함
# 단점 :  k값 설정 어렵다, 분류 작업이 느리다, 직관적이지 않다

bc <- read.csv('wisc_bc_data.csv')
bc <- bc[-1]
str(bc)
table(bc$diagnosis)
bc$diagnosis <- factor(bc$diagnosis,levels=c('B','M'),
                       labels=c('Benign','Malignant'))
round(prop.table(table(bc$diagnosis))*100,1)

summary(bc[c('radius_mean','area_mean','smoothness_mean')])

#정규화
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
normalize(c(10,20,30,40,50))

lapply(bc[2:31],normalize) #결과가 리스트
sapply(bc[2:31],normalize) #결과가 리스트

bc_n <- as.data.frame(lapply(bc[2:31],normalize))

#연습문제 8:2 비율로 train/test
set.seed(2021)
sample(1:nrow(bc),nrow(bc)*0.8)

wbcd_train <- bc_n[1:469,]
wbcd_test <- bc_n[470:569,]

wbcd_train_labels <- bc[1:469,1]
wbcd_test_labels <- bc[470:569,1]
wbcd_train_labels

#모델 생성
library(class)
wbcd_predict <- knn(train=wbcd_train, test=wbcd_test, 
    cl=wbcd_train_labels,k=21)
wbcd_predict #예측결과
wbcd_test_labels #답
#교차표로 둘 비교
install.packages('gmodels')
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_predict)

#표준화
wbcd_z <- as.data.frame(scale(bc[-1]))
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_z_pred <- knn(train=wbcd_train, test=wbcd_test,
    cl=wbcd_train_labels,k=21)
CrossTable(x=wbcd_test_labels,y=wbcd_z_pred)$t[1,1]+CrossTable(x=wbcd_test_labels,y=wbcd_z_pred)$t[2,2]

#k값에 변화를 주면서 가장 테스트 정확도 높았을때 k값 (1~25)
#코드와 정확도 함께 카피
acc <- c()
for(i in seq(1,25,2)){
  wbcd_z_pred <- knn(train=wbcd_train, test=wbcd_test,
                     cl=wbcd_train_labels,k=i)
  acc <- c(acc,sum(wbcd_test_labels==wbcd_z_pred)/length(wbcd_test_labels))
}
which(acc==max(acc))


#---연습---
set.seed(1234)
idx <- sample(1:nrow(iris),nrow(iris)*0.7)
iris_train <- iris[idx,]
iris_test <- iris[-idx,]

str(iris_train)
iris_train_n <- as.data.frame(lapply(iris_train[-5],normalize))
iris_test_n <- as.data.frame(lapply(iris_test[-5],normalize))
iris_predict <- knn(train=iris_train_n,test=iris_test_n,cl=iris_train[,5],k=3)
sum(iris_test[,5]==iris_predict)/length(iris_predict)
CrossTable(iris_test[,5],iris_predict)

acc <- c()
for(i in seq(1,25,2)){
  iris_predict <- knn(train=iris_train_n,test=iris_test_n,cl=iris_train[,5],k=i)
  acc <- c(acc,sum(iris_test[,5]==iris_predict)/length(iris_predict))  
}
acc
index <- which(acc==max(acc))
k <- 2*index-1
acc[index]
paste('k=',k,acc[index])

#2.
