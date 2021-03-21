#그래프 관련 함수
#curve(함수나 표현식, 시작점, 끝점)
curve(sin,)
curve(sin,0,2*pi)


#선형회귀 -> 신뢰구간 출력
#속력에 따른 거리의 변화

dev.off()
cars
m <- lm(dist~speed,cars)
p <- predict(m, interval='confidence') #신뢰구간
#fit=직선, lwv=하한구간, upr=상한구간
#fit=회귀모델로 적합된 값
#각 행은 cars 데이터의 각 행에 대한 신뢰구간 표시
#1행은 speed 4에 대한 dist값 예측

plot(cars, cex=0.5)
#자료 내용 출력(문자열)
text(cars$speed,cars$dist,pos=2,cex=0.5) #pos는 위치
identify(cars$speed,cars$dist) #표 우측에 finish 눌러야 나옴
abline(m) #회귀선

head(cars)

#신뢰구간 다각형으로 만들기
#벡터 만들어서 그 안에 넣기
x <- c(cars$speed,tail(cars$speed,1),rev(cars$speed),cars$speed[1]) #rev는 역순
y <- c(p[,'lwr'],tail(p[,'upr'],1),rev(p[,'upr']),p[,'lwr'][1])
#다각형 만들기
polygon(x,y,col=rgb(.7,.7,.7,.5)) #마지막값은 투명도

#iris로 점 찍기
plot(iris$Sepal.Width,iris$Sepal.Length,cex=0.5,pch=20,
     xlab='width',ylab='length')
points(iris$Petal.Width,iris$Petal.Length,cex=0.5,pch='+'
       ,col='#ff0000')
#범례
legend('topright',legend=c('sepal','petal'),pch=c(20,43),
       cex=0.8,col=c('black','red'),bg='grey')

#박스
boxplot(iris$Sepal.Width)
box <- boxplot(iris$Sepal.Width)
#박스의 정보
box #out:극단치
boxplot(iris$Sepal.Width,horizontal=T)
hist(iris$Sepal.Length,breaks='sturges') #히스토그램
#sturges=log2(n)+1
hist(iris$Sepal.Length,breaks=8,freq=F) #default는 개수
#freq=F 확률밀도로 합이 1
h <- hist(iris$Sepal.Length,breaks=8)
h
#밀도 그래프
plot(density(iris$Sepal.Width))
#히스토그램과 비교
hist(iris$Sepal.Width,freq=F)
lines(density(iris$Sepal.Width))

#바 그래프
barplot(tapply(iris$Sepal.Width,iris$Species,mean))

#데이터 비율 :pie그래프 (구간 나눠서)
pie(table(cut(iris$Sepal.Width,breaks=10)))


library(ggplot2)
#3층 레이어로 그래프 그릴 수 있음
#데이터프레임만 가능
#1층: 배경 + 2층: 그래프 + 3층: 범위,설정
#aes:배경(변수설정)
ggplot(data=mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  labs(title='practice',
       x='weight',y='mpg')

mtcars$gear <- factor(mtcars$gear, levels=c(3,4,5),
                      labels=c('3gear','4gear','5gear'))
mtcars$cyl <- factor(mtcars$cyl, levels=c(4,6,8),
                      labels=c('4cyl','6cyl','8cyl'))

ggplot(data=mtcars, aes(x=mpg))+
  geom_histogram()+
  facet_grid(cyl~.)+
  labs(title='cyl vs mpg')

ggplot(data=mtcars, aes(x=cyl,y=mpg))+
  geom_boxplot()+
  labs(title='cyl x mpg')

ggplot(data=mtcars, aes(x=mpg, fill=cyl))+
  geom_density()+
  labs(title='mpg x cyl density')





#kmeans 머신러닝 알고리즘(클러스터링)
#미국 10대 학생 시장 세분화

teens <- read.csv('snsdata.csv')
str(teens)
# 앞의 4변수는 각 학생의 정보(졸업년도, 성별, 나이, 친구 수)
# 나머지는 가장 많이 언급된 36개 단어
# NA값은 성별과 나이에 포함
table(teens$female)
table(teens$gender, useNA='ifany') #NA를 하나의 변수로
#성별 NA값 2724개
#1은 여성, 0은 남성이거나 NA
teens$female <- ifelse(teens$gender=='F'&!is.na(teens$gender),1,0)
#성별이 없으면 1
teens$nogender <- ifelse(is.na(teens$gender),1,0) 

#나이 NA값 5086개, 이상치 없애기
#13세 이상 20세 미만: 정상, 그 외는 NA
teens$age <- ifelse(teens$age>=13&teens$age<20,teens$age,NA)
summary(teens$age)
#나이 NA값 5523개

#졸업 연도 별 나이의 평균 출력
with(teens,tapply(age,gradyear,mean,na.rm=T)) #결과 array
library(dplyr)
teens %>% group_by(gradyear) %>% 
  summarise(mean(age,na.rm=T)) #결과 tibble
aggregate(data=teens,age~gradyear,mean,na.rm=T) #결과 dataframe
#평균구하는 함수 ave
ave_age <- ave(teens$age,teens$gradyear,FUN=function(x) mean(x,na.rm=T))

#나이 결측값 대체
teens$age <- ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

#kmeans
#kmeans(x=숫자형 매트릭스,centers=k)
interests <- teens[5:40] #36차원 데이터임
#각 열단위로 표준화(평균=0, 표준편차=1)
interests_z <- as.data.frame(lapply(interests, scale))

#클러스터 랜덤하게 잡으므로 set.seed로 고정
set.seed(2345)
teen_clusters <- kmeans(interests_z,5) #군집화

teen_clusters

# Cluster means:
#   각 클러스터의 centeroid의 좌표
#   표준화된 데이터라 양수면 평균보다 많이 언급, 
#   음수면 평균보다 낮게 언급한 것.
# cluster vector:
#   각 데이터(행)가 어느 그룹에 속하는지

#cluster 정보
teen_clusters$size #k값 : 그룹개수
teen_clusters$centers #Cluster means:
teen_clusters$cluster #cluster vector:
teen_clusters$withinss #클러스터 내 데이터간 거리의 제곱합. 작을수록 동질성 높음
teen_clusters$tot.withinss #sum(withinss)
teen_clusters$betweenss #클러스터 간 중심의 거리 제곱합. 클수록 이질성 높음

#클러스터 종류 teens에 추가
teens$cluster <- teen_clusters$cluster
#각 클러스터 나이의 평균
aggregate(data=teens,age~cluster,mean) #클러스터의 특징에 나이는 없는듯
#각 클러스터 성별의 수
aggregate(data=teens,female~cluster,mean)
#각 클러스터 친구 수
aggregate(data=teens,friends~cluster,mean)


#연습
#1.
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212),'hwy'] <- NA

str(mpg)
table(mpg$drv) #NA 없음
summary(mpg$hwy) #NA 5개
colSums(is.na(mpg)) #drv에 0개, hwy에 5개
mpg %>% 
  filter(!is.na(hwy)) %>%
  group_by(drv) %>% 
  summarise(mean.hwy=mean(hwy)) %>% 
  arrange(-mean.hwy)

#2.
mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k" # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) # cty 이상치 할당
mpg$drv <- ifelse(mpg$drv %in% c('4','f','r'), mpg$drv, NA) 
mpg$drv
summary(mpg$cty)
box_cty <- boxplot(mpg$cty)
lwr <- box_cty$stats[1,1]
uppr <- box_cty$stats[5,1]
mpg <- mpg %>% 
  mutate(cty=ifelse(cty<lwr|cty>uppr,NA,cty))
boxplot(mpg$cty) 


aggregate(data=mpg,cty~drv,mean,na.rm=T)

mpg %>%
  filter(!is.na(drv)) %>%   
  group_by(drv) %>% 
  summarise(mean=mean(cty,na.rm=T))

View(sns)
#3.

#표준화
iris_z <- as.data.frame(lapply(iris[-5],scale))
set.seed(2021)
iris_cluster <- kmeans(iris_z,3)
iris_cluster$tot.withinss
iris_cluster$betweenss

#4.
sns <- read.csv('snsdata.csv')
summary(sns)
str(sns)

#------성별 결측값-------
table(sns$gender,useNA='ifany')

female <- sns %>% 
  filter(gender=='F') %>% 
  select(,-(1:4))
male <- sns %>% 
  filter(gender=='M') %>% 
  select(,-(1:4))

a <- order(-colSums(female))[1:5] #여자 관련 키워드 top5
b <- order(-colSums(male))[1:5] #남자 관련 키워드 top5

#공통 제외
key <- setdiff(b,a)+4 #남자 관련 키워드 #6 21 5

index <- which(is.na(sns$gender))
sns$gender[index] <- ifelse(sns[,key[1]]>0|sns[,key[2]]>0|sns[,key[3]]>0,'M','F')
table(sns$gender,useNA='ifany')

#----나이 결측값-----
summary(sns$age)
boxplot(sns$age)
box.age <- boxplot(sns$age)
box.age
#극단치 없애기
sns$age <- with(sns,ifelse(age<13|age>21,NA,age))

#졸업연도 별로 나이 평균
df <- aggregate(data=sns,age~gradyear,mean,na.rm=T)
sns$age[is.na(sns$age)] <- ifelse(sns$gradyear[is.na(sns$age)]==df$gradyear[1],df$age[1],
       ifelse(sns$gradyear[is.na(sns$age)]==df$gradyear[2],df$age[2],
              df$age[3]))

ave.age <- ave(sns$age,sns$gradyear,FUN=function(x) mean(x,na.rm=T))
sns$age <- ifelse(is.na(sns$age),ave.age,sns$age)

summary(sns$age)

#수치형 표준화
sns_z <- as.data.frame(lapply(sns[-(1:4)],scale))

set.seed(0311)
sns_cluster <- kmeans(sns_z,5)
sns_cluster$tot.withinss
sns_cluster$betweenss
