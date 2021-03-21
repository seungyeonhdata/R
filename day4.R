id <- c('a1','a2','a3')
name <- c('x','y','z')
price <- c(1000,2000,3000)
product <- data.frame(id,name,price)
product
str(product)
product <- rbind(product,c('a4','s',4000))
product
#열추가
cbind(product, madein=c('kor','chi','kor','usa'))
product$madein <- c('kor','ind','kor','usa')
product

#여러열 추가. 데이터프레임 이용
new.cols <- data.frame(menu=c('aa','bb','aa','cc'),
           quantity=c(10,20,10,30)
           )
cbind(product,new.cols)

df1 <- data.frame(gender='f',months=1,weight=55)
df2 <- data.frame(gender='f',months=3,weight=58)
df3 <- data.frame(gender='m',months=4,weight=105)
df4 <- data.frame(gender='m',months=10,weight=75)
df5 <- data.frame(gender='f',months=12,weight=85)
lst <- list(df1,df2,df3,df4,df5)
lst
class(lst[1])
class(lst[[1]])
str(lst[[1]])

rbind(lst[[1]],lst[[2]])

#여러 데이터프레임 합쳐서 데이터프레임 만들기
#do,call() : 리스트 내의 여러개 데이터프레임 합침
do.call(rbind,lst)

#리스트들 합쳐서 데이터프레임 만들기
lst1 <- list(gender='f',months=1,weight=55)
lst2 <- list(gender='f',months=3,weight=58)
lst3 <- list(gender='m',months=4,weight=105)
lst4 <- list(gender='m',months=10,weight=75)
lst5 <- list(gender='f',months=12,weight=85)
lstt <- list(lst1,lst2,lst3,lst4,lst5)
lstt
class(do.call(rbind,lstt))
lstt[1]
as.data.frame(lstt[1]) #lstt 안의 모든 list를 dataframe으로 변환해줘야함
#lapply() : 리스트에 함수 적용
do.call(rbind, lapply(lstt,as.data.frame))

#merge로 합치기
state.x77 
class(state.x77) #matrix임
states <- data.frame(state.x77) #데이터프레임으로 변환
str(states)
row.names(states) #행 이름 벡터로 출력
#행을 열로 바꿔 추가하기(행도 데이터로 쓰기)
states$name <- row.names(states) #행 이름 열에 추가
states
row.names(states) <- NULL #행 지우기

head(states) #상위 6개 자료
tail(states,10) #하위 10개 자료

#states에서 income이 5000달러 이상인 자료 출력
rich <- states[states$Income>=5000,c('name','Income')]

large <- states[states$Area>=100000,c('name','Area')]

#merge로 합치기 (기본값 : 공통으로 열(name)이 일치하는 자료)
merge(rich,large) #교집합
merge(rich,large,all=TRUE) #합집합

#subset(): 행과 열 선택
subset(states, Income>5000 & Area>50000) #income이 5000넘는 자료의 모든 정보 추출
subset(states, name=='Alaska')
subset(states, subset=(Income>5000 & Area>50000), select=c(Murder,Illiteracy))

mtcars
subset(mtcars,mpg>30)
subset(mtcars,mpg>30,c(mpg,disp))
mtcars[mtcars$mpg>30,]

#cyl가 4이고 am이 0인 mtcars자료의 mpg,hp,wt 출력
mtcars[mtcars$cyl==4&mtcars$am==0,c('mpg','hp','wt')]
mtcars[mtcars$mpg>mean(mtcars$mpg),c('mpg','hp','wt')]
subset(mtcars, cyl==4&am==0, c(mpg,hp,wt))
subset(mtcars, mpg>mean(mpg), c(mpg,hp,wt))
subset(mtcars, select=c(mpg,hp,wt), subset=mpg>mean(mpg))


class(iris)
subset(iris,select=-c(Sepal.Width,Petal.Length))
iris$Sepal.Length/iris$Sepal.Width #비율
#특성공학: 주어진 데이터 칼럼으로부터 연산을 수행하여 새로운 칼럼만듦
#with(): 여러개의 명령 한번에 실행, iris$생략
with(iris,Sepal.Length/iris$Sepal.Width)
with(iris, {
  print(summary(Sepal.Length))
  plot(Sepal.Length, Sepal.Width)  
})

#기술통계
summary(iris$Sepal.Length)

with(iris,{
  stats <<-summary(Sepal.Length)
  stats
})
#<<- : 현재 속해있는 변수환경 하나 위에 있는 환경에 있는 변수에 값을 부여할 때

stats


iris$Sepal.Ratio <- iris$Sepal.Length/iris$Sepal.Width
iris <- within(iris, Sepal.Ratio <- Sepal.Length/Sepal.width)
iris
#with, within은 거의 동일(코드의 양 줄여줌)
#within은 데이터 수정 가능

#Sepal.Ratio가 na이면 중앙값으로 대체
within(iris,{   #        조건,                 참,                     거짓
  Sepal.Ratio=ifelse(is.na(Sepal.Ratio), median(Sepal.Ration,na.rm=T),Sepal.Ratio)
})

#sqldf패키지 : SQL 명령문 사용 가능
install.packages('sqldf') #패키지 설치
library(sqldf)
#Structured Query Language : 데이터베이스에 데이터를 검색/수정/삭제/추가 작업
#수행하는 질의어

sqldf('select * from mtcars', row.names=T)
sqldf('select cyl from mtcars', row.names=T)
sqldf('select * from mtcars where mpg>30 order by hp', row.names=T)
sqldf('select avg(mpg) as avg_mpg from mtcars where mpg>30 group by cyl', row.name=T)

class(state.x77)
st <- state.x77 #배열
st <- data.frame(st) #데이터프레임
colnames(st)
rownames(st)

dim(st) #50 obs, 8 features
rowSums(st)
rowMeans(st)




#연습

#1.
x <- c(-0.2,1,2,1.5,2.2)
name <- c('kim','lee','park','lim')
2:9
seq(0,10,0.5)
rep(c(1,2,3),3)

#2.
x <- c(2,-1,3,7,0.5,8)
x[5]
x[1:3]
x[c(6,2,4)]
x[-3]
x[x>0]
x[x%%2==0]
x <- x[x%%2==0]
x

#3.
library(MASS)
cats #생쥐데이터
class(cats)
length(cats)*nrow(cats)
mean(cats$Bwt)
mean(cats$Hwt)
sd(cats$Bwt)
sd(cats$Hwt)
female <- cats[cats$Sex=='F',]
mean(female$Bwt)
sd(female$Bwt)
mean(female$Hwt)
sd(female$Hwt)
male <- cats[cats$Sex=='M',]
mean(male$Bwt)
sd(male$Bwt)
mean(male$Hwt)
sd(male$Hwt)

with(cats,{
  print(mean(Bwt))
  print(mean(Hwt))
  print(sd(Bwt))
  print(sd(Hwt))
})

cats
colMeans(cats[2:3], na.rm=T)


#4.
d1 <- 1:50
d2 <- 51:100
d1
d2
length(d2)
d1+d2
d2-d1
d1*d2
d2/d1
sum(d1)
sum(d2)
sum(d1,d2)
max(d2)
min(d2)
mean(d2)-mean(d1)



#5.
v1 <- 30:100
v1[v1<60]
sum(v1<70)
sum(v1[v1>65])
v1[v1>60&v1<73]
subset(v1,subset=(v1>60&v1<73))

v1[v1<65|v1>80]
v1[v1%%7==3]
v1[v1%%7==0] <- 0
v1
sum(v1[v1%%2==0])
v1[v1%%2!=0|v1>80]
v1[v1%%3==0 & v1%%5==0]
v1[v1%%2==0] <- v1[v1%%2==0]*2
v1
v1 <- v1[(v1%%7!=0)]
v1



BTS <- c(5,4,1.5)
SON <- c(4,5,2)
Red <- c(2.5,2,1)
Twice <- c(3.5,4,5)
p <- data.frame(row.names=c('밀정','경이로운소문','국제시장'),BTS,Red,Twice)
p
p-SON
p.son <- sqrt(colSums((p-SON)^2))
p.son
p.son[min(p.son)]



#6.
data(iris)
iris
temp <- iris
temp
df <- t(temp[1:4])
df
ex <- c(4.0, 3.0, 1.5, 0.15)
ex
temp$dist <- sqrt(colSums((df-ex)^2))
table(head(temp[order(temp$dist),],9)$Species)

#표준화(평균:0, 표준편차:1) / 정규화
#표준화 = (각 열 data - 각 열 평균) / 각 열 표준편차
#정규화 = (각 열 data - 각 열 최소값) / (각 열 최대값 - 각 열 최소값)

iris.mean <- c(mean(iris$Sepal.Length),mean(iris$Sepal.Width),mean(iris$Petal.Length),mean(iris$Petal.Width))
iris.sd <- c(sd(iris$Sepal.Length),sd(iris$Sepal.Width),sd(iris$Petal.Length),sd(iris$Petal.Width))
iris.mean
iris.sd

iris.temp <- t(iris[1:4])
iris.std <- (iris.temp-iris.mean)/iris.sd
iris.std
ex.std <- (ex-iris.mean)/iris.sd
ex.std
iris$distance <- sqrt(colSums(iris.std-ex.std)^2)
head(iris[order(iris$distance),],9)

data(iris)
iris
scale(iris[1:4])
scale(ex)

#정규화
data(iris)
min(iris[,-5]) #데이터프레임이라서 연산안됨 #각 열 데이터 따로 해야함..
cbind(iris, scale=scale(iris[,-5]))

var(iris[,-5])
mean(iris[,-5])

iris.nor <- (iris[,-5]-min(iris[,-5]))/(max(iris[,-5])-min(iris[,-5]))
iris.nor #안됨

#apply(데이터, 행(1)/열(2), 함수)
apply(iris[,-5],2,scale)
apply(iris[,-5],2,function(x){(x-mean(x,na.rm=T))/sd(x)})
apply(iris[,-5],2,function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))})





#뻘짓하며 공부한 흔적

n.ex <- (ex-min(ex))/(max(ex)-min(ex))
n.ex

n.temp <- within(temp,{
  Sepal.Length <- (Sepal.Length-min(Sepal.Length))/(max(Sepal.Length)-min(Sepal.Length))
  Sepal.Width <- (Sepal.Width-min(Sepal.Width))/(max(Sepal.Width)-min(Sepal.Width))
  Petal.Length <- (Petal.Length-min(Petal.Length))/(max(Petal.Length)-min(Petal.Length))
  Petal.Width <- (Petal.Width-min(Petal.Width))/(max(Petal.Width)-min(Petal.Width))
})
n.temp
n.temp[1:4]
n.temp$dist <- sqrt(rowSums((n.temp[1:4]-n.ex)^2))
n.temp
a <- head(n.temp[order(n.temp$dist),],9)
a
table(a$Species)


iris[,-c(4,5)]
iris$distance<-sqrt(rowSums((iris[,-c(4,5)]-new)^2))
iris
iris.top9 <- head(iris[order(iris$distance), ],9)
iris.top9
table(iris.top9['Species'])





iris.mean <- c(mean(iris$Sepal.Length),mean(iris$Sepal.Width),mean(iris$Petal.Length),mean(iris$Petal.Width))
iris.sd <- c(sd(iris$Sepal.Length),sd(iris$Sepal.Width),sd(iris$Petal.Length),sd(iris$Petal.Width))
p <- (ex-iris.mean)/iris.sd
p


s.iris <- as.data.frame(scale(iris[,1:4]))
(s.iris)
iris$distance <- sqrt(rowSums((p-s.iris)^2))
iris
iris[order(iris$distance),]

iris.f <- iris[1:4]
iris.f
euc <- sqrt(colSums((iris.f-ex)^2))
euc
iris['distance'] <- euc
iris

#1)
data(iris)
new <- c(4.0, 3.0, 1.5, 0.15)
new <- (new-min(new))/(max(new)-min(new))
new

iris <-within(iris,{
  Sepal.Length = (Sepal.Length - min(Sepal.Length)) / (max(Sepal.Length) - min(Sepal.Length))
  Sepal.Width = (Sepal.Width - min(Sepal.Width)) / (max(Sepal.Width) - min(Sepal.Width))
  Petal.Length = (Petal.Length - min(Petal.Length)) / (max(Petal.Length) - min(Petal.Length))
  Petal.Width = (Petal.Width - min(Petal.Width)) / (max(Petal.Width) - min(Petal.Width))
})
iris
iris$distance<-sqrt(rowSums((iris[,-c(4,5)]-new)^2))
iris
iris.top9 <- head(iris[order(iris$distance), ],9)
iris.top9
table(iris.top9['Species'])


#2)
target<-(c(4.0, 3.0, 1.5, 0.15)-
           c(mean(iris$Sepal.Length),mean(iris$Sepal.Width),mean(iris$Petal.Length),mean(iris$Petal.Width)))/
  c(sd(iris$Sepal.Length),sd(iris$Sepal.Width),sd(iris$Petal.Length),sd(iris$Petal.Width))
target
iris.std<-t(scale(iris[,1:4]))
iris.std
dist<-sqrt(colSums((iris.std-target)^2))
dist
iris.new<-cbind(iris,dist)
iris.new
iris.new<-iris.new[c(order(iris.new$dist)),]
iris.new
table(iris.new$Species[1:9])
iris

#3)
# 정규화 : (각각 자료값-최소값)/(최대값-최소값)
iris
x <- c(4.0-min(iris$Sepal.Length)/(max(iris$Sepal.Length) - min(iris$Sepal.Length)), 
       3.0- min(iris$Sepal.Width) / (max(iris$Sepal.Width) - min(iris$Sepal.Width)), 
       1.5- min(iris$Petal.Length) / (max(iris$Petal.Length) - min(iris$Petal.Length)), 
       0.15 - min(iris$Petal.Width) / (max(iris$Petal.Width) - min(iris$Petal.Width)))
x

# 정규화 작업(normalization) : (각각의 자료값 - 최소값) / (최대값 - 최소값)
iris$Sepal.Length <- (iris$Sepal.Length - min(iris$Sepal.Length)) / (max(iris$Sepal.Length) - min(iris$Sepal.Length))
iris$Sepal.Width <- (iris$Sepal.Width - min(iris$Sepal.Width)) / (max(iris$Sepal.Width) - min(iris$Sepal.Width))
iris$Petal.Length <- (iris$Petal.Length - min(iris$Petal.Length)) / (max(iris$Petal.Length) - min(iris$Petal.Length))
iris$Petal.Width <- (iris$Petal.Width - min(iris$Petal.Width)) / (max(iris$Petal.Width) - min(iris$Petal.Width))
iris

iris.euclid <- within(iris, euclid <- sqrt(rowSums((iris[,-5] - x)^2)))
iris.euclid

key<-sort(iris.euclid$euclid)[1:10]
key
summary(iris.euclid$Species[iris.euclid$euclid<=key])






#4)
dim(t(new[1:4]))
infomat<-t(new[1:4])
#표준화
infomat<-(infomat-c(msl,msw,mpl,mpw))/c(ssl,ssw,spl,spw)
infomat

dist<-(infomat-infomat[,151])^2
dist<-sqrt(colSums(dist))
data.frame(dist)
new<-cbind(new,dist)
key<-sort(new$dist)[10]
summary(new$Species[new$dist<=key])



















