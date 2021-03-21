p <- c(2,3,5,19,22,14,199)
p[c(TRUE,TRUE,FALSE)] 

#which():논리값-> TRUE위치 인덱스 추출 함수
data <- c(10:21)
data
which(data>15)
which.min(data)
length(data)
data <- c(100:111)
month.abb[which(data>105)]

#요일별 교통사고 사망자수 데이터
traffic.death <- c(100,90,80,70,120,150,200)
names(traffic.death) <- c('mon','tue','wed','thu','fri','sat','sun')
names(traffic.death[traffic.death>=100])


#팩터 : 카테고리 구분하는 범주형 데이터
#레벨 : 팩터에 포함된 범주값
#factor() : 문자/숫자 벡터를 범주형 데이터로 변형
review <- c('good','good','bad','ind','bad','good')
review.factor <- factor(review)
review.factor
str(review.factor)
as.numeric(review.factor) #팩터형->숫자벡터로 변환

everyday <- c('mon','mon','fri','fri','tue')
everyday.factor <- factor(everyday)
everyday.factor
everyday.factor <- factor(everyday, levels=c('mon','tue','wed','thu','fri','sat','sun'))
levels(everyday.factor)
levels(review.factor) <- c('B','G','I')
review.factor
nlevels(review.factor)

#서열 팩터 : 순서가 있는 범주형 데이터
eval <- c('medium','low','high','medium','high')
eval.factor <- factor(eval)
eval.ordered <- factor(eval, levels=c('low','medium','high'),ordered=TRUE)
eval.ordered

#table(): 각 레벨별 빈도
eval.factor
table(eval.factor)
table(eval.ordered)

#숫자벡터 ->팩터로 변환
gender <- c(2,1,2,2,1,0)
#남:1 여:2
factor(gender)
gender.factor <- factor(gender, levels=c(1,2), labels=c('male','female'))
gender.factor

#matrix 함수로 행렬 생성
#dim 함수로 벡터에 차원 부여

v <- 1:12
dim(v) <- c(3,4)
v
dim(v)
matrix(data=v, nrow=3, ncol=4, byrow=TRUE)

rnames <- c('r1','r2','r3')
cnames <- c('c1','c2','c3','c4')
matrix(v,3,4,byrow=TRUE,dimnames=list(rnames,cnames))

matrix(0,3,4)
matrix(NA,3,4)
mat <- matrix(v,ncol=4)
str(mat)
mat
dim(mat)
dim(mat)[1]
nrow(mat)
ncol(mat)
length(mat)
#벡터 2개 결합해 행렬
v1 <- 1:5
v2 <- 6:10
rbind(v1,v2)
cbind(v1,v2)

1:3
4:6
cbind(1:3,4:6,matrix(7:12,3,2))
rbind(matrix(1:6,2,3),matrix(7:12,2,3))
mat <- matrix(v,3,4)
mat[1,]
mat[1,,drop=FALSE]
mat[2:3,]
mat[,3:4]
mat[,3]
mat[,3:5]
mat[,2:3]
mat[,c(1,3)]
mat
mat[,c(1,4)]
mat[,-c(2,3)]
mat[1:3,-3]
mat[1,3] <- 77
mat[2,] <- c(22,55,22,55)
mat[2,] <- 33
mat
mat[2:3,3:4] <- c(1:4)
mat
city <- c('seoul','busan','daegu','gwangju','jeonju')
v <- c(0,350,300,300,200,350,0,50,200,190,300,50,0,180,200,300,200,180,0,80,200,190,200,80,0)
city.distance.mat <- matrix(v,5,5,byrow=TRUE,dimnames=list(city,city))
city.distance.mat
city.distance.mat['seoul','busan']

w <- c(1:6)      
mtx <- matrix(w,2,3)
mtx*4
matrix(1:6,2,3)%*%matrix(6:1,3,2)
t(mtx)
mtx
t(t(mtx))
rowSums(mtx)
colSums(mtx)
rowMeans(mtx)
colMeans(mtx)

#연습
#1.
x <- c(1:10)
y <- c(3:1)
names(x) <- letters[x]
x+y

names(x) <- letters[x+y]
x+y

#2.
two <- c(2,5,3)
rep(two,5)
rep(two,length.out=10)
rep(two, c(2,5,3))

#3.
dice <- c(3,2,5,1,5,6,5)
dice.factor <- factor(dice,levels=c(1:6),labels=c('one','two','three','four','five','six'))
table(dice.factor)
summary(dice.factor)

#4.
alpha.mtx <- matrix(1:12,3,4,dimnames=list(letters[1:3],letters[1:4]))
alpha.mtx

#5.
alpha.cbind.mtx <- cbind(alpha.mtx[,c(1,3)],alpha.mtx[,c(2,4)])
alpha.cbind.mtx

#6.
alpha.mtx+alpha.cbind.mtx
alpha.mtx-alpha.cbind.mtx
alpha.mtx*alpha.cbind.mtx
alpha.mtx/alpha.cbind.mtx
alpha.mtx%*%t(alpha.cbind.mtx)

#7.
v <- 1:9999
dim(v) <- c(1111,9)
v[-(1:1108),-(1:7)]

m <- matrix(1:9999,,9)
m
m[(nrow(m)-2):nrow(m),8:9]
new <- matrix(1:10,2,5)
new[nrow(new)]
new[ncol(new)]
