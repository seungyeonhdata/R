a <- 1:24
dim(a) <- c(3,4,2) #3차원, 2개의 면(테이블), 3행 4열
a

#array함수 :  다차원 배열 생성
ary <- array(1:24, c(3,4,2))
ary[1,3,2]
ary[1,,,drop=FALSE]

#두번째 면에 있는 1번 열의 값
ary[,1,2]


lst <- list(0.6,0.9,'fruits')
lst
lst[[3]] <- 'apple'
lst[[4]] <- matrix(1:6,2,3)
lst
names(lst) <- c('n1','n2','n3','n4')
lst$n1
names(lst)
length(lst)
#lst[[n]]:lst의 n번째 원소 선택. 원소 저장형식 그대로 가짐
#lst[n]:lst의 n번째 원소 선택. 리스트 형식으로 출력
prod <- list('a1','mouse',3000)
prod
prod[3]
prod[[3]]*2
class(prod[[3]])
class(prod[3])

prod[c(1,2)]
prod[c(TRUE,FALSE,TRUE)]
prod[-1]
prod <- list(id='a001',name='mouse',price=3000)
prod
prod[['name']]
prod$name
prod[c('name','price')]
prod[['name']]
prod['na']
prod[4]
prod[5]
prod[[5]]
prod

lst <- list(1,2,list(a=3.1,b=3.2))
lst[['three']]
lst['three']
lst$three
lst[[3]]
lst[3]
lst[[3]][['a']]

#하나의 원소에 여러개의 값을 할당할때 [[]],[]는 다름
#[[]]는 벡터 형식으로 줘야하고
#[]는 리스트 형식으로 변환해야함

prod[[3]] <- c(40000,50000)
prod[[3]]
prod[3] <- list(c(20000,30000))
prod[[3]]
prod[3]
prod[1:3] <- list('a02','monitor',10000)
prod
prod[[4]] <- c(type=c('domestic','export'))
prod
prod[['madein']] <- c('korea','china')
prod
length(prod)
prod[6:8] <- list(0.1,0.2,'0.3')
prod
prod[4] <- NULL

n <- c('n1','n2','n3')
v <- c(100,200,300)
mylist <- list()
mylist[n] <- v
mylist$n2 <- c(100,200,300)
mylist
#조건에 따른 원소제거
mylist[mylist<200] <- NULL
mylist

#리스트 결합
c(prod,mylist)

#r의 수치연산 관련 함수 대부분은 벡터 자료구조 사용
mydata <- list(1.5,2.0,3.5,4.5)
mean(unlist(mydata))
max(unlist(mydata))

#데이터프레임은 2차원 리스트
v1 <- c('a1','a2','a3')
v2 <- c(10,20,30)
v3 <- c('x','y','z')
data.frame(v1,v2)
df <- data.frame(v1,v2,v3)
str(df)

#이미지 데이터라면? 100*100이라면

#  x1 x2 ... x10000
#0  0 20 ... 255
#1  0 20 ... 255
#...
#99 0 20 ... 255

#variable(feature)는 10000
#obs(instance,object)는 관측치 (몇장인지)
#각 이미지의 차원은 10000차원
#각각의 이미지가 10000차원 공간의 (0,2.,...,255) 위치에 점으로 표시됨
#점 사이의 거리를 구할 수 있다. 가까운 점이 유사한 이미지
#두 점 사이의 거리는 피타고라스의 정리로 구하듯
#만 차원이니까 유클리디안 거리로 이미지 유사도 분석 가능

data.frame(row.names=v1,v2,v3)
str(data.frame(id=v1, name=v2, price=v3))
#stringAsFactors : 벡터 -> 데이터프레임 생성시 데이터타입을 팩터로 지정
stringAsFactors=TRUE #문자열을 팩터로 읽는다.

p <- data.frame(id=v1, price=v2, name=v3, stringsAsFactors = TRUE)
p
str(p)
#문자열 총 10개면 stringAsFactors=FALSE 주면 문자열 유지

#as.data.frame(): 행렬, 리스트로부터 데이터프레임 생성
mat <- matrix(c(1,2,3,4,5,6), ncol=2)
mat
as.data.frame(mat)

num <- as.data.frame(mat)
colnames(num) <- c('d1','d2')#열이름 변경
num
v1<-c("a1","a2","a3")
v2<-c(10,20,30)
v3<-c("x","y","z")

lst <- list(v1,v2,v3)
lst
p <- as.data.frame(lst)
p
colnames(p) <- c('id','name','price')
rownames(p) <- c('a','b','c')
p
length(df)#열의 개수
length(p)#데이터 프레임은 열의 개수

state.abb #미국 주 약자
state.name
state.region
str(state.region)
us.state <- data.frame(state.abb,state.name,state.region,state.area)
us.state
us.state[['state.name']] #벡터 형식
us.state['state.name'] #state.name 열 추출. 데이터프레임 형식
us.state[c(2,3)]
#행렬 인덱싱 사용 가능
us.state[,2,drop=FALSE]
us.state[,2]
us.state[c('state.name','state.abb')] #리스트 인덱싱
us.state[,c('state.name','state.abb')] #행렬 인덱싱

id<-c("a1","a2","a3")
price<-c(10,20,30)
name<-c("x","y","z")

product <- data.frame(id,price,name)
product
str(product)


#데이터 추가
product <- rbind(product, c('a4',40,'k'))
add <- data.frame(id=c('a4','a5','a6'),price=c(40,50,60),name=c('k','l','m'))
product <- rbind(product, add)
product$price <- as.numeric(product$price)
product <- product[-c(4:6),]
product
str(product)


#연습
#1.
lst <- list(c(3,5,7),c('A','B','C'))
lst[[2]][1] <- 'a'
lst

#2.
score <- list(math=list(95,90),eng=list(80,90),kor=list(85,80))
score
mean(c(unlist(score[[1]][1]),unlist(score[[2]][1]),unlist(score[[3]][1])))
mean(c(unlist(score[[1]][2]),unlist(score[[2]][2]),unlist(score[[3]][2])))
mean(unlist(score))

#3.
temp <- c(
  -2.4, 0.4, 5.7, 12.5, 17.8, 22.2, 24.9, 25.7, 21.2, 14.8, 7.2, 0.4)
mon <- list()
mon[month.abb] <- temp
mon
mon[temp<0]
avg.temp <- mean(unlist(mon))
avg.temp
mon[temp<avg.temp]
mon <- mon[temp<avg.temp]
mon

#4.
new.list <- list(list(a=1, b=2),pi)
new.list
new.list[[2]]

#5.
row <- c('r1','r2','r3')
col <- c('c1','c2','c3')
matrix(1:9,3,3,dimnames=list(row,col))
mat <- cbind(matrix(1:3,3),matrix(4:6,3),matrix(7:9,3))
rownames(mat) <- row
colnames(mat) <- col
mat
r1 <- seq(1,7,3)
r2 <- seq(2,8,3)
r3 <- seq(3,9,3)
mat <- rbind(r1,r2,r3)
colnames(mat) <- col
mat

v <- 1:9
dim(v) <- c(3,3)
dimnames(v) <- list(row,col)
rownames(v) <- c('r1','r2','r3')
colnames(v) <- c('c1','c2','c3')
v

#6.
nrow(v)
ncol(v)
rownames(v)
colnames(v)
length(v)

#7. nope
A <- matrix(c(1,4,1,0,1,2), 2, byrow=T)
B <- rbind(c(1,0,1),c(-1,1,-1))
A+B
A-B
B*A
B/A
A[2,] <- c(1,2,1)
t <- B[,1] 
u <- B[,2]
B[,1] <- u
B[,2] <- t
B[,c(1,2)]
B[,c(1,2)] <- B[,c(2,1)]
B
A <- A[-2,]
A
B[B<1] <- 0
B

#8.
#행렬은 2차원 배열은 다차원
array(1:18,c(3,2,3))

#9.
L <- list(ID=c(1,2,3), 
       name=c('Kim', 'Lee', 'Park'),
       score=c(80, 95, 75))
length(L) #열의 개수
L[['score']][3] <- 80
L$name=='Park' #name이 Park인 값이 true 반환
L$score[L$name=='Park'] #name이 Park인 값의 score
c(L$name[L$ID==1],L$score[L$ID==1])

#10. 
id <- c(1,2,3,4)
age <- c(21,22,31,40)
sex <- c('남','여','여','여')
height <- c(175,165,155,160)
weight <- c(80,55,45,50)
data.frame(id,age,sex,height,weight)

#11.
BTS <- c(5,4,1.5)
SON <- c(4,5,2)
Red <- c(2.5,2,1)
Twice <- c(3.5,4,5)
p <- data.frame(row.names=c('밀정','경이로운소문','국제시장'),BTS,Red,Twice)
p.son <- sqrt(colSums((p-SON)^2))
p.son
p.son[min(p.son)]

p <- rbind(BTS,SON,Red,Twice)
colnames(p) <- c('밀정','경이로운소문','국제시장')
p
p.euclid <- dist(p,method='euclidean')
p.euclid

#영화별로 정규화 작업
# 정규화 : (각각의 평점 - 최소값) / (최대값-최소값)
# 0<=정규화<=1
