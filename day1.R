#벡터, 행렬, 데이터프레임, 리스트, 텐서 등 제공됨
#벡터 : 데이터 분석에서 자료의 기본형
#스칼라 : 길이가 1인 벡터
#N차원 벡터 : 데이터의 개수가 n개인 하나의 벡터
#행렬 :  벡터의 집합
#피쳐 : 벡터의 각 변수들
#팩터 : 문자형 데이터가 저장되는 벡터의 일종, 문자값들이 어떠한 종류를 나타내는 값(성별, 혈액형형 등)일 때 사용

#텐서 : 

#키, 몸무게, 혈액형, 나이 (4차원 벡터)
#(170, 60, A, 30) 순서 중요

num<-c(1,2,3)
cha<-c("x","y","z")
c(num,cha)

#str함수: 벡터의 유형 및 구조 확인
str(num)
str(cha)

da <- c(3:10)
length(da)
a<-3
print(a)
a
b<--2
b

c(1,2,3)
c("we","love","data")
odd<-c(1,3,5)
even<-c(2,4,6)
c(odd,even)
3:9
5:1

seq(from=3, to=9, by=0.5)
seq(3,9,2)
seq(from=3, to=9, length.out=5)
seq(3,9,length.out=5)
rep(1,times=3)
rep(c(1,2,3), times=5)
rep(c(1,2,3), each=5)
rep(1:3, length.out=8)

plot(1:10)
3+(4*5)

#상수벡터
LETTERS
letters
month.abb
month.name
pi

m<-c(12, 9 ,3, 5, 1)
m #5개의 요소로 이루어진 1차원 벡터
month.name
month.name[2]
#벡터의 요소들을 여러개 참조하려면 c함수로 하나의 벡터로 만들어야함 
month.name[month]

1+2 #1이라는 벡터와 2라는 벡터에 +연산을 수행한 결과.
#벡터화 연산 ==> 속도가 빠르다, 원소끼리 연산수행
c(1,2,3) + c(4,5)
2*c(2,4,5)
y<-c(2,4,6)
y*c(3)

#행벡터 = 행 단위의 데이터
#열백터 = 열 단위의 데이터

x<-c(1:3)
y<-c(4:9)
x+y
#길이가 짧은쪽의 벡터가 긴쪽에 맞춰서 반복됨
c(1,3,5)+10

#!x, ==, !=, x|y, x&y 
v <- pi
w <- 10/3
v == w
v != w
(v == w) | (v<w)
isFALSE(v==w)
x <- c(1,3,5)
y <- c(2,3,5)
x==y

as.numeric(TRUE)
TRUE+TRUE

x <- c(0, 25,50,100,200)
x>50
sum(1:5)
sum(1:2, 3:5)
sum(TRUE, FALSE, TRUE)
sum(x>50)

a <- -3:3
any(a>0)
all(a>0)

#2의 제곱근의 제곱 => r에서는 2가 안나옴. 부동소수점수 연산 수행해서
sqrt(2)^2==2
#수치 비교시 정밀도 문제를 엄밀하게 하려면 all.equal()함수 이용
all.equal(sqrt(2)^2,2)

fruits <- c("gum","gu","bar")
food <- c("pie","juice","cake")
paste(fruits,food)
paste(fruits, "yay")


x <- -3:3
abs(x)

#로그(디폴트로 밑수가 e(2.718..)임)
log(2:5)
log(2:5, base=2)
log2(2:5)

#exp함수: 밑수가 e인 지수값
y <- exp(1:5)
log(y)

factorial(1:5)
choose(5,2) #5개 중 2개를 선택하는 경우의 수 5C2
#nCr = n! / r!(n-r)!

#유효자릿수 확인
options("digits")
sqrt(1:5)
#유효자릿수 지정
signif(6.434, digits=3)
signif(456.434, digits=2)
signif(456.439, digits=5)
round(456.434, digits=2)
round(456.434, digits=3)


ans <- sqrt(1:10)
round(ans, digits=2)
round(sqrt(1:10), 2)

# 반올림 숫자가 5인 경우 가까운 짝수로 반올림
round(11.5)
round(12.5)
# floor : 내림
# ceiling : 올림
# trunc : 버림

floor(3.52)
ceiling(3.62)
trunc(3.72)

3/0
is.infinite(3/0)
#r은 1.8*10의 308승까지 표현 가능

z <- c(1,2,3,4,NA)
sum(z)
range(z)
median(z)
sum(z, na.rm=TRUE)
range(z)
sum(na.omit(z))

traffic.death <- c(10,20,30,20)
cumsum(traffic.death)
diff(traffic.death)
1:10
diff(1:10, lag=3)

p <- 1:10
q <- 6:15
union(p,q)
intersect(p,q)
setdiff(p,q)
is.element(1,p)

n <- 0:30
n[4]
n[5:20]
n[c(2,5,10)]
prime <- c(2,3,5,7,11,13,17)
idx <- c(3,4,5)
prime[idx]
prime[3:5]
prime[-3]
prime[-2:-4]
prime[-(2:4)]
#마지막꺼 빼기
prime[-length(prime)]
#값 변경
prime[c(3,4)] <- c(30,40)
prime

absent <- c(8,2,0,4,1)
names(absent)
names(absent) <- c('mon','tue','wed','thu','fri')
absent['mon']


#연습

#표준화 = 각 data - 평균 / 표준편차

seq(1,10)
seq(10,1,-1)
c(1:10,9:1)
signif(pi, digits=5)
round(pi, digits=6)
ceiling(pi)
floor(pi)
trunc(pi)

height <- c(180,160,165,185,155)
weight <- c(75,70,60,100,65)
bmi <- weight/((height/100)^2)
weight[bmi>25]

sum(1:100%%7==0)
x <- 1:10
x-mean(x)/sd(x)

