sort() #정렬. 데이터프레임 사용 x
order() #정렬 색인 값 추출. 데이터프레임 정렬에서 사용

v1 <- c(20,60,10,10,15)
v2 <- c(300,200,100,700,600)
v3 <- c('a','b','c','d','f')
df <- data.frame(v1,v2,v3)
sort(v1)
sort(v1,decreasing=T)
v1
df
order(v1)
df[order(v1),]
order(-v1) #내림차순
df[order(-v2,v1,v3),]

install.packages('plyr')
library(plyr)
arrange(df,v1,desc(v2),v3)



#text mining
#베이즈 이메일 분류기 (Bayesian filter) 이진 추론. 성능 좋지 않음

#인공지능 언어모델 bert가 나오면서 다 사라짐

x <- 'we have a dream'
x
nchar(x) #공백문자 포함
length(x) #문자 벡터의 벡터 길이(원소 개수가 1개)

y <- c('we','have','a','dream')
y
length(y)
nchar(y)
nchar(y[4])

#대소문자 변환
tolower(x)
toupper(x)

#분리, 합치기
strsplit(x,split=' ')
class(strsplit(x,split=' ')) #리스트임

unlist(strsplit(x,split=' ')) #벡터로 만들기
res <- strsplit(x,split=' ')[[1]] #벡터로 만들기
res[4]

x1 <- 'We have a dream'
x2 <- 'how are you'
x3 <- c('i am fine','do you wanna build a snowman. it doesnt have to be a snowman. okay bye')
myword <- c(x1,x2,x3)
myword
strsplit(myword," ")

#대소문자 구분됨
said <- 'WHAT IS ESSENTIAL is invisible to the Eye'
said.word <- strsplit(said,' ')
#유일한 단어 추출
unique(said.word[[1]]) #벡터만 가능
unique(tolower(said.word[[1]]))
unique(c('go','fetch','go','now'))

자연어 <- sql
#회사에서 직급이 대리인 사람의 이름 모두 출력
#SELECT name FROM company WHERE 직급='대리'

#paste() : 분리된 벡터 결합. 벡터의 원소들을 분리한 다음 결합하는게 아님
#기본값으로 공백문자 들어감
paste('everybody','wants','to','fly')
paste('everybody','wants','to','fly',sep='-')
paste('everybody','wants','to','fly',sep='')
paste(pi, sqrt(pi))
paste('aaa',1+2,'b')
paste(c('everybody','wants','to','fly'),c('everybody','wants','to','touchy'))

said
res <- strsplit(said,' ')

heroes <- c('batman','captain america','hulk')
colors <- c('black','blue','green')
paste(colors,heroes)
paste('type',1:5)

#paste함수 collapse 옵션 :  텍스트 결합으로 생성된 텍스트들 연결하는 구분자 정의
paste(c('Everybody','wants','to','fly'),collapse=' ')

paste(heroes,c('wants','to','fly'),collapse=', and ')
paste(heroes,'wants','to','fly',collapse=';',sep='-')

#outer: 두 집합에 대해 가능한 모든 순서쌍의 곱 수행 (카티전곱)
outer(c(1,2,3),c(3,2,1))

#outer+paste 로 문자열 생성
#outer 함수 3번째 자리에 함수를 작성하여 
#모든 순서쌍에 대해 카티전 곱이 아닌 다른 작업 수행
asia.countries <- c('korea','china','japan')
info <- c('gdp','pop','area')
out <- outer(asia.countries, info, FUN=paste,sep='-')
out
as.vector(out)
res <- outer(asia.countries, asia.countries, FUN=paste, sep="-")
res

lower.tri(res) #하삼각행렬
res[lower.tri(res)]
!lower.tri(res)
res[!lower.tri(res)]

#텍스트의 특정 부분 문자열 추출
substr('data analytics',1,4)

substring() #끝위치 생략
substring('data analytics',6)

#텍스트 벡터가 인수인 경우 각 벡터의 원소별로 각각 추출
myclass <- c('data analytics','data mining','data visualization')
substr(myclass, 1,4)
substring(myclass, 6)
substr(myclass, nchar(myclass)-5,nchar(myclass)) #뒤의 다섯글자(length가 아니라 nchar)


islands
class(islands)
landmass <- names(islands)
grep(pattern='New',x=landmass)

landmass[grep(pattern='New',x=landmass)]
grep(pattern='New',x=landmass,value=T)

#두 개 이상의 단어로 이루어진 대륙 또는 섬을 출력
grep(' ', landmass,value=T)
landmass[grep(' ', landmass)]

#sub(),gsub() : 문자열 검색해서 다른 문자열로 변경
#sub() : 처음 1개만
#gsub() : 모든 문자열
txt <- 'Data Anal is useful. Data Anal is interesting.'
sub(pattern='Data',replacement = 'Business', txt)
gsub(pattern='Data',replacement = 'Business', txt)

#파일명만 출력
x <- c('input.csv','data.csv','big.csv')
gsub('.csv','',x)

#파일 불러오기
df <- read.csv('samsung.csv')
df

#연습
#1.
text <- c('Happy','Birthday','to','you')
length(text)
sum(nchar(text))

#2.
one <- paste(text,collapse=' ')
length(one)
nchar(one)

#3.
"A 1" "B 2" "C 3" "D 4" "E 5"
paste(LETTERS[1:5],1:5)

"A1" "B2" "C3" "D4" "E5"
paste(LETTERS[1:5],1:5,sep='')



#4.
sen <- c("Yesterday is history,", "That's, why we call it the present - from Kung")
s <- gsub('- |,','',sen)
s
unlist(strsplit(s,' '))


#5.후순
num <- c("110101-1234123","950102-2121212")
substring(num,8) <- '********'
num

pattern <- substr(num,nchar(num)-6,nchar(num))
pattern
num[7:14]
num[nchar(num)-7:nchar(num)]
a <- gsub(pattern[1],'*******',num)
a
b <- gsub(pattern[2],'*******',a)
b

#6.
paste(month.abb,1:12,sep='_',collapse='-')

#7.
df <- read.csv('samsung.csv')
df
df[,-1]
df.min
df.min <- apply(df[,-1],2,min)
df.max <- apply(df[,-1],2,max)
df.mean <- apply(df[,-1],2,mean)
df.sd <- apply(df[,-1],2,sd)
df.std <- apply(df[,-1],2,scale)
df.std
df.norm <- apply(df[,-1],2,(df[,-1]-df.min)/(df.max-df.min))
summary(df)
df.nor <- apply(df[,-1],2,function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))})
df.nor
scale(df[-1])

df.high <- df[,2]
df.high
df.high.diff <- abs(df.high[1:248]-df.high[2:249])
df.high.diff
df.high.max <- max(df.high.diff)
df.high.max
which(df.high.diff==df.high.max)


df.low <- df[,3]
df.low.diff <- abs(df.low[1:248]-df.low[2:249])
df.low.diff
df.low.max <- max(df.low.diff)
df.low.max
which(df.low.diff==df.low.max)
df[215:218,]
df[217,1]
