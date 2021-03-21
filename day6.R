#문자열 변경

#grep: fixed=F (pattern을 정규표현식으로 인식), fixed=T (patternm을 문자열로 취급)

words <- c('at','bat','cat','cheap','check','cheese','chick','hat','chase','chaselle')
grep('che',words)
grep('ch|at',words,value=T)
grep('[ch]',words,value=T) #한글자씩
grep('ch[ei]ck',words,value=T)
grep('ch(e|i)ck',words,value=T)
#? ?앞의 문자가 0번 또는 1번
#* *앞의 문자가 0번 이상 반복
#+ +앞의 문자가 1번 이상 반복
grep('chas?e',words,value=T)
grep('chas*e',words,value=T)
grep('ch(a?e*)se$',words,value=T)
grep('^a',words,value=T)
grep('t$',words,value=T)
#.* : 모든 문자가 0개 이상
grep('^c.*t$',words,value=T)
grep('^[ch]?at',words,value=T)

words2 <- c("12 Dec", "OK", "http://", "<TITLE>Time?</TITLE>","12345", "Hi there")
#[[:alnum:]],\\w : 알파벳+숫자
#[[:digit:]], \\d : 숫자
#[[:punct:]] : 문장부호, 특수문자
#[[:space:]] : 공백
grep('[[:alnum:]]',words2,value=T)
grep('\\w',words2,value=T)
grep('[[:alpha:]]',words2,value=T)
grep('[[:digit:]]',words2,value=T)
grep('\\d',words2,value=T)
grep('[[:punct:]]',words2,value=T)


Sys.Date() #현재날짜
date() #현재 날짜 및 시간
Sys.time()
class(Sys.Date())
as.Date('2021-02-26')
as.Date('02/26/21', format='%m/%d/%y')
d <- as.Date('2021-02-26')
format(d,format='%m/%d/%Y')
today <- Sys.Date()
format(today, '%Y/%m/%d %A')


weekdays(as.Date('2021-02-27'))

d <- as.Date('2021-02-26')
d-1
d+1:10
weekdays(d+1:10)

#seq()
s <- as.Date('2021-02-26')
e <- as.Date('2021-04-01')
seq(s,e,5)
seq(s,by=2,length.out=5)
seq(s,by='5 years',length.out=5)
seq(s,by='2 weeks',length.out=5)
seq(s,by='2 months',length.out=5)
seq(as.Date('2021-01-30'),by='month',length.out=3)
qrt <- seq(s,by='3 months',length.out=4)
months(qrt)
quarters(qrt)


#입력
product <- data.frame()
edit(product)
fix(product)
str(product)

#저장
write.csv(product,file='myproduct.csv',row.names = F)
p <- readClipboard()
p

read.table(file='clipboard', sep='\t')
read.table(file='clipboard', sep='\t',header=T)

#파일 불러오기

read.csv('product-with-no-header.csv',header=F)
read.csv('product.csv', stringsAsFactors=T)

read.csv('product.csv',as.is=T) #as.iS=T는 stringAsFactors=F와 같음


#table형식 : 구분자로 자료가 구분되어져 있고, 데이터 열의 개수가 같음
read.table('product.txt',header=T)

read.table('product-colon.txt',header=T,sep=':')

#NA : 데이터 누락(결측값)
read.table('product-missing.txt',header=T, na.strings=c('.','idk'))

read.table('product-comment.txt',)

read.csv('brand-eval.csv')
brand <- read.table('brand-eval.csv',header=T,sep=',',row.names='id')
#id열 표기가 사라져버리네?
brand

#자료 타입 지정
brand <- read.table('brand-eval.csv',header=T,sep=',',row.names='id',
                    colClasses=c('character','character','numeric','numeric','numeric'))
brand

#엑셀 데이터 읽기
install.packages('openxlsx')
library(openxlsx)
read.xlsx('myproduct.xlsx')

library(ggplot2)
str(diamonds)
head(diamonds)
diamonds.new <- subset(diamonds,carat>=2&cut=='Premium')
write.csv(diamonds.new,'shiny_diamonds.csv',row.names=F)
diamonds.load <- read.csv('shiny_diamonds.csv')
diamonds.new <- subset(diamonds.load,color!='D')
write.xlsx(diamonds.new,'shiny_diamonds.xlsx',row.names=F)

#url로 데이터 불러오기
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris.uci <- read.csv(url,header=F)

#파일 다운로드
download.file(url=url, destfile='myIris.csv')
download.file(url,'myiiris.csv')
baseball <- 'http://seanlahman.com/files/database/baseballdatabank-master_2016-03-02.zip'
download.file(baseball,'baseball.zip')
read.csv(unzip('baseball.zip','baseballdatabank-master/core/Salaries.csv'))


#연습

#1.
test <- c(80,60,70,50,90)
test

#2.
mean(test)
#3.
test.avg <- mean(test)
test.avg

#4.
r1 <- c('제품','가격','판매량')
v1 <- c('사과','딸기','수박')
v2 <- c(1800,1500,3000)
v3 <- c(24,38,13)
df <- data.frame(v1,v2,v3)
colnames(df) <- r1

#5.
mean(df[,2])
mean(df[,3])
apply(df[2:3],2,mean)

#6.
install.packages('ggplot2')
library(ggplot2)
#tibble은 dataframe같은건데 좀더 설명 붙음
df <- as.data.frame(mpg)
df <- rename(df,city=cty,highway=hwy)
mean(df$highway[df$displ<=4])
mean(df$highway[df$displ>=5])
mean(df$city[df$manufacturer=='audi'])
mean(df$city[df$manufacturer=='toyota'])
max(c(mean(df$city[df$class=='suv']),mean(df$city[df$class=='compact'])))

#7.
midwest
midwest <- as.data.frame(midwest)
str(midwest)
class(midwest)
dim(midwest)
head(midwest)
tail(midwest)
summary(midwest)
midwest <- rename(midwest,total=poptotal,asian=popasian)
within(midwest,{
  percasian.eval=ifelse(percasian>mean(percasian),
                   'large',
                   'small')
  })


prop<-midwest$asian/midwest$total
meanProp<-mean(prop)
prop>meanProp

midwest$size<-c('large')
midwest$size[prop<=meanProp]<-'small'
midwest$size

#변수명 바꾸기
install.packages('dplyr')
library(dplyr)
#rename(데이터프레임명, 변경후이름=변경전이름)



#8.
read.table('Hamlet.txt')
txt <- readLines('Hamlet.txt')
grep('^\\*{6}.*\\*{6}$',txt,value=T)
words <- unlist(strsplit(txt,split=' '))
up.words <- grep('^[[:upper:]]',words,value=T)
up.words <- gsub('[^[:alpha:]]','',up.words)
table(up.words)
bracs <- grep('^\\[.*\\]$',txt,value=T)

lo.words <- tolower(unlist(strsplit(txt,split=' ')))
lo.words <- gsub('[^[:alpha:]]','',lo.words)
lo.words <- lo.words[lo.words!='']
sort(c(table(lo.words)),decreasing=T)[1:50]

gsub('Ham.','Hamlet.',txt)

grep('^Scene',txt,value=T)

# ??? 문제 3. 대괄호([ 또는 ])로 묶여있는 문자열 출력
charr<-grep("[[].*[]]",haml,value=T)
gsub('.*[[]|[]].*',replacement='',charr)
# ??? 문제 4. 모든 단어를 소문자로 변환 -> 가장 빈도수가 높은 단어 50개 출력
lowers<-tolower(haml1)
lowers<-gsub('[^[:alpha:]]',replacement = '',lowers)
lowers<-lowers[lowers!=""]
sort(c(table(lowers)),decreasing = T)[1:50]

# ??? 문제 5. Ham. ->Hamlet. 으로 치환
repla<-gsub('Ham[.]',replacement = 'Hamlet.',haml1)
grep('Ham[.]',repla,value = T)
grep('Hamlet[.]',repla,value = T)

# ??? 문제 6. Scene 으로 시작되는 문자열 출력
grep('^(Scene)',haml,value = T)

#9.
format(seq(as.Date('2021-02-27'),by=1,length.out=7),"%a-%m%d")


iris
str(iris)
levels(iris[,5])
nrow(sleep);ncol(sleep)

iris[,'Species'] 
iris[,5] 
iris[5] 
iris['Species']
iris$Species
