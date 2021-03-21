메일 : free => spam?
  
스팸 확률 0.3
free 포함 확률 0.4
스팸 중 free 포함 확률 2/3

베이지안 확률
P(spam|free)=P(free|spam)*P(spam)/P(free)
2/3 * 0.3 /0.4 = 0.5

1) 나이트=예, 돈=아니오, 식료퓸=아니오, 구독취소=예인 경우 스팸일 확률
P(스팸|나이트,-돈,-식료품,구독쥐소)
메모리 너무 커서 어려움

전부 조건부 독립으로 가정하고

분자 = P(스팸)*P(나이트,-돈,-식료품,구독취소|스팸)
  = P(스팸)*P(나이트|스팸)*P(-돈|스팸)*P(-식료품|스팸)*P(구독취소|스팸)
분모 = 무시(상수취급)

2) 같은 조건에 햄일 확률

우도가 0이 되지 않기 위해
라플라스값 =1 인 우도값=분자에 1씩더하고 더하게 된 값을 분모에 추가

분류기준은 범주형이어야함

smsraw <- read.csv('sms_spam_ansi.txt')
str(smsraw)
smsraw$type <- factor(smsraw$type)

#텍스트 마이닝 패키지
install.packages('tm')
library(tm)
corpus라고도 불림(말뭉치)
큰 문자데이터 다룰때 쓰임
함수의 데이터 형식은 
source형식(말뭉치 만들려는 대상 데이터가 어떤 형태로있는지 나타내는것)
DirSource, VectorSource, DataframeSource 등

#words <- system.file('texts')
#VCorpus(DirSource(words, mode='binary'),
#        readerControl = list(reader=...))

library(stringr)
mytext <- c('software environment','software  environment','software\tenvironment')
str_split(mytext, ' ') #띄어쓰기로 나누기
sapply(str_split(mytext, ' '),length) #단어 수
sapply(str_split(mytext, ' '),str_length) #각 단어의 글자수

#전체 문자열 다른 문자로 바꾸기 (gsub)
mytext.nowhitespace <- str_replace_all(mytext,'[[:space:]]{1,}',' ')
mytext.nowhitespace
sapply(str_split(mytext.nowhitespace, ' '),length) #단어 수
sapply(str_split(mytext.nowhitespace, ' '),str_length) #각 단어의 글자수

mytext <- "The 45th President of the United States, Donald Trump, 
states that he knows how to play trump with the former president"
#단어단위로 분리
myword <- unlist(str_extract_all(mytext, boundary('word'))) #리스트로 출력
myword
table(tolower(myword))
myword<-str_replace(myword, "Trump", "Trump_unique_")
myword<-str_replace(myword, "States", "States_unique_")
myword

mytext <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
#숫자 제거
mytext2 <- str_replace_all(mytext, '[[:digit:]]{1,}[[:space:]]{1,}','')
#자르고 연결하기
mytext2 <- str_split(mytext2,' ')
str_c(mytext2[[1]],collapse=' ') 
mytext3 <- str_replace_all(mytext, '[[:digit:]]{1,}[[:space:]]{1,}','_number_')
mytext3 <- str_split(mytext3,' ')
str_c(mytext3[[1]],collapse=' ') 

#'. '기준으로 분리
mytext <- "Kim et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the Internet."
str_split(mytext,' ')
str_split(mytext,'\\. ')

mytext2 <- str_replace_all(mytext,'-',' ') #정규표현식도 가능
mytext2

#불용어(stopwords): , a, an, the 빈번하게 사용되나 의미 없는 단어들
#불용어 사진 설정 및 제거
mytext <- c('She is an actor','She is the actor')
mystopwords <- '(\\ba )|(\\ban )|(\\bthe )'
str_remove_all(mytext,mystopwords)
#기본 stopwords 이미 있음
stopwords('en') #영어 stopwords 모음
stopwords('SMART')
#stopwords에서 뽑아서 사용

#어근 동일화 작업
various_be <- '(\\b(a|A)m)|(\\b(a|A)re)|(\\b(i|I)s)|(\\b(w|W)as)|(\\b(w|W)ere)'
mystemmer.func <- function(myt){
  mytext <- str_replace_all(myt,various_be,'be')
  print(mytext)
}
mytext <- c('I am a boy. You are a boy. The person might be a boy. Is Jane a boy?')
mystemmer.func(mytext)

mytext <- "The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
myword <- unlist(str_extract_all(mytext,boundary('word')))
table(myword) #각 단어 개수
length(table(myword)) #종류개수
sum(table(myword)) #전체 단어수
mytext.2gram <- str_replace_all(mytext,'\\bUnited States','United_States')
#     the 관사까지 포함하려면 '\\b(t|T)he United States','The_United_States'
mytext.2gram
mytext2 <- unlist(str_extract_all(mytext.2gram,boundary('word')))
table(mytext2)

#말뭉치 : VCorpus함수로
my.text.location <- 'ymbaek_papers'
mypaper <- VCorpus(DirSource(my.text.location)) #폴더 통으로 줄때
mypaper
summary(mypaper)

mypaper[[2]]
mypaper[[2]]$content #내용 확인
mypaper[[2]]$meta #메타정보?
#meta 정보 수정
meta(mypaper[[2]], tag='author') <- 'Y. M. Baek'
meta(mypaper[[2]], tag='author')
meta(mypaper[[2]])
class(mypaper)
#단어+특수문자+단어 패턴을 추출
myfunc <- function(x){
  str_extract_all(x$content,'[[:alnum:]]{1,}[[:punct:]]{1}?[[:alnum:]]{1,}') 
  #x$content = mypaper안의 모든 컨텐트 
  #extract함수 boundary에 '정규표현식'가능
}
mypuncts <- lapply(mypaper,myfunc)
table(unlist(mypuncts))

#대문자로 시작하는 단어들의 빈도수
upperx <- function(x){
  str_extract_all(x$content,'[A-Z]{1}[[:alnum:]]{1,}')
  #str_extract_all(mypaper$content,'[[:upper:]]{1}[[:alnum:]]{1,}')
}
table(unlist(lapply(mypaper,extractx)))

#숫자로만 이루어진 단어들의 빈도수
numx <- function(x){
  str_extract_all(x$content,'\\d{1,}')
}
mydigits <- lapply(mypaper,numx)
table(unlist(mydigits))

smsraw <- read.csv('sms_spam_ansi.txt')
str(smsraw)
smsraw$type <- factor(smsraw$type)
smsraw
class(smsraw)
#smsraw로 코퍼스 생성 => text열 vector형식이므로 vectorsource
smsCorpus <- VCorpus(VectorSource(smsraw$text))
smsCorpus[[1]]$content

inspect(smsCorpus[1:5])
inspect(smsCorpus[[1]])
lapply(smsCorpus[1:3],as.character)

#tm_map(코퍼스에 대해 전처리(변환작업)를 수행하는 함수)
#소문자 변환
smsCorpusClean <- tm_map(smsCorpus,content_transformer(tolower))
smsCorpusClean[[1]]$content
lapply(smsCorpusClean[1:3],as.character)

#tm_map에 활용가능한 함수들 알아보기
tm_map(smsCorpus,removeNumbers) #숫자 모두 제거
tm_map(smsCorpus,content_transformer(removeNumbers)) #숫자 모두 제거
res <- tm_map(smsCorpus,tolower)
res[[5]]

#단독사용 가능한 함수 5개
removeNumbers, 
removePunctuation, 
removeWords, 
stemDocument,
stripWhitespace
#그 외(사용자함수 포함)는 content_transformer()안에


#특수문자 제거
smsCorpusClean <- tm_map(smsCorpusClean,removePunctuation)
smsCorpusClean[[4]]$content

#stopwords 제거
stopwords()
smsCorpusClean <- tm_map(smsCorpusClean,removeWords, stopwords())
smsCorpusClean[[4]]$content

removePunctuation('helloo...RP')
remPunc <- function(x){  #살짝 변환시킨
  gsub('[[:punct:]]+',' ',x)
}
remPunc('hello...RP')

#word stemming 어근추출할때 많이 씀
install.packages('SnowballC')
library(SnowballC)
wordStem(c('run','runs','running'))
wordstem(c('name','named','naming'))


#연습
smsraw <- read.csv('sms_spam_ansi.txt')
smsraw$type <- factor(smsraw$type)
class(smsraw)
str(smsraw)

#smsraw로 코퍼스 생성
library(stringr)
library(tm)
smsCorpus <- VCorpus(VectorSource(smsraw$text))

#첫번째 메일내용 프린트하기(inspect=corpus유지, as.character=문자벡터)
inspect(smsCorpus[[4]])
as.character(smsCorpus[[4]])

#1. 수치데이터 추출
digits <- function(x){
  str_extract_all(x,'\\d{1,}') 
}
smsdigits <- unlist(lapply(smsCorpus,digits))
smsdigits

#1-2 수치데이터 제거
smsCorpusNoNum <- tm_map(smsCorpus,removeNumbers)
inspect(smsCorpusNoNum[[4]])

#2 불용어 제거
stopwords()
smsCorpusNoStopwords <- tm_map(smsCorpus,removeWords, stopwords())
inspect(smsCorpusNoStopwords[[4]])

#3. 대소문자 통일
smsCorpusLower <- tm_map(smsCorpus, content_transformer(tolower))
inspect(smsCorpusLower[[4]])

#4. 특수문자 제거
smsCorpusNoPunct <- tm_map(smsCorpus, removePunctuation)
inspect(smsCorpusNoPunct[[4]])

#5.단어 길이 2미만 제거
smsCorpusRemove <- tm_map(smsCorpus,removeWords,'[[:alpha:]]{1}')
inspect(smsCorpusRemove[[12]])


#6. 가장 많이 나온 단어
#소문자로 통일하고 숫자 제거 불용어 제거 후 단어만 출력
smsCorpusClean <- tm_map(smsCorpus,content_transformer(tolower))
smsCorpusClean <- tm_map(smsCorpusClean,removeNumbers)
stopwords('SMART')
smsCorpusClean <- tm_map(smsCorpusClean,removeWords,stopwords('SMART'))
#smsCorpusClean <- tm_map(smsCorpusClean,str_replace_all,'[^[:alpha:]]{1,}',' ')
myfunc <- function(x){
  str_extract_all(x,'[[:alpha:]]{1,}')
}
sort(table(unlist(lapply(smsCorpusClean,myfunc))),decreasing=T)[1:10]
