setwd('C:/rwork')
smsraw <- read.csv('sms_spam_ansi.txt')
str(smsraw)
smsraw$type <- factor(smsraw$type)
table(smsraw$type)

library(stringr)
library(tm)
library(dplyr)

smsCorpus <- VCorpus(VectorSource(smsraw$text))

inspect(smsCorpus[[4]])
as.character(smsCorpus[[4]])
smsCorpus[[4]]$content

smsCorpusClean <- smsCorpus %>% 
  tm_map(content_transformer(tolower)) %>%   #소문자 변환 
  tm_map(removeNumbers) %>%                  #숫자제거
  tm_map(removeWords, words=stopwords()) %>% #불용어 제거
  tm_map(removePunctuation)
smsCorpusClean[[5]]$content

#... 없애고 띄어쓰기하는 사용자 정의 함수
replacePunctuation <- function(x){
  str_replace_all(x,'[[:punct:]]+',' ')
  #gsub('[[:punct:]]+',' ',x)
}
smsCorpusClean <- tm_map(smsCorpusClean,replacePunctuation)
#공백 여러개를 하나로 변환 stripWhitespace
smsCorpusClean <- tm_map(smsCorpusClean,stripWhitespace)

#word stemming
library(SnowballC)
wordStem(c('learn','learned','learns'))
smsCorpusClean <- tm_map(smsCorpusClean,stemDocument)
as.character(smsCorpusClean[[4]])

#각 행을 문서로 만듬
#DTM(행:문서, 열:단어가 오는 행렬 terms:중복제거한 단어들의 개수)
smsDtm <- DocumentTermMatrix(smsCorpusClean)
smsDtm
#<<DocumentTermMatrix (documents: 5559, terms: 6557)>>
#  Non-/sparse entries: 33972/52543050   #유의미한단어칸/전체칸
#Sparsity           : 100%       
#Maximal term length: 45                  가장 긴 단어
#Weighting          : term frequency (tf)

#===문서간 유사도=== 
#TF-IDF 알고리즘 
inspect(smsDtm[1:3,50:60])

# 코퍼스
# 문서1: 먹고 싶은 사과
# 문서2: 노란 바나나
# 문서3: 길고 노란 바나나 바나나
# 문서4: 저는 과일이 좋아요
# 
# DTM(4*9)
#      과일이 길고 노란 먹고 바나나 사과 싶은 저는 좋아요
# 문서1   0     0    0    1     0     1    1    0    0 
# 문서2   0     0    1    0     1     0    0    0    0  
# 문서3   0     1    1    0     2     0    0    0    0 
# 문서4   1     0    0    0     0     0    0    1    1 
# 
# 유클리디안 거리로 knn가능
# 
# 
# #코사인 유사도
# cos(d1,d2)=(d1*d2)/|d1|*|d2| -> 벡터의 크기
# 
# tf(d,t) : 문서 d 에서 t단어가 나온 횟수
# df(t) : 단어 t가 등장한 문서의 수
# 높으면 단어의 중요도가 떨어질 것.
# idf(d,t): df(t)에 반비례하는 수
# 자연로그(e=2.718...)
# idf(d,t)=ln(n/(1+df(t)))
# n:총 문서의 개수(문서1~4번까지, 4)
# 로그를 사용해서 가중치 격차 줄임
# 
# tf-idf=tf*idf
# idf가 클수록 특정 문서에만 자주 등장, 중요도가 높음
# the의 tfidf는 다른 단어의 tfidf보다 낮음
# 1) 특정 단어를 가장 중요하게 다루는 문서를 알 수 있음
# 2) 각 문서들마다 중요한 단어 알 수 있음
# 
# 활용
# 1) cos 유사도 구할 수 있음



#랜덤하게 분할
smsDtmTrain <- smsDtm[1:4169,]
smsDtmTest <- smsDtm[4170:5559,]


smsTrainLabels <- smsraw[1:4169,]$type
smsTestLabels <- smsraw[4170:5559,]$type

prop.table(table(smsTrainLabels))
prop.table(table(smsTestLabels))

#0인 값이 많으면 데이터 차지하므로 제거
smsDtmTrain
#removeSparseTerms(): 잘 안나오는? term 제거
smsDtmFreqTrain <- removeSparseTerms(smsDtmTrain,0.999)
smsDtmFreqTrain
#<<DocumentTermMatrix (documents: 4169, terms: 1102)>>
#  Non-/sparse entries: 24817/4569421   #유의미한단어칸/전체칸
#Sparsity           : 99%       
#Maximal term length: 19                  가장 긴 단어
#Weighting          : term frequency (tf)

#최소 n번 이상 m번 이하 등장한 단어
findFreqTerms(smsDtmTrain,n,m)
smsFreqWords <- findFreqTerms(smsDtmTrain,5)
str(smsFreqWords)

smsDtmFreqTrain <- smsDtmTrain[,smsFreqWords] #4169*1137
smsDtmFreqTest <- smsDtmTest[,smsFreqWords] #1390*1137

#1137개 단어에 대하여 등장한 경우/아닌경우로 나누어 구분하여 저장
#0이 아니면 yes, 0이면 no

convertCounts <- function(x){
  x <- ifelse(x>0,'Yes','No')
}
#열단위로
smsTrain <- apply(smsDtmFreqTrain, MARGIN=2, convertCounts)

smsTest <- apply(smsDtmFreqTest, MARGIN=2, convertCounts)

str(smsTrain)
smsTrain[1]
smsTrain[4169]


#원으로 단어들 시각화
install.packages('wordcloud')
library(wordcloud)     #최소 언급 횟수, 클수록 가운대로
wordcloud(smsCorpusClean, min.freq=50, random.order=F)

spam <- subset(smsraw, type=='spam')
ham <- subset(smsraw, type=='ham')
                     #최대 표시단어 개수
wordcloud(spam$text, max.words=40, colors=brewer.pal(5,'Greens')) #색단계, 색상



#나이브베이즈 패키지
install.packages('e1071')
library(e1071)

#나이브베이즈 모델 생성
naiveBayes(데이터, 답)
smsClassifier <- naiveBayes(smsTrain, smsTrainLabels, laplace=0) #default

#테스트데이터 -> 모델 -> 예측(분류) 결과
smsTestPred <- predict(smsClassifier, smsTest)

smsTestPred #예측결과
smsTestLabels #정답

library(gmodels)
CrossTable(smsTestPred,smsTestLabels)

#라플라스값 1로 모델 생성
smsClassifie2r <- naiveBayes(smsTrain, smsTrainLabels, laplace=1)

#테스트데이터 -> 모델 -> 예측(분류) 결과
smsTestPred2 <- predict(smsClassifier2, smsTest)

smsTestPred2 #예측결과
smsTestLabels #정답

CrossTable(smsTestPred2,smsTestLabels)



#=========연습===========

email <- read.csv('email_title.txt',header=F)
email$V1 <- factor(email$V1)
str(email)
table(email$V1)

library(tm)
library(stringr)

emailCorpus <- VCorpus(VectorSource(email$V2))


emailCor <- emailCorpus %>% 
  tm_map(removeNumbers) %>% #숫자제거
  tm_map(removeWords, stopwords())  #불용어제거


rePunc <- function(x){str_replace_all(x,'[[:punct:]]+',' ')}

emailCor <- emailCor %>% 
  tm_map(content_transformer(rePunc)) %>%  #구두점 제거
  tm_map(removeWords, '[[:alpha:]]{1}') %>% #한글자 제거
  tm_map(stripWhitespace) %>%  #중복 띄어쓰기 제거
  tm_map(content_transformer(tolower)) #소문자 변환

lapply(emailCor,as.character)

#DTM
emailDtm <- DocumentTermMatrix(emailCor)
emailDtm #단어 종류 151개,  95% sparsity

set.seed(2021)
idx <- sample(1:22,22*0.7)
emailDtmTrain <- emailDtm[idx,]
emailDtmTest <- emailDtm[-idx,]

emailTrainLabels <- email[idx,]$V1
emailTestLabels <- email[-idx,]$V1

#0인 데이터 추려내기 findFreqTerms

#151개 단어에 대해서 있으면 yes 없으면 no
convertCounts <- function(x){
  x <- ifelse(x>0,'Yes','No')
}

emailTrain <- apply(emailDtmTrain, 2, convertCounts)
emailTest <- apply(emailDtmTest, 2, convertCounts)

emailTrain[1003]

#베이즈 모델
library(e1071)

Classifier <- naiveBayes(emailTrain,emailTrainLabels)
emailTestPred <- predict(Classifier,emailTest)

library(gmodels)
CrossTable(emailTestPred,emailTestLabels)
sum(emailTestPred==emailTestLabels)/length(emailTestPred) #0.86

