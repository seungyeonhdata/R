#연관집합
#거래데이터는 희소행렬(몇개 없는거) sparse matrix(기억장소 낭비임)
#물품종류 169개
#1번거래아이템: a,b,c
#2번: a,b,t
#3번: b,c
#4번: g
#이런 식으로 저장되어있음

df <- read.table('groceries.csv',header=F)
str(df)
head(df)
#이렇게 하면 희소행렬때문에 빈열이 많이 생김 
#이런거 전용 패키지 이용

install.packages('arules')
library(arules)
groceries <- read.transactions('groceries.csv',sep=',')
summary(groceries)
class(groceries)

#데이터 보는 함수 inspect {}형식
inspect(groceries[1:10]) #트렌잭션 단위로 구매아이템 확인

#알파벳 순서로 각 단어 비율 = 지지도
itemFrequency(groceries[,])

#====시각화====
dev.off()
itemFrequencyPlot(groceries,support=0.1) #최소지지도 0.1
#열명중에 한명은 이 물건들은 산것
#상위 물품 개수 적절하게 적정한 지지도 설정해야함

#지지도 높은 25가지 품목 출력
itemFrequencyPlot(groceries,topN=25)

#데이터 분포확인 #행렬다이어그램
image(groceries[1:10])
#x축 아이템 번호는 알파벳순으로 나열한것

#랜덤하게 100개 상품 추출하여 행렬다이어그램 형식
image(sample(groceries,100))

#apriori 알고리즘으로 규칙지정(빈발 항목집합 추리기)
groceryrules <- apriori(groceries,
                        list(support=0.006, confidence=0.25, minlen=2))

apriori(groceries) #옵션 없이 넣으면 default 옵션으로 들어가서
#맞는 규칙 하나도 없음 (default=support=0.1, confidence=0.8, maxlen=10)

groceryrules #규칙 463개

summary(groceryrules)

inspect(groceryrules[1:3]) #그냥 3개 

#연관 높은 상품들
inspect(sort(groceryrules,by='lift')[1:10]) #향상도 기준으로 정렬해서 상위 10개

#특정 상품과 관련된 규칙들만 추출
#키워드 줘서 추출
#'berries'가 lhs, rhs(items라고 함)에 포함된 경우 추출
berryrules <- subset(groceryrules,items %in% 'berries') #4개
inspect(berryrules)
berryrules <- subset(groceryrules,lhs %in% 'berries') #4개 #왼쪽에만 조건

#'berries'나 'yogurt' 들어간 경우
yogurtoo <- subset(groceryrules,items %in% c('berries','yogurt')) #132개
inspect(yogurtoo)

#부분 매칭 (포함되는 경우도 모두)
fruitrules <- subset(groceryrules, items %pin% 'fruit') #111개
inspect(fruitrules)

#완전매칭 (모두 포함하는 경우만)
allrules <- subset(groceryrules,items %ain% c('berries','yogurt')) #1개
inspect(allrules)

#파일로 만들기(quote=따옴표, row.names=행번호)
write(groceryrules,'groceryrules.csv',sep=',',quote=F,row.names=F)

#데이터프레임으로 바꾸기(분석업무할때)
groceryrulesDf <- as(groceryrules,'data.frame')

#추가조건(지지도나 신뢰도같은거)
fruitrules <- subset(groceryrules, items %pin% 'fruit' & confidence>0.4) #47개
inspect(fruitrules)


help(Epub)
#비엔나 경영대학 전자책 사용 기록
#15729 transactions 936개 물품
data("Epub")
summary(Epub)

inspect(Epub[1:10])

#각 아이템 별 지지도
itemFrequency(Epub[,])

#시각화
itemFrequencyPlot(Epub,topN=12, main='top12') #규칙 많이 보려면 0.005 밑으로 가야함
itemFrequencyPlot(Epub,support=0.012,main='support min 0.012') #12개
image(Epub[1:100])

#160이 최대 (support=0.001, confidence=0.05)
erules <- apriori(Epub,list(support=0.001,confidence=0.05,minlen=2))
inspect(erules)
summary(erules)
inspect(sort(erules,by='lift')[1:25])
inspect(sort(erules,by='lift',decreasing=F)[1:25])
inspect(sort(erules,by='confidence')[1:10])
inspect(sort(erules,by='support')[1:10])

#120개 support=0.001, confidence=0.1, maxlift=454

#12개 support=0.002, confidence=0.1, maxlift=53
erules12 <- apriori(Epub,list(support=0.002,confidence=0.1,minlen=2))
summary(erules12)
inspect(erules12)
inspect(sort(erules12,by='lift')[1:10])
inspect(sort(erules12,by='confidence')[1:10])
inspect(sort(erules12,by='support')[1:10])

#size3
erules3 <- apriori(Epub,list(support=0.001,confidence=0.1,minlen=3))
inspect(erules3)
#doc_6e7,doc_6e8,doc_6e9 중 하나를 보면 무조건 다른 것도 봄
#헝거게임인듯..
inspect(subset(erules, items %pin% '6e9'))

#
inspect(subset(erules, lhs %pin% '506'))
inspect(subset(erules, items %pin% '882'))
inspect(subset(erules, items %pin% '11d'))
inspect(subset(erules, items %pin% '87c'))

inspect(subset(erules, lhs %pin% c('4b4')))
inspect(subset(erules, lhs %pin% c('3d6')))
inspect(subset(erules, lhs %pin% c('574')))
inspect(subset(erules, lhs %pin% c('714')))
install.packages('arulesViz')
library(arulesViz)
plot(sort(erules,by='support')[1:10],method='grouped')
        