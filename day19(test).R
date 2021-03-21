#
#주석으로 이미지번호 표시하였습니다.


#mtcars 분석
#연비에 대한 각 변수의 연관성 분석
#가장 연관이 높은 wt로 예측


#삼성전자
#데이터 처리
#삼성전자 주식 데이터의 날짜가 20~21년에 걸쳐있기 때문에
#연도+월, 연도+분기로 분류

#주식 데이터 분석
#월별/분기별 최고, 최저, 평균가 통계를 냈을 때 
#전체적으로 상향하는 추세를 보이며 2020/10부터 2021/1까지 가파르게 상승
#2021 1월초, 2020년 3월 중순에 가장 큰 거래량

#상승장과 하락장을 표시하여 거래추이 살펴보기



install.packages('lubridate') #연도 불러오기
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
library(ggplot2)
library(gmodels)
library(lubridate)
library(dplyr)
library(reshape2)

# 1. mtcars 데이터를 불러온 후,
# - 전체 32개의 관측치 중 25개를 train, 7개를 test 데이터로 나누어 작업
# - mpg를 종속변수로 두고 다양한 회귀 모델 작성



mtcars
str(mtcars)

#32개 중 25개와 7개를 train/test로 분리
set.seed(0319)
idx <- sample(1:32,25)
train_mtcars <- mtcars[idx,]
test_mtcars <- mtcars[-idx,]


#mpg(연비)를 종속변수로 다양한 회귀모델


#단순회귀분석

#무게가 연비에 미치는 영향
wt.lm <- lm(mpg~wt,train_mtcars)
summary(wt.lm)
#P-value 0에 가까워 유의미
#adj rsquared=0.7223 무게가 차의 연비를 72% 설명가능


#등분산성 ##1번 이미지
dev.off()
par(mfrow=c(2,2))
plot(wt.lm)         

#산점도  ##2번 이미지
par(mfrow=c(1,1))
with(train_mtcars,plot(wt,mpg)) 
abline(wt.lm$coefficients,col='red')


#loess 방식으로 회귀분석 95% 신뢰구간 ##3번 이미지
ggplot(train_mtcars, aes(x=wt,y=mpg))+
  geom_point()+
  geom_smooth(method='loess',level=0.95)

#test데이터의 wt가 주어졌을 때 mpg값
mpg_pred_with_wt <- predict(wt.lm, test_mtcars)


#예측값과 실제값 비교 ##4번 이미지
plot(mpg_pred_with_wt,type='l',
     ylab='mpg',main='무게에 따른 연비')
lines(test_mtcars['mpg'],col='red')
legend('topleft',legend=c('예측','실제'),
       col=c('black','red'),lty=1)


#다중회귀분석
#wt, cyl, hp에 따른 mpg

multi.lm <- lm(mpg~wt+cyl+hp,train_mtcars)
summary(multi.lm)
#adj.rsqred 0.8223으로 연관 높은편
#p value는 0.0001 이하로 유의한 결과
#cyl은 p value 0.994663으로 연관성 매우 낮음

multi.lm$coefficients
#mpghat= -4.1854*wt -0.0045*cyl -0.0326*hp + 38.3728

#등분산성 ##5번 이미지
par(mfrow=c(2,2))
plot(multi.lm)

#변수별 분포와 회귀선, 상관관계 ##6번 이미지
mtcars_rel <- train_mtcars[c('mpg','wt','cyl','hp')]
chart.Correlation(mtcars_rel,histogram=T, pch=20)

#test데이터 주어졌을 때 mpg값
mpg_pred_with_multi <- predict(multi.lm, test_mtcars)


#예측값과 실제값 비교 ##7번 이미지
plot(mpg_pred_with_multi,type='l',
     ylab='mpg',main='wt,cyl,hp에 따른 연비')
lines(test_mtcars['mpg'],col='red')
legend('topleft',legend=c('예측','실제'),
       col=c('black','red'),lty=1)



# 2. yahoo.finance.com에서 최근 1년 동안의(2020.3.18~2021.3.18) 삼성전자 주식 데이터를 
# 다운로드 후, 종가를 예측하는 모델을 만드시오. (위 기간을 다르게 설정해도 관계없습니다)
# 1) 데이터 분리
# train 데이터 : 2020년 3월 18일~2021년 2월 18일(11개월)
# test 데이터 : 2021년 2월 19일~3월 18일까지 (1개월)
# 2) 데이터 분석
# - 월/분기별 최대/최소값 등 통계치
# - 월/분기별 주가 변화 시각화
# - 전체 기간 중 최고/최저 주가
# 3) 모델 작성 및 예측
# test데이터로 종가를 예측하고, 실제 종가와 비교하여 rss(잔차 제곱의 합)를 구하시오.


#===데이터 불러오기===
samsung_stock <- read.csv('samsung_stock.csv')
str(samsung_stock) #247개
sum(is.na(samsung_stock)) #결측치 없음

#===데이터 분리=== (2021-02-18일 기준)
samsung_stock$Date <- as.Date(samsung_stock$Date)
samsung_train <- samsung_stock[samsung_stock$Date<'2021-02-19',] #228개
samsung_test <- samsung_stock[samsung_stock$Date>'2021-02-18',] #19개


#===데이터 분석===

#월별 최고
monthly_max <- samsung_stock %>%
  select(-Date) %>% 
  group_by(mon=format(samsung_stock$Date,'%Y-%m')) %>% 
  summarise_all(funs(max=max(.)))
monthly_max


#월별 최저
monthly_min <- samsung_stock %>%
  select(-Date) %>% 
  group_by(mon=format(samsung_stock$Date,'%Y-%m')) %>% 
  summarise_all(funs(min=min(.)))
monthly_min


#월별 평균
monthly_avg <- samsung_stock %>%
  select(-Date) %>% 
  group_by(mon=format(samsung_stock$Date,'%Y-%m')) %>% 
  summarise_all(funs(avg=mean(.)))
monthly_avg


#분기별 최고
quarter_max <- samsung_stock %>% 
  select(-Date) %>% 
  group_by(quarter=paste(lubridate::year(samsung_stock$Date),
                        quarters(samsung_stock$Date))) %>% 
  summarise_all(funs(max=max(.)))
quarter_max

#분기별 최저
quarter_min <- samsung_stock %>% 
  select(-Date) %>% 
  group_by(quarter=paste(lubridate::year(samsung_stock$Date),
                         quarters(samsung_stock$Date))) %>% 
  summarise_all(funs(min=min(.)))
quarter_min

#분기별 평균
quarter_avg <- samsung_stock %>% 
  select(-Date) %>% 
  group_by(quarter=paste(lubridate::year(samsung_stock$Date),
                         quarters(samsung_stock$Date))) %>% 
  summarise_all(funs(avg=mean(.)))
quarter_avg


#전체기간 중 최고/최저 주가
stock_max <- samsung_stock %>% 
  select(-Date) %>% 
  summarise_all(funs(max=max(.)))
stock_max

stock_min <- samsung_stock %>% 
  select(-Date) %>% 
  summarise_all(funs(min=min(.)))
stock_min



#=====시각화=======

#날짜별 주가 변화 ##8번 이미지
#volume은 단위가 다르기 때문에 뺌(표준화해야함)
samsung_stock %>% 
  select(-Volume) %>%
  melt(id.vars='Date') %>% 
  ggplot(aes(x=Date,y=value,col=variable,group=variable))+
  geom_line(size=1)+
  xlab('날짜')+ylab('주가')+
  labs(title='날짜별 주가 변화',col='주가 설명')+
  theme_bw()+
  theme(legend.position=c(0.2,0.8))+
  stat_smooth(col='orange',fill='orange',method='loess',alpha=0.1) #추세선

#거래량(volume) ##9번 이미지
ggplot(samsung_stock,aes(x=Date,y=Volume))+
  geom_col(alpha=0.3)+geom_line(col='red')+
  xlab('날짜(일)')+ylab('거래량(원)')+
  labs(title='날짜별 거래량')
#거래량 특히 많은 4개 자료 날짜 궁금
head(samsung_stock[order(-samsung_stock$Volume),],4)
head(samsung_stock[order(-samsung_stock$Volume),],4)$Date
#그래프에 표시


#월별 최고가 시각화 ##10번 이미지

monthly_max %>% 
  select(-Volume_max) %>% 
  melt(id.vars='mon') %>% 
  ggplot(aes(x=mon,y=value,col=variable),alpha=0.5)+
  geom_point()+
  geom_line(aes(group=variable),alpha=0.8)+
  xlab('월')+ylab('최고 주가')+
  labs(col='주가 종류', title='월별 최고 주가')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        text=element_text(size=10,face='bold'),
        legend.position=c(0.2,0.8))

#2020년 10월부터 2021년 1월까지 급상승


#분기별 최고가 시각화 ##11번 이미지
quarter_max %>% 
  select(-Volume_max) %>% 
  melt(id.vars='quarter') %>% 
  ggplot(aes(x=quarter,y=value, col=variable),alpha=0.5)+
  geom_point()+
  geom_line(aes(group=variable))+
  xlab('분기')+ylab('최고 주가')+
  labs(title='분기별 최고 주가',col='주가 종류')+
  theme_bw()+
  theme(legend.position=c(0.2,0.8))

#근 일년간 최고-최저?  
stock_max
ggplot(melt(stock_max[-6]),aes(x=variable,y=value))+
  geom_col()


#모델 작성 및 예측
library(GGally)
ggcorr(samsung_stock,label=T) #volume 제외 모두 1


#train - 종가를 종속변수, adj.close를 독립변수로 회귀모델
model_adj <- lm(Close~Adj.Close,samsung_train)

summary(model_adj)
# Call:
#   lm(formula = Close ~ Adj.Close, data = samsung_train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -680.95 -207.70  -57.23  154.09 1185.96 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.008e+03  1.061e+02   37.76   <2e-16 ***
#   Adj.Close   9.575e-01  1.744e-03  549.07   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 335.8 on 226 degrees of freedom
# Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9992 
# F-statistic: 3.015e+05 on 1 and 226 DF,  p-value: < 2.2e-16



#상관관계가 1인 adj.close와 close #12번 이미지
#adj.close로 만든 회귀모델로 회귀선 그렸더니 종가가 낮을때는 예측이 벗어남
plot(samsung_train$Close,samsung_train$Adj.Close,
     main='주가',xlab='종가',ylab='수정종가')
abline(model_adj$coefficients,col='red')


attributes(model_adj)
fitted.values(model_adj) #train 예측값


#test 종가 예측
adj_pred <- predict(model_adj,samsung_test)

#시각화로 비교 ##13번 이미지
plot(adj_pred,type='l',col='red',
     main='Adj.close 모델을 이용한 종가 예측',
     ylab='주가')
lines(samsung_test['Close'])
legend(x='topright',legend=c('실 데이터','예측 데이터'),
       col=c('black','red'),
       lty=1)







#개장, 최고, 최저가로 다중회귀모델
model_ohl <- lm(Close~Open+High+Low,samsung_train)

summary(model_ohl)


ohl_pred <- predict(model_ohl, samsung_test)

#시각화로 비교 ##14번 이미지
plot(ohl_pred,type='l',col='red',
     main='Open+High+Low 모델을 이용한 종가 예측',
     ylab='주가')
lines(samsung_test['Close'])
legend(x='topright',legend=c('실 데이터','예측 데이터'),
       col=c('black','red'),
       lty=1)

#test데이터로 종가를 예측하고, 실제 종가와 비교하여 
#rss(잔차 제곱의 합)를 구하기

#잔차제곱의합


#Adj.Close
sum((samsung_test$Close-adj_pred)^2)
#4824552

#open+high+low
sum((samsung_test$Close-ohl_pred)^2)
#5173823


#adj.close만으로 회귀분석했을때 정확도 더 높음