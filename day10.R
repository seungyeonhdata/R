#데이터 전처리
#데이터 분할 -> 분할자료에 함수 적용 -> 결과 취합
airquality
with(airquality,airquality[order(-Temp,Month,Day),])

#group_by #그룹단위 연산
library(dplyr)
data(airquality)
summarise(group_by(airquality,Month),
          n(),
          mean(Temp,na.rm=T))

air <- select(airquality,Ozone,Temp,Month) %>% 
  group_by(Month) %>% 
  summarise(Mean.Ozone=mean(Ozone, na.rm=T),
            Mean.Temp=mean(Temp)) %>% 
  filter(Mean.Ozone>70|Mean.Temp>80)

#left_join() :열데이터 합치기
#bind_rows() :행데이터 합치기

str(air)
class(air)
as_tibble(air)
#dataframe->tibble
tbl_df(as.data.frame(air))

install.packages('Lahman')
library(Lahman)
Batting #1870년부터 150년 간의 미국 야구선수 타율 데이터
str(Batting)
head(Batting)
tail(Batting)
as_tibble(Batting)

#----데이터 변환-----
#외형상 특징 : 와이드형(가로), 롱형(세로: 열이 subject, var, value)
#melt() : 와이드형 -> 롱형 reshape2 패키지에 있음
#dcast() : 롱형 -> 와이드형
install.packages('reshape2')
library(reshape2)
smiths #reshape2에 포함된 데이터
str(smiths) 
#melt(data,id.vars='subject') factor형 이어야함
smiths.long <- melt(smiths)
smiths.long
#포물러x~y : x는 식별자 변수, y는 측정 변수, 변수가 여러개면 +로 연결
dcast(smiths.long,subject~variable)

melt(airquality)
#month 와 day 묶어서 식별자로 사용
aq.long <- melt(airquality,id.vars=c('Month','Day'))
aq.wide <- dcast(aq.long,Month+Day~variable)


#-----연습-----------
exam <- read.csv('csv_exam.csv')
exam %>% 
  filter(!class==1)
#1,2,3반 자료
exam %>% 
  filter(class %in% c(1,2,3))

exam %>% 
  filter(class==1) %>% 
  summarise(mean(math))

exam %>% 
  arrange(desc(math))

exam %>% 
  select(id,math) %>% 
  head(6)

exam %>% 
  mutate(total=math+english+science,
         test=ifelse(science>=60,'pass','fail')) %>% 
  arrange(total)

exam %>% 
  group_by(class) %>% 
  summarise(mean(math),
            sum(math),
            median(math),
            n())

mpg %>% 
  group_by(manufacturer) %>%
  filter(class=='suv') %>% 
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(tot=mean(tot)) %>% 
  arrange(desc(tot)) %>% 
  head(5)

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

left_join(test1,test2,by='id')

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
left_join(name,exam)

# 학생 1~5 번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
# 학생 6~10 번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
bind_rows(group_a,group_b)


#---연습문제----
#1. 
mpg %>% 
  filter(displ<=4|displ>=5) %>% 
  mutate(disp=ifelse(displ<=4,'low.displ','high.displ')) %>% 
  group_by(disp) %>% 
  summarise(mean.hwy=mean(hwy))

mpg %>%
  filter(manufacturer %in% c('audi','toyota')) %>% 
  group_by(manufacturer) %>% 
  summarise(mean.cty=mean(cty) %>%
  arrange(mean.cty)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(manufacturer %in% c('chevrolet','ford','honda')) %>% 
  summarise(mean.hwy=mean(hwy))

#2.
new <- mpg %>% 
  select(class,cty)

head(new)
str(new)

new %>% 
  group_by(class) %>% 
  filter(class %in% c('suv','compact')) %>% 
  summarise(max=max(cty),
            mean=mean(cty))  

#3.
mpg %>% 
  filter(manufacturer=='audi') %>% 
  group_by(model) %>% 
  arrange(desc(hwy)) %>% 
  head(5)

#4.
mpg %>% 
  mutate(tot=cty+hwy,
         avg=tot/2) %>% 
  arrange(desc(avg)) %>% 
  head(3)

#5.
q1 <- mpg %>% 
  group_by(class) %>% 
  summarise(mean.cty=mean(cty))

q1 %>% 
  arrange(desc(mean.cty))

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean.hwy=mean(hwy)) %>% 
  arrange(desc(mean.hwy)) %>% 
  head(3)

mpg %>% 
  filter(class=='compact') %>% 
  group_by(manufacturer) %>% 
  summarise(cnt=n()) %>% 
  arrange(desc(cnt))

#6.
fuel <- data.frame(fl=c('c','d','e','p','r'),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22))
mpg %>% 
  left_join(fuel) %>% 
  select(model,fl,price_fl) %>% 
  head(5)

#7.
ggplot2::midwest
midwest['percasian']

q1 <- midwest %>% 
  mutate(percchild=(poptotal-popadults)/poptotal*100)
  
q2 <-q1 %>% 
  arrange(desc(percchild)) %>% 
  select(county,percchild) %>% 
  head(5)

q1 %>% 
  mutate(c.category=cut(percchild,breaks=c(0,30,40,100),
      labels=c('small','middle','large'),
      right=F,
      include.lowest=T)) %>% 
  group_by(c.category) %>% 
  summarise(n.counties=n())

midwest %>% 
  mutate(percasians=popasian/poptotal*100) %>% 
  select(state,county,percasians) %>% 
  arrange(percasians) %>% 
  head(10)
