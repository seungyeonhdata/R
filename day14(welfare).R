install.packages("foreign") # foreign 패키지 설치
library(foreign) # SPSS 파일 로드
library(dplyr) # 전처리
library(ggplot2) # 시각화
install.packages('readxl')
library(readxl) # 엑셀 파일 불러오기
library(Amelia) #결측치 시각화

setwd('C:/rwork')
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",to.data.frame = T)
head(raw_welfare)

View(raw_welfare)

feature <- read_excel('Koweps_Codebook.xlsx')
jobcode <- read_excel('Koweps_Codebook.xlsx',sheet=2)
jobcode
#============전처리=============
#변수명 변경 및 추출(출생연도는 나이로, 성별은 남녀로,직종코드내용 포함)
welfare <- raw_welfare %>% 
  rename(gender=h10_g3,age=h10_g4,marriage=h10_g10,
         religion=h10_g11,job=h10_eco9,income=p1002_8aq1,region=h10_reg7) %>% 
  select(gender,age,marriage,religion,job,income,region) %>% 
  mutate(age=2015-age+1,
         gender=ifelse(gender==1,'male','female')) %>% 
  left_join(jobcode,by=c('job'='code_job'))

head(welfare)
str(welfare) #16664개 자료



#--------결측치 확인---------

missmap(welfare,col=c('red','grey'))
summary(welfare) #job 9135개, income 12030개 


#------이상치 확인---------
summary(welfare) #무응답 없음 
#자세히: ...
table(welfare$gender) #남자:1, 여자:2 이상치 없음
table(welfare$job) #직종 무응답(9999) 없음
table(welfare$marriage)
table(welfare$religion)
table(welfare$region)
summary(welfare$age) #나이 무응답(-8023) 없음
boxplot(welfare$age) #극단치 없음
summary(welfare$income) #최소 0, 최대 2400, 무응답(9999)없음 #NA:12030개

par(mfrow=c(1,1))


#---월급 전처리---
#월급 범위(1~9998) 밖의 값은 결측처리
welfare$income <- ifelse(welfare$income==0,NA,welfare$income)
summary(welfare) #job 9135개, income 12030개 ->12044개


#-------변수별 상관관계-------
library(GGally)
ggcorr(welfare,label=T) 
#0.2이상: gender-income> job-income > job-age > marriage-income
#                 -            -          +             -


                 
#--------성별x월급---------
df <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(income) %>% 
  arrange(-income)

ggplot(df, aes(x=income, col=gender))+
  geom_histogram(position='identity')

gender_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(mean_income=mean(income,na.rm=T))
gender_income #남여 평균 월급

with(welfare,tapply(income,gender,max,na.rm=T)) #남여 최대 월급

#female   male 
#2400.0 1853.3 

ggplot(gender_income, aes(x=gender, y=mean_income, fill=gender))+
  geom_col()+
  geom_label(aes(label=round(mean_income,1)))+
  labs(title='성별에 따른 평균월급',
       ylab='x10,000(원)')

boxplot(welfare$income~welfare$gender)

#--------나이x월급-----------

plot(density(welfare$age)) #나이 분포

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
age_income #20세부터 월급 있음

ggplot(age_income, aes(x=age, y=mean_income))+
  geom_line()
  

#--나이 연령대별로 나누기--
welfare$age.ctg <- cut(welfare$age,breaks=c(0,20,30,40,50,60,70,80,120),
          labels=c('minor','20s','30s','40s','50s','60s','70s','senior'),
          right=F)
table(welfare$age.ctg)


#-----연령대x월급-----

#연령대별 월급평균
agectg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age.ctg) %>% 
  summarise(mean_income=mean(income))
agectg_income

#분석그래프
ggplot(agectg_income, aes(x=age.ctg, y=mean_income))+
  geom_col()+
  geom_label(aes(label=round(mean_income,1)),size=3)

# 40s > 50s > 30s > 20s > 60s and older 


#-------성별x연령대x월급------
gen_agectg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender,age.ctg) %>% 
  summarise(mean_income=mean(income))

ggplot(gen_agectg_income, aes(x=age.ctg, y=mean_income, fill=gender))+
  geom_col(position='dodge')

#--------지역x월급평균------------?

df <- data.frame(id=c(1:7),
      region=c('서울','인천/경기','부산/경남/울산',
                 '대구/경북','대전/충남','강원/충북',
                 '광주/전남/전북/제주도'))
welfare <- left_join(welfare,df,by=c('region'='id'))

region_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(region.y) %>% 
  summarise(mean_income=mean(income))

region_income  

ggplot(region_income, aes(x=region.y, y=mean_income, fill=region.y))+
  geom_col()+
  coord_flip()
  
  
#----------직업x월급평균-----------
#고월급 직업 top 10
wellpaid10 <- welfare %>% 
  filter(!is.na(job.y)&!is.na(income)) %>% 
  group_by(job.y) %>% 
  summarise(mean_income=mean(income)) %>% 
  arrange(-mean_income) %>% 
  head(10)
wellpaid10

ggplot(wellpaid10, aes(x=reorder(job.y,mean_income),y=mean_income))+
  geom_col()+
  coord_flip()

#-------성별x직업----------

female_job <- welfare %>% 
  filter(!is.na(job.y)&gender=='female') %>% 
  group_by(job.y) %>% 
  summarise(num=n()) %>% 
  mutate(perc=round(num/sum(num)*100,1)) %>% 
  arrange(-num) %>% 
  head(7)
female_job

par(mar=c(0,0,0,0),oma=c(0,0,2,0))
pie(female_job$num, labels=paste(female_job$job.y,female_job$perc,'%'),
    col=rainbow(12,alpha=seq(0.1,1,length=12)),
    main='female job distribution',
    clockwise=T, radius=0.5)


male_job <- welfare %>% 
  filter(!is.na(job.y)&gender=='male') %>% 
  group_by(job.y) %>% 
  summarise(num=n()) %>% 
  mutate(perc=round(num/sum(num)*100,1)) %>% 
  arrange(-num) %>% 
  head(7)
male_job

pie(male_job$num, labels=paste(male_job$job.y,male_job$perc,'%'),
    col=topo.colors(10,alpha=seq(0.1,1,length=10)),
    main='male job distribution',
    clockwise=T,radius=0.5)

#-----성별x직업x월급-----? 
gender_job_income <- welfare %>% 
  filter(!is.na(income)&!is.na(job)) %>% 
  group_by(job.y,gender) %>% 
  summarise(mean_income=mean(income)) %>% 
  arrange(-mean_income)

gender_job_income

ggplot(gender_job_income, aes(x=job.y, y=mean_income))+
  geom_col()+
  coord_flip()+
  facet_grid(gender ~ .)

#----종교x월급-----
religion_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(religion) %>% 
  mutate(religion=ifelse(religion==1,'yes','no')) %>% 
  summarise(mean_income=mean(income,na.rm=T))

ggplot(religion_income, aes(x=religion,y=mean_income, fill=religion))+
  geom_col()



#============================

#나이별 평균 월급으로 월급 결측값 채우기--
ave.income <- with(welfare,ave(income,age,FUN=function(x) mean(x,na.rm=T)))
welfare$income <- ifelse(is.na(welfare$income),ave.income,welfare$income)
summary(welfare$income)
(welfare$age[is.na(welfare$income)])

#직업 결측값 제거
welfare <- na.omit(welfare$job)


#-------kmeans--------

  

