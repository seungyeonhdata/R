#-----버섯문제--------
setwd('C:/rwork')
mushroom <- read.csv('agaricus-lepiota.data',header=F,na.strings='?')
names(mushroom)=c('class','cap-shape','cap-surface','cap-color',
                  'bruises','odor','gill-attachment','gill-spacing',
                  'gill-size','gill-color','stalk-shape', 'stalk-root',
                  'stalk-surface-above-ring', 'stalk-surface-below.ring', 
                  'stalk-color-above-ring','stalk-color-below-ring', 'veil.type',
                  'veil-color', 'ring-number', 'ring-type',
                  'spore-print-color', 'population', 'habitat')
head(mushroom)
str(mushroom)
summary(mushroom)


table(mushroom['class'])

#한번에 보기
name <- names(mushroom)
for(i in 1:length(name)){
  print(name[i])
  print(table(mushroom[name[i]]))
}

library(Amelia)
missmap(mushroom,col=c('red','grey'))
sum(is.na(mushroom$`stalk-root`)) #NA 2480개

library(gmodels)
CrossTable(mushroom[,12])

#========결측치 처리=============
#1)컬럼별 동일한 문자 값이 가장 많은 k개 만큼 추출 -> ? 값을 대체

index_na <- which(is.na(mushroom[,12])) #NA 행들 위치

data_na <- mushroom[index_na,-12] #NA 행들 root열 제외 데이터

index_else <- which(!is.na(mushroom[,12])) #그외 행들 위치
length(index_else)

data_else <- mushroom[index_else,-12] #그외 행들 root열 제외 데이터


#가장 비슷한 것 상위 5개
for(i in 1:length(index_na)){
  top5 <- order(-colSums(apply(data_else,1,function(x){x==data_na[i,]})))[1:5]
  mushroom[index_na,12][i] <- names(which.max(table(mushroom[index_else,][top5,12])))
}

#20분 넘게 걸림

#2)컬럼별 문자를 원핫인코딩
temp <- mushroom[-12] #결측치 열을 제외한 임시 데이터프레임으로 원핫인코딩
name <- names(temp)
name
for(i in 2:length(name)){
  for(j in unique(temp[[i]])){
    temp[paste(name[i],j,sep='-')] <- ifelse(temp[i]==j,1,0)
  }
}

names(temp) #새로만든 이름확인
ncol(temp) 
hot_mushroom <- temp[,c(1,23:ncol(temp))] #문자로 된 앞부분 자르기
hot_mushroom

  
#3.train/test 분할(7:3)
train_mushdata
set.seed(1234)
split <- sample(2,nrow(train_mushdata),replace=T,prob=c(0.7,0.3))
train <- train_mushdata[split==1,-1]
test <- train_mushdata[split==2,-1]
train_labels <- train_mushdata[split==1,1]
test_labels <- train_mushdata[split==2,1]

#4.knn으로 정확도 
library(class)
class_pred <- knn(train,test,cl=train_labels,k=21)

sum(class_pred==test_labels)/length(class_pred) #0.9988031


#5.KNN으로 결측값 대체
index_na <- which(is.na(mushroom[,12])) #NA 행들 위치

train_mushdata <- hot_mushroom[-index_na,-1] #연습데이터
test_mushdata <- hot_mushroom[index_na,-1] #테스트 데이터
ans <- mushroom[-index_na,12] #연습데이터 답
ans

library(class)
knn(train=train_mushdata,test=test_mushdata,cl=ans,k=5)
#Error in knn(train_mushdata, test_mushdata, cl = ans, k = 21) : 
#too many ties in knn


# 5) NA값을 knn 적용하여 예측값 넣기

na.part <- na.part[,-1]

na.part_pred <- knn(train = train_tr,
                    test = na.part,
                    cl = train_labels,
                    k=5)
na.part_pred <- as.character(na.part_pred)
na.part_pred

one.hot.encoding[idx,][[1]] <- na.part_pred
one.hot.encoding[idx,][[1]]

# 6) stalk.root열도 원핫인코딩 진행

level <- unique(one.hot.encoding[[1]]) #"e" "c" "b" "r"
for(j in level){
  new <- paste("stalk.root", j, sep = ".")
  one.hot.encoding[new] <- ifelse(one.hot.encoding[[1]]==j,1,0)
}

names(one.hot.encoding)

# 전처리 끝낸 자료

new.mushroom <- cbind(class=mushroom[,1], one.hot.encoding[,-1]) #class열 factor
new.mushroom$class <- as.character(new.mushroom$class) #class열 chr로 변경


# 3. train data : test data = 7:3으로 나누기

set.seed(0311)
nrow(new.mushroom)*0.7 #5686.8
final_idx <- sample(1:nrow(new.mushroom), 5687)
train <- new.mushroom[final_idx,-1]
test <- new.mushroom[-final_idx,-1]

train_labels <- new.mushroom$class[final_idx]
test_labels <- new.mushroom$class[-final_idx]

# 4. knn을 이용한 모델 생성

test_pred <- knn(train = train,
                 test = test,
                 cl = train_labels,
                 k=4)
train_te_pred

# 5. 정확도 확인

sum(test_pred==test_labels) / length(test_labels)   #정확도 : 1









#4.knn으로 테스트










#전처리(결측값)stalk-shape
library(Amelia)
missmap(mushroom,col=c('red','blue'))


idx<-which(names(mushroom)=='stalk-shape') #11
N_na<-nrow(mush[is.na(mush[,'stalk-shape']),-idx]) 
target<-t(mush[!is.na(mush[,'stalk-shape']),-idx])
Na_predict<-c()
for(i in 1:N_na){
  na_sample<-unlist(mush[is.na(mush[,'stalk-shape']),-idx][i,,drop=T])
  ans<-target==na_sample
  similar_idx<-as.numeric(sort(colSums(ans),decreasing = T)[1:5])
  Na_predict<-c(Na_predict,names(sort(table(mush[similar_idx,idx]),decreasing = T))[1])
}
mush[is.na(mush[,'stalk-shape']),idx]<-Na_predict
sum(!complete.cases(mush))


#문자-> 수치변환 어떠한 규칙으로 할 것인가?
#종류가 2개 이상이고 순서가 없다면 one ho tencoding실시

str(mush)
features<-colnames(mush)[-1]
for(feature in features){
  iter<-unique(mush[[feature]])
  for(i in iter){
    new_col<-paste(feature,i,sep='')
    mush[new_col]<-ifelse(mush[feature]==i,1,0)
  }
}

train <- 
  colnames(mush)

end<-ncol(mush)
mushOnehot<-mush[c(1,24:end)]
str(mushOnehot)
head(mushOnehot)

#one hot encoding
for(i in seq(2,23,)){
  for (j in seq(1,length(unique(mushroom[i][,1] ) ))){
    mushroom[paste('V',i,'_',j,sep='')]<-ifelse(df[paste('V',i,sep='')]==unique(df[i])[j,1],1,0)
  } 
}

summary(df)
head(df,10)
mushroom[,c(1,23:ncol(mushroom))]




# 1. mushroom 데이터 읽기

mushroom <- read.table("agaricus-lepiota.data", sep = ",", na.strings = "?", as.is = FALSE)
str(mushroom)

colnames(mushroom)<- c("classes","cap.shape","cap.surface","cap.color","bruises",
                       "odor","gill.attachment","gill.spacing","gill.size",
                       "gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                       "stalk.surface.below.ring","stalk.color.above.ring",
                       "stalk.color.below.ring","veil.type","veil.color","ring.number",
                       "ring.type","spore.print.color","population","habitat")

summary(mushroom) #veil.type(17번째 열)은 모두 p인 것을 확인
mushroom <- mushroom[,-17]

apply(mushroom, 2, function(x){sum(is.na(x))})
# stalk-root(12번째 열)에 2480개의 결측값이 있음을 확인
# 2. 전처리(방법1 컬럼별 동일한 문자값이 가장 많은 k개만큼 추출 -> 값을 대체 -> 원핫인코딩)

# 1) V12에 있는 NA값 처리

st.d <- mushroom[,-12]
root <- mushroom[[12]]

na.idx <- which(is.na(root))

na.d <- st.d[na.idx,]
st.d <- st.d[-na.idx,]

for(i in 1:length(na.idx)){
  dist <- colSums(apply(st.d, 1, function(x){x == na.d[i,]}))
  res <- table(head(na.omit(root[order(dist)]),5))
  root[na.idx[i]] <- names(res[which.max(res)])
}

mushroom$stalk.root <- root

## 문제점 : 봐야할 데이터가 너무 많아 위의 for문을 처리하는데 시간이 엄청 걸린다

# 2) 문자 -> 수치화

old.colname <- colnames(mushroom[,-1])

for(i in old.colname){
  level <- unique(mushroom[[i]])
  for(j in level){
    new <- paste(i, j, sep = ".")
    mushroom[new] <- ifelse(mushroom[i]==j,1,0)
  }
}

colnames(mushroom)

one.hot.encoding <- mushroom[,c(1,23:ncol(mushroom))]



# 2. 전처리(방법2 컬럼별 문자를 원핫인코딩 -> knn -> ?값을 대체)

str(mushroom)
# 이 mushroom은 level이 1개인 17번 열을 제거만 한 상태(전처리 방법1 진행 직전과 동일한 상태)

# 1) 결측값가지고 있는 skelt.root 제외한 컬럼별 문자 원핫인코딩

old.colname <- colnames(mushroom[,-c(1,12)]) 
#class와 결측값존재하는 stalk.root 제외한 상태

for(i in old.colname){
  level <- unique(mushroom[[i]])
  for(j in level){
    new <- paste(i, j, sep = ".")
    mushroom[new] <- ifelse(mushroom[i]==j,1,0)
  }
}

colnames(mushroom)

one.hot.encoding <- mushroom[,c(12,23:ncol(mushroom))]

# 2) train 데이터와 test 데이터 생성
str(one.hot.encoding[[1]]) #factor
one.hot.encoding[[1]] <- as.character(one.hot.encoding[[1]]) #chr로 변환

idx <- which(is.na(mushroom$stalk.root))
train <- one.hot.encoding[-idx,]  #결측값 가지지 않는 경우
na.part <- one.hot.encoding[idx,]  #결측값 가지는 경우, 아래 5)번 과정에서 모델을 기준으로 채워넣어야 할 부분


# 결측값 가지지 않는 경우 내에서 다시 train과 test를 나눴다

set.seed(0310)
nrow(train)*0.7 #3950.8
tr_idx <- sample(1:nrow(train), 3950)
train_tr <- train[tr_idx,-1]
str(train_tr)
train_te <- train[-tr_idx,-1]

labels <- train[,1]
train_labels <- labels[tr_idx]
test_labels <- labels[-tr_idx]


# 3) 트레인 데이터로 knn 모델 생성

library(class)
train_te_pred <- knn(train = train_tr,
                     test = train_te,
                     cl = train_labels,
                     k=5)
train_te_pred

# 4) 테스트 데이터로 테스트

sum(train_te_pred==test_labels) / length(test_labels)  # 정확도 : 0.9988194

# 5) NA값을 knn 적용하여 예측값 넣기

na.part <- na.part[,-1]

na.part_pred <- knn(train = train_tr,
                    test = na.part,
                    cl = train_labels,
                    k=5)
na.part_pred <- as.character(na.part_pred)
na.part_pred

one.hot.encoding[idx,][[1]] <- na.part_pred
one.hot.encoding[idx,][[1]]

# 6) stalk.root열도 원핫인코딩 진행

level <- unique(one.hot.encoding[[1]]) #"e" "c" "b" "r"
for(j in level){
  new <- paste("stalk.root", j, sep = ".")
  one.hot.encoding[new] <- ifelse(one.hot.encoding[[1]]==j,1,0)
}

names(one.hot.encoding)

# 전처리 끝낸 자료

new.mushroom <- cbind(class=mushroom[,1], one.hot.encoding[,-1]) #class열 factor
new.mushroom$class <- as.character(new.mushroom$class) #class열 chr로 변경
???

???

# 3. train data : test data = 7:3으로 나누기

set.seed(0311)
nrow(new.mushroom)*0.7 #5686.8
final_idx <- sample(1:nrow(new.mushroom), 5687)
train <- new.mushroom[final_idx,-1]
test <- new.mushroom[-final_idx,-1]

train_labels <- new.mushroom$class[final_idx]
test_labels <- new.mushroom$class[-final_idx]

# 4. knn을 이용한 모델 생성

test_pred <- knn(train = train,
                 test = test,
                 cl = train_labels,
                 k=4)
train_te_pred

# 5. 정확도 확인

sum(test_pred==test_labels) / length(test_labels)   #정확도 : 1
