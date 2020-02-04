
## 검정 시작
## 나이브베이즈 분류기 기반의 검정을 시작합니다.

install.packages('caret')
install.packages('e1071')
install.packages('gmodels')

library(caret)
library(e1071)
library(gmodels)



## --------------------나이브베이즈---------------------##

set.seed(32121391)

# 행의 건수 확인
dim() #사용할 데이터 셋 넣으면 행렬 개수를 볼 수 있습니다.

train_cnt <- round(0.7 * dim(n_word_3)[1])

# shuffle 하는 방법 중 하나
# 전체 데이터 중 70%를 훈련 데이터로, 30%를 평가 데이터로 나눕니다.

train_indx <- sample(1:dim(n_word_3)[1], train_cnt, replace = F)

table_train <- n_word_3[train_indx,]
table_test <- n_word_3[-train_indx,] 


# 오스트리아에서 만든 나이브베이즈 분류기 패키지입니다.

library(e1071)

#훈련데이터를 이용해 모델(분류기)를 만듭니다.
Result_Poli <- naiveBayes(party ~., data = table_train)

# training
pred.poli <- predict(Result_Poli, newdata = table_train)

# 훈련데이터로만든 모델에 다시 훈련 데이터를 넣었을 때
# 결과입니다.
confusionMatrix(pred.poli, table_train$party)


#validation
pred.poli <- predict(Result_Poli, newdata = table_test)

# 평가 데이터로 모델의 정확도를 파악합니다.
# 훈련데이터와 평가 데이터의 정확도를 비교하면서
# 과적합 여부를 판단합니다.
confusionMatrix(pred.poli, table_test$party)