
## 검정 시작
## 나이브베이즈 분류기 기반의 검정을 시작합니다.

# install.packages('caret')
# install.packages('e1071')
# install.packages('gmodels')

library(caret)
library(e1071)
library(gmodels)



## --------------------나이브베이즈---------------------##

set.seed(32121391)

# 행의 건수 확인
dim() #사용할 데이터 셋 넣으면 행렬 개수를 볼 수 있습니다.

# 4_divide의 내용들을 그대로 옮겨왔다. 
df_with_party <- read.table("C:/Users/ycg00/Documents/Textmining_R/editing/df_with_party.txt",
                            sep = "\t")
View(df_with_party)

df_with_party %>%
  select(V2, V3) %>%
  unnest_tokens(input = V2,
                output = word,
                token = SimplePos09) -> word_party

# 엇, 에러가 뜬다.
# warnings()를 쳐보니 character로 바꾸라 한다. 바꾸자

df_with_party$V2 <- as.character(df_with_party$V2)


# 이제 잘 된다.

df_with_party %>%
  select(V2, V3) %>%
  unnest_tokens(input = V2,
                output = word,
                token = SimplePos09) -> word_party

View(word_party)

word_party %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word_done = str_remove(word, "/.*$")) %>%
  select(V3, word_done) %>%
  filter(nchar(word_done)>1)->
  n_party

View(n_party)


train_cnt <- round(0.7 * dim(n_party)[1])

# shuffle 하는 방법 중 하나
# 전체 데이터 중 70%를 훈련 데이터로, 30%를 평가 데이터로 나눕니다.

train_indx <- sample(1:dim(n_party)[1], train_cnt, replace = F)

table_train <- n_party[train_indx,]
table_test <- n_party[-train_indx,] 


# 오스트리아에서 만든 나이브베이즈 분류기 패키지입니다.

library(e1071)

#훈련데이터를 이용해 모델(분류기)를 만듭니다.
Result_Poli <- naiveBayes(V3 ~., data = table_train)

# training
pred.poli <- predict(Result_Poli, newdata = table_train)

# 훈련데이터로만든 모델에 다시 훈련 데이터를 넣었을 때
# 결과입니다.
confusionMatrix(pred.poli, table_train$V3)


#validation
pred.poli <- predict(Result_Poli, newdata = table_test)

# 평가 데이터로 모델의 정확도를 파악합니다.
# 훈련데이터와 평가 데이터의 정확도를 비교하면서
# 과적합 여부를 판단합니다.
confusionMatrix(pred.poli, table_test$V3)



### 결론
## 얼마나 데이터가 없으면  딴 정당 단어보고 자기네 단어라고 하는지
## 단순히 단어를 쪼개서 쓰는 게 아니라 엔그램 같은 것들을 섞었으면 더 좋았을 수도 있겠다.