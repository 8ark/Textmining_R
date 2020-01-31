library(KoNLP)
library(tm)
library(stringr)


## 해당 코드 이후부터는 전처리 이후의 과정입니다.
## tm패키지는 텍스트마이닝에 필요한 패키지입니다.
## tm 패키지를 통해 폴더 내 파일들을 corpus로 만들어줍니다.

## "~/project/test/textmining_news/dtm" 는 분석에 사용할 텍스트, PDF 파일들을 한 곳에 모아놓은 폴더 경로입니다. 해당 경로를 지정함으로 문서 데이터를 모두 corpus 전환합니다.

mytextlocation <- "~/project/test/textmining_news/dtm"
mypaper <- VCorpus(DirSource(mytextlocation))

## 해당 함수는 mypaper로 저장된 corpus 중 숫자 데이터를 제거한다는 뜻입니다.
## 숫자 텍스트가 분석에 필요없을 경우 해당 과정을 통해 데이터를 줄입니다.
mycorpus <- tm_map(mypaper, removeNumbers)

## Corpus 형태로 묶은 데이터 중, noun, 즉 명사형태의 데이터를 추출합니다.
## KoNLP 패키지를 구동시켜 놓으셔야 사용 가능합니다.

myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext), collapse = ' ')
  myNounList
}


## 해당 함수는 corpus내 단어들을 가나다 순으로 보는 함수입니다.
## 해당 함수를 통해 유사한 단어들을 하나로 정리할 준비를 할 수 있습니다.

myNounCorpus <- mycorpus
for (i in 1:length(mycorpus)) {
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

table(unlist(lapply(mycorpus, function(x) str_extract_all(x, boundary("word")))))



## 앞서 파악한 유사어를 해당 함수를 통해 통일시킵니다.


temp <- mycorpus
for (i in 1:length(mycorpus)) {
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "가능[[:alpha]]{1,}","가능")
  # 가능[[:alpha]](1,)는 '가능'이라는 말이 붙은 모든 단어를 '가능'으로 통일시킨다는 뜻입니다. 위와 같은 과정을 통해 유사어를 통일합니다.
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "간사[[:alpha]]{1,}","간사")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "감표위원[[:alpha]]{1,}","감표위원")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "개혁[[:alpha]]{1,}","개혁")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "검찰경찰[[:alpha]]{1,}","검찰경찰")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "검토[[:alpha]]{1,}","검토")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "겁박[[:alpha]]{1,}","겁박")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "경찰[[:alpha]]{1,}","경찰")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "고위공직자[[:alpha]]{1,}","고위공직자")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "공수처[[:alpha]]{1,}","공수처")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "국민[[:alpha]]{1,}","국민")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "권한쟁의[[:alpha]]{1,}","권한쟁의")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "근본[[:alpha]]{1,}","근본")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "꼼수[[:alpha]]{1,}","꼼수")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "날치기[[:alpha]]{1,}","날치기")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "남용[[:alpha]]{1,}","남용")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "논의[[:alpha]]{1,}","논의해")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "다수[[:alpha]]{1,}","다수")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "대표발의[[:alpha]]{1,}","대표발의")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "더불어민주당[[:alpha]]{1,}","더불어민주당")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "도둑[[:alpha]]{1,}","도둑")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "독재[[:alpha]]{1,}","독재")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "무효[[:alpha]]{1,}","무효")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "문제[[:alpha]]{1,}","문제")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "민주당[[:alpha]]{1,}","더불어민주당")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "바른미래당[[:alpha]]{1,}","바른미래당")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "민주평화당[[:alpha]]{1,}","민주평화당")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "반대[[:alpha]]{1,}","반대")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "발의[[:alpha]]{1,}","발의")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "방해[[:alpha]]{1,}","방해")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "보임[[:alpha]]{1,}","사보임")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "부분[[:alpha]]{1,}","부분")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "불법[[:alpha]]{1,}","불법")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사개특위[[:alpha]]{1,}","사개특위")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사보임[[:alpha]]{1,}","사보임")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사임[[:alpha]]{1,}","사임")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "생각[[:alpha]]{1,}","생각")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "신뢰[[:alpha]]{1,}","신뢰")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "신속[[:alpha]]{1,}","신속처리안건")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "여러분[[:alpha]]{1,}","여러분")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "원내대표[[:alpha]]{1,}","원내대표")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "위반[[:alpha]]{1,}","위반")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "위법[[:alpha]]{1,}","위법")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "의사진행[[:alpha]]{1,}","의사진행발언")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "의장[[:alpha]]{1,}","의장")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "자유한국당[[:alpha]]{1,}","자유한국당")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "저희들[[:alpha]]{1,}","저희들")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "적법[[:alpha]]{1,}","적법")
  mycorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "참석[[:alpha]]{1,}","참석")
}



## 각 문서들을 'row', 각 단어들을 'column'형태인
## DTM(Document Term Matrix)로 변환해줍니다.

dtm <- DocumentTermMatrix(mycorpus)


## 분석에 필요한 개인용 함수를 만들었습니다.
## 해당 함수를 통해 코사인 유사도 기반 분석을 할 수 있습니다.

my.assoc.func <- function(mydtm, term1, term2){
  myvar1 <- as.vector(mydtm[, term1])
  myvar2 <- as.vector(mydtm[, term2])
  cor.test(myvar1, myvar2)
}

length.doc <- length(rownames(dtm))
my.doc.cor <- matrix(NA, nrow = length.doc, ncol = length.doc)

for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm), rownames(dtm)[i], rownames(dtm)[j])$est
  }
}

rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm)

round(my.doc.cor[1:4,4:1], 2)


# 코사인 유사도 기반의 거리 측정을 했습니다.
## 거리 측정에 사용한 데이터는 각 단어로, 하나의 단어는 하나의 차원으로 사용됐습니다. # 숫자가 작을수록 유사한 것입니다.

dist.dtm <- dist(dtm)
as.matrix(dist.dtm)[1:4,1:4]



## 검정 시작
## 나이브베이즈 분류기 기반의 검정을 시작합니다.

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