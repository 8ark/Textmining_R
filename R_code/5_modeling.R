rm(list =ls())

# 자, 이제 문서를 4개로 쪼갰으니까, 각 문서는 각 정당을 나타낸다.
# Corpus를 만들어보자

install.packages('RmecabKo')
install.packages('devtools')
devtools::install_github("junhewk/RmecabKo", INSTALL_opts=c('--no-lock'))

install.packages('RcppMeCab')
install.packages('tidytext')
install.packages('dplyr')
install.packages('tidyr')
install.packages('purrr')
install.packages('magrittr')
install.packages('tidyverse')
install.packages('rJava')
install.packages('rlang')
install.packages("stringi", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
devtools::install_github('haven-jeon/KoNLP') #, KoNLP가 그냥 인스톨로는 진행하기 어려운 상황
# library(rJava) # java 설치할 때, R 32/64 맞춰서 설치하삼


pkgs <- c('RmecabKo','devtools','RcppMeCab','tidytext','purrr','dplyr','tidyr','magrittr','tidyverse', 'rJava', 'KoNLP')
sapply(pkgs,require,character.only = TRUE)

library(tm)
library(stringr)
library(readr)


## 해당 코드 이후부터는 전처리 이후의 과정입니다.
## tm패키지는 텍스트마이닝에 필요한 패키지입니다.
## tm 패키지를 통해 폴더 내 파일들을 corpus로 만들어줍니다.

## "~/project/test/textmining_news/dtm" 는 분석에 사용할 텍스트, PDF 파일들을 한 곳에 모아놓은 폴더 경로입니다. 해당 경로를 지정함으로 문서 데이터를 모두 corpus 전환합니다.

mytextlocation <- "C:/Users/ycg00/Documents/Textmining_R/party"
mypaper <- VCorpus(DirSource(mytextlocation))

mycorpus <- tm_map(mypaper, removeNumbers)
# 해당 함수는 mypaper로 저장된 corpus 중 숫자 데이터를 제거한다는 뜻입니다.
# 숫자 텍스트가 분석에 필요없을 경우 해당 과정을 통해 데이터를 줄입니다.

## Corpus 형태로 묶은 데이터 중, noun, 즉 명사형태의 데이터를 추출합니다.
## KoNLP 패키지를 구동시켜 놓으셔야 사용 가능합니다.


mytempfunct <- function(myobject, oldexp, newexp) {
  newobject <- tm_map(myobject,
                      content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp)
  
  newobject
}

mycorpus <- mytempfunct(mycorpus, "[[:lower:]]", "")
mycorpus <- mytempfunct(mycorpus, "[[:upper:]]", "")
mycorpus <- mytempfunct(mycorpus, "\\(", "")
mycorpus <- mytempfunct(mycorpus, "\\)", "")
mycorpus <- mytempfunct(mycorpus, "'", "")
mycorpus <- mytempfunct(mycorpus, "/", "")
mycorpus <- mytempfunct(mycorpus, "-", "")
mycorpus <- mytempfunct(mycorpus, "\\?", "")


mycorpus <- tm_map(mycorpus, stripWhitespace)





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

table(unlist(lapply(myNounCorpus, function(x) str_extract_all(x, boundary("word")))))



## 앞서 파악한 유사어를 해당 함수를 통해 통일시킵니다.


temp <- myNounCorpus
str(temp)


str(myNounCorpus)

for (i in 1:length(myNounCorpus)) {
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                              "개혁[[:alpha:]]{1,}","개혁")
  # 개혁[[:alpha]](1,)는 '가능'이라는 말이 붙은 모든 단어를 '가능'으로 통일시킨다는 뜻입니다. 위와 같은 과정을 통해 유사어를 통일합니다.
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                              "가능[[:alpha:]]{1,}","가능")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "거부[[:alpha:]]{1,}","거부")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "검사[[:alpha:]]{1,}","검사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "경찰[[:alpha:]]{1,}","경찰")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "고위공무원","고위공직자")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "고지[[:alpha:]]{1,}","고지")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "공직[[:alpha:]]{1,}","공직")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "규정[[:alpha:]]{1,}","규정")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "그[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "근본[[:alpha:]]{1,}","근본")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "독재[[:alpha:]]{1,}","독재")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "무효[[:alpha:]]{1,}","무효")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "민주당[[:alpha:]]{1,}","더불어민주당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "더불어민주[[:alpha:]]{1,}","더불어민주당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "바른미래[[:alpha:]]{1,}","바른미래당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "바른정당[[:alpha:]]{1,}","바른미래당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "법률[[:alpha:]]{1,}","법률")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "보임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "불법[[:alpha:]]{1,}","불법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "사임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "상의했습니","상의")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "의원[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "위원[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "자격[[:alpha:]]{1,}","자격")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "조용[[:alpha:]]{1,}","조용")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "조정[[:alpha:]]{1,}","조정")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "통보[[:alpha:]]{1,}","통보")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "패스트[[:alpha:]]{1,}","패스트트랙")
# myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,   # 하려 했으나, 패스트 개수와 트랙 개수가 같은 것을 보고
#                                              "트랙","패스트트랙") # 안 해도 되는 것 파악
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "표결[[:alpha:]]{1,}","표결")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "합법[[:alpha:]]{1,}","합법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "헌법[[:alpha:]]{1,}","헌법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "확신[[:alpha:]]{1,}","확신")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "확실[[:alpha:]]{1,}","확신")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "0","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "c","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "character","")
  
}



## 각 문서들을 'row', 각 단어들을 'column'형태인
## DTM(Document Term Matrix)로 변환해줍니다.

dtm <- DocumentTermMatrix(myNounCorpus,
                          control = list(wordLengths = c(2, Inf),
                                         weighting = function(x) weightTfIdf(x, normalize = T)))

# TF-IDF 하고 싶으면 이걸로
# DTM_TF = removeSparseTerms(dtm, as.numeric(0.999))

# Freq_TF = colSums(as.matrix(DTM_TF))

# wordDf_TF = data.frame("word" = names(Freq_TF), "TF-IDF" = Freq_TF)
# View(wordDf_TF)



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

round(my.doc.cor[1:4,1:4], 2)


# 코사인 유사도 기반의 거리 측정을 했습니다.
## 거리 측정에 사용한 데이터는 각 단어로, 하나의 단어는 하나의 차원으로 사용됐습니다. # 숫자가 작을수록 유사한 것입니다.

dist.dtm <- dist(dtm)
as.matrix(dist.dtm)[1:4,1:4]

# 솔직히 지금 결과는 노답
# 말많이 한 정당이 승리인 결과
# 왜냐하면 정당 별 발언량이 유사하지 않기 때문이다.
# 더불어민주당이랑 자유한국당만 말하고 다른 정당은 거의 말할 기회도 없었다.
# 발언량이 비슷해졌으면 달랐을 것 같다.
