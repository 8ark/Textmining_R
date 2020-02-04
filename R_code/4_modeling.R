rm(list =ls())


library(KoNLP)
library(tm)
library(stringr)
library(readr)


## 해당 코드 이후부터는 전처리 이후의 과정입니다.
## tm패키지는 텍스트마이닝에 필요한 패키지입니다.
## tm 패키지를 통해 폴더 내 파일들을 corpus로 만들어줍니다.

## "~/project/test/textmining_news/dtm" 는 분석에 사용할 텍스트, PDF 파일들을 한 곳에 모아놓은 폴더 경로입니다. 해당 경로를 지정함으로 문서 데이터를 모두 corpus 전환합니다.

mytextlocation <- "~/assembly_record/Textmining_R/party"
mypaper <- VCorpus(DirSource(mytextlocation))


## 해당 함수는 mypaper로 저장된 corpus 중 숫자 데이터를 제거한다는 뜻입니다.
## 숫자 텍스트가 분석에 필요없을 경우 해당 과정을 통해 데이터를 줄입니다.
mycorpus <- tm_map(mypaper, removeNumbers)

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
                                           "가능[[:alpha:]]{1,}","가능")
  # 가능[[:alpha]](1,)는 '가능'이라는 말이 붙은 모든 단어를 '가능'으로 통일시킨다는 뜻입니다. 위와 같은 과정을 통해 유사어를 통일합니다.
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "[[:lower:]]","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "가슴[[:alpha:]]{1,}","가슴")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "간사[[:alpha:]]{1,}","간사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "간첩[[:alpha:]]{1,}","간첩")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "간부[[:alpha:]]{1,}","간부")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "각오[[:alpha:]]{1,}","각오")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "간곡[[:alpha:]]{1,}","간곡")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "강압[[:alpha:]]{1,}","강압")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "강화[[:alpha:]]{1,}","강화")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "개선[[:alpha:]]{1,}","개선")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "개편[[:alpha:]]{1,}","개편")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "감표위원[[:alpha:]]{1,}","감표위원")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "개혁[[:alpha:]]{1,}","개혁")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "검찰[[:alpha:]]{1,}","검찰")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "검사[[:alpha:]]{1,}","검사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "견제[[:alpha:]]{1,}","견제")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "검토[[:alpha:]]{1,}","검토")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "겁박[[:alpha:]]{1,}","겁박")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "경찰[[:alpha:]]{1,}","경찰")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "고위공직자[[:alpha:]]{1,}","고위공직자")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "고민[[:alpha:]]{1,}","고민")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "고발[[:alpha:]]{1,}","고발")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "공무원[[:alpha:]]{1,}","공무원")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "고소[[:alpha:]]{1,}","고소")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "국민[[:alpha:]]{1,}","국민")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "권한쟁의[[:alpha:]]{1,}","권한쟁의")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "근본[[:alpha:]]{1,}","근본")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "꼼수[[:alpha:]]{1,}","꼼수")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "날치기[[:alpha:]]{1,}","날치기")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "남용[[:alpha:]]{1,}","남용")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "논의[[:alpha:]]{1,}","논의해")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "다수[[:alpha:]]{1,}","다수")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "대표발의[[:alpha:]]{1,}","대표발의")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "더불어민주당[[:alpha:]]{1,}","더불어민주당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "도둑[[:alpha:]]{1,}","도둑")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "독재[[:alpha:]]{1,}","독재")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "무효[[:alpha:]]{1,}","무효")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "문제[[:alpha:]]{1,}","문제")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "민주당[[:alpha:]]{1,}","더불어민주당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "바른미래당[[:alpha:]]{1,}","바른미래당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "민주평화당[[:alpha:]]{1,}","민주평화당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "반대[[:alpha:]]{1,}","반대")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "발의[[:alpha:]]{1,}","발의")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "방해[[:alpha:]]{1,}","방해")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "보임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "부분[[:alpha:]]{1,}","부분")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "불법[[:alpha:]]{1,}","불법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사개특위[[:alpha:]]{1,}","사개특위")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사보임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "사임[[:alpha:]]{1,}","사임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "생각[[:alpha:]]{1,}","생각")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "신뢰[[:alpha:]]{1,}","신뢰")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "신속[[:alpha:]]{1,}","신속처리안건")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "여러분[[:alpha:]]{1,}","여러분")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "원내대표[[:alpha:]]{1,}","원내대표")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "위반[[:alpha:]]{1,}","위반")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "위법[[:alpha:]]{1,}","위법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "의사진행[[:alpha:]]{1,}","의사진행발언")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "의장[[:alpha:]]{1,}","의장")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "자유한국당[[:alpha:]]{1,}","자유한국당")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "저희들[[:alpha:]]{1,}","저희들")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "적법[[:alpha:]]{1,}","적법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                           "참석[[:alpha:]]{1,}","참석")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "character","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "가벼","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "간첩[[:alpha:]]{1,}","간첩")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "강원[[:alpha:]]{1,}","강원랜드")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "강화[[:alpha:]]{1,}","강화")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "간첩[[:alpha:]]{1,}","간첩")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "간첩[[:alpha:]]{1,}","간첩")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국선변호[[:alpha:]]{1,}","국선변호")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "가급[[:alpha:]]{1,}","가급적")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "가능[[:alpha:]]{1,}","가능")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "가당치","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "가사비송이나","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기망행위[[:alpha:]]{1,}","기망행위")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기록[[:alpha:]]{1,}","기록")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기능[[:alpha:]]{1,}","기능")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기관지","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기관[[:alpha:]]{1,}","기관")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기계[[:alpha:]]{1,}","기계")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "근무[[:alpha:]]{1,}","근무")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "근거[[:alpha:]]{1,}","근거")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "그분[[:alpha:]]{1,}","그분")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "권력[[:alpha:]]{1,}","권력")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "군사[[:alpha:]]{1,}","군사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국회법[[:alpha:]]{1,}","국회법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국제인권법연구원[[:alpha:]]{1,}","국제인권법연구회")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국민[[:alpha:]]{1,}","국민")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "구태[[:alpha:]]{1,}","구태")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "구성[[:alpha:]]{1,}","구성")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "교통[[:alpha:]]{1,}","교통")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "교수[[:alpha:]]{1,}","교수")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "교육[[:alpha:]]{1,}","교육")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "공직자[[:alpha:]]{1,}","공직자")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "공정[[:alpha:]]{1,}","공정")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "공익[[:alpha:]]{1,}","공익")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "것[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "거예요","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "견제[[:alpha:]]{1,}","견제")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "공수처[[:alpha:]]{1,}","공수처")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "법관[[:alpha:]]{1,}","법관")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "삼성[[:alpha:]]{1,}","삼성")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "우리[[:alpha:]]{1,}","우리")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "이것[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "인권[[:alpha:]]{1,}","인권")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "전관예우[[:alpha:]]{1,}","전관예우")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "청와대[[:alpha:]]{1,}","청와대")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "판결[[:alpha:]]{1,}","판결")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "간첩[[:alpha:]]{1,}","간첩")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "갈등[[:alpha:]]{1,}","갈등")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "감표위원[[:alpha:]]{1,}","감표위원")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "강압[[:alpha:]]{1,}","강압")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "강원도[[:alpha:]]{1,}","강원도")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "강원랜드[[:alpha:]]{1,}","강원랜드")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "개념[[:alpha:]]{1,}","개념")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "개선[[:alpha:]]{1,}","개선")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "개편[[:alpha:]]{1,}","개편")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "객관[[:alpha:]]{1,}","객관")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "거악[[:alpha:]]{1,}","거악")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "거야[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "거지요[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "건데요[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "검거[[:alpha:]]{1,}","검거")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "겁박[[:alpha:]]{1,}","겁박")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "것들[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "것이[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "것인[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "고위[[:alpha:]]{1,}","고위직")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "구속[[:alpha:]]{1,}","구속")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국선[[:alpha:]]{1,}","국선변호")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국제인권법연구회[[:alpha:]]{1,}","국제인권법연구회")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "그것[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "그러[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "국선[[:alpha:]]{1,}","국선변호")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기망행위[[:alpha:]]{1,}","기망행위")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기무부대[[:alpha:]]{1,}","기무사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "기무사[[:alpha:]]{1,}","기무사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "꼼수[[:alpha:]]{1,}","꼼수")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "남북[[:alpha:]]{1,}","남북")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "댓글[[:alpha:]]{1,}","댓글")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "로스쿨[[:alpha:]]{1,}","로스쿨")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "로펌[[:alpha:]]{1,}","로펌")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "말씀드[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "문제[[:alpha:]]{1,}","문제")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "미국[[:alpha:]]{1,}","미국")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "미투[[:alpha:]]{1,}","미투")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "민노총[[:alpha:]]{1,}","민노총")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "사개특위[[:alpha:]]{1,}","사개특위")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "사ㆍ보임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "사보임[[:alpha:]]{1,}","사보임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "서울중앙지검[[:alpha:]]{1,}","서울중앙지검")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "서울지검[[:alpha:]]{1,}","서울중앙지검")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "선임[[:alpha:]]{1,}","선임")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "성접대[[:alpha:]]{1,}","성접대")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "수사[[:alpha:]]{1,}","수사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "아니[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "아닙[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "어떠[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "어떤[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "어젠다[[:alpha:]]{1,}","어젠다")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "없[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "여러분[[:alpha:]]{1,}","여러분")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "우리법연구회[[:alpha:]]{1,}","우리법연구회")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "이라크[[:alpha:]]{1,}","이라크")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "이러[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "일본[[:alpha:]]{1,}","일본")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "있다[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "저희[[:alpha:]]{1,}","")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "적법[[:alpha:]]{1,}","적법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "적폐[[:alpha:]]{1,}","적폐")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "정당[[:alpha:]]{1,}","정당성")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "적법[[:alpha:]]{1,}","적법")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "제도[[:alpha:]]{1,}","제도")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "조사[[:alpha:]]{1,}","조사")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "지휘[[:alpha:]]{1,}","지휘")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "청와대[[:alpha:]]{1,}","청와대")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "추진[[:alpha:]]{1,}","추진")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "특활비[[:alpha:]]{1,}","특활비")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "표결[[:alpha:]]{1,}","표결")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,
                                               "피의사실공표[[:alpha:]]{1,}","피의사실공표")
  myNounCorpus[[i]]$content <-  str_replace_all(temp[[i]]$content,
                                               "회의[[:alpha:]]{1,}","회의")
}



## 각 문서들을 'row', 각 단어들을 'column'형태인
## DTM(Document Term Matrix)로 변환해줍니다.

dtm <- DocumentTermMatrix(myNounCorpus,
                          control = list(wordLengths = c(2, Inf),
                                         weighting = function(x) weightTfIdf(x, normalize = T)))

DTM_TF = removeSparseTerms(dtm, as.numeric(0.999))

Freq_TF = colSums(as.matrix(DTM_TF))

wordDf_TF = data.frame("word" = names(Freq_TF), "TF-IDF" = Freq_TF)
View(wordDf_TF)



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