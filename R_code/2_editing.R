rm(list =ls())


# 합친 회의록 데이터를 정리하자

df <- read.csv("C:/Users/ycg00/Documents/Textmining_R/law/law_all.txt",
                 sep = "\t",
                 header = F)

head(df)


library(stringr)
# V1 에서
v1 <- str_replace_all(df$V1, "&#9711;", "")
v1_1 <- str_replace_all(v1, " 위원", "")
v1_2 <- str_replace_all(v1_1, " 의원", "")
v1_3 <- str_replace_all(v1_2, "\\(", "")
v1_4 <- str_trim(v1_3, side = c("both"))

# 해당 회의에 참여한 인물들을 알 수 있음
table(v1_4)
View(v1_4)



# V2 에서
v2_1 <- str_replace_all(df$V2, "&#8228;", "")
v2_2 <- str_replace_all(v2_1, "\\([[:print:]]{1,}\\)", "")

# 원래 여기서 하나하나 편집하는 건 아니지만
# 위원석에서 - 라는 단어가 계속 설명식으로 나오길래 여기서 같이 지워버리자
v2_3 <- str_replace_all(v2_2, "위원석에서 ―", "")
v2_4 <- str_trim(v2_3, side = c("both"))


# 각 의원의 정당을 맞춰서 칼럼을 추가하자

df_party <- data.frame(v1_4 = c("강효상", "곽상도", "권은희", "김도읍", "노회찬", "박범계", "박주미", "박지원", "백혜련", "송기석", "송기헌", "안호영", "여상규", "염동열", "오신환", "윤상직", "윤한홍", "이상민", "李恩宰", "이장우", "이재정", "이종걸", "이철규", "이철희", "임재훈", "장제원", "鄭宗燮", "정태옥", "조응천", "지상욱", "진선미", "채이배", "표창원", "咸珍圭"),
                       party = c("자유한국", "자유한국", "바른미래", "자유한국", "공동교섭(정의-민주평화)", "더불어민주", "공동교섭(정의-민주평화)", "공동교섭(정의-민주평화)", "더불어민주", "국민의", "더불어민주","더불어민주", "자유한국", "자유한국", "바른미래", "자유한국", "자유한국", "더불어민주", "자유한국", "자유한국", "더불어민주", "더불어민주", "자유한국", "더불어민주", "바른미래","자유한국","자유한국", "자유한국","더불어민주","바른미래","더불어민주","바른미래","더불어민주", "자유한국"))



# 정리가 끝난 각 컬럼을 재결합
df_new <- cbind(v1_4, v2_4)
View(df_new)



df_nnew <- merge(df_new, df_party, by ='v1_4', all.x=T)

table(df_nnew$party)

# 당없는 사람들과 위원장은 다 제거하자
# party가 없는 NA상태의 행 모두 제거


df_with_party <- na.omit(df_nnew)

# 열 이름 바꾸기

colnames(df_with_party) = c("name", "ment", "party")
df_with_party$ment <- as.character(df_with_party$ment)
df_with_party$party <- as.character(df_with_party$party)

View(df_with_party)
str(df_with_party)

# 잠깐 쉬어가는 타임, 일단 df_with_party 저장

write.table(df_with_party, "C:/Users/ycg00/Documents/Textmining_R/editing/df_with_party.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            quote = F)

# 어절로 데이터 자른 뒤, 유사단어 통합 및 정제

library(stringr)
library(stringi) # install.packages("stringi", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(tidyverse)
library(dplyr)
library(tidytext)
library(KoNLP)

useNIADic()

str(df_with_party)

df_with_party %>%
  select(ment, party) %>%
  unnest_tokens(input = ment,
                output = word,
                token = SimplePos09) -> word_party

str(word_party)
View(word_party)

library(tidyr)
library(purrr)
library(wordcloud)# install.packages("wordcloud")


word_party %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word_done = str_remove(word, "/.*$")) %>%
  select(party, word_done) %>%
  filter(nchar(word_done)>1)->
  n_party

View(n_party)



for (i in 1:length(n_party$word_done)) {
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "[[::punct::]]","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가능[[:alpha:]]{1,}","가능")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "[[:lower:]]","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가슴[[:alpha:]]{1,}","가슴")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간사[[:alpha:]]{1,}","간사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간첩[[:alpha:]]{1,}","간첩")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간부[[:alpha:]]{1,}","간부")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "각오[[:alpha:]]{1,}","각오")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간곡[[:alpha:]]{1,}","간곡")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강압[[:alpha:]]{1,}","강압")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강화[[:alpha:]]{1,}","강화")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개선[[:alpha:]]{1,}","개선")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개편[[:alpha:]]{1,}","개편")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "감표위원[[:alpha:]]{1,}","감표위원")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개혁[[:alpha:]]{1,}","개혁")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "검찰[[:alpha:]]{1,}","검찰")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "검사[[:alpha:]]{1,}","검사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "견제[[:alpha:]]{1,}","견제")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "검토[[:alpha:]]{1,}","검토")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "겁박[[:alpha:]]{1,}","겁박")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "경찰[[:alpha:]]{1,}","경찰")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "고위공직자[[:alpha:]]{1,}","고위공직자")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "고민[[:alpha:]]{1,}","고민")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "고발[[:alpha:]]{1,}","고발")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "공무원[[:alpha:]]{1,}","공무원")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "고소[[:alpha:]]{1,}","고소")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국민[[:alpha:]]{1,}","국민")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "권한쟁의[[:alpha:]]{1,}","권한쟁의")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "근본[[:alpha:]]{1,}","근본")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "꼼수[[:alpha:]]{1,}","꼼수")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "날치기[[:alpha:]]{1,}","날치기")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "남용[[:alpha:]]{1,}","남용")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "논의[[:alpha:]]{1,}","논의해")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "다수[[:alpha:]]{1,}","다수")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "대표발의[[:alpha:]]{1,}","대표발의")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "더불어민주당[[:print:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "도둑[[:alpha:]]{1,}","도둑")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "독재[[:alpha:]]{1,}","독재")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "무효[[:alpha:]]{1,}","무효")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "문제[[:alpha:]]{1,}","문제")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "민주당[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "바른미래당[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "민주평화당[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "반대[[:alpha:]]{1,}","반대")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "발의[[:alpha:]]{1,}","발의")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "방해[[:alpha:]]{1,}","방해")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "보임[[:alpha:]]{1,}","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "부분[[:alpha:]]{1,}","부분")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "불법[[:alpha:]]{1,}","불법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사개특위[[:alpha:]]{1,}","사개특위")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사보임[[:alpha:]]{1,}","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사임[[:alpha:]]{1,}","사임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "생각[[:alpha:]]{1,}","생각")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "신뢰[[:alpha:]]{1,}","신뢰")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "신속[[:alpha:]]{1,}","신속처리안건")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "여러분[[:alpha:]]{1,}","여러분")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "원내대표[[:alpha:]]{1,}","원내대표")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "위반[[:alpha:]]{1,}","위반")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "위법[[:alpha:]]{1,}","위법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "의사진행[[:alpha:]]{1,}","의사진행발언")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "의장[[:alpha:]]{1,}","의장")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "자유한국당[[:print:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "한국당[[:print:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "저희들[[:alpha:]]{1,}","저희들")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "적법[[:alpha:]]{1,}","적법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "참석[[:alpha:]]{1,}","참석")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "character","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가벼","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간첩[[:alpha:]]{1,}","간첩")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강원[[:alpha:]]{1,}","강원랜드")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강화[[:alpha:]]{1,}","강화")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간첩[[:alpha:]]{1,}","간첩")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간첩[[:alpha:]]{1,}","간첩")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국선변호[[:alpha:]]{1,}","국선변호")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가급[[:alpha:]]{1,}","가급적")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가능[[:alpha:]]{1,}","가능")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가당치","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "가사비송이나","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기망행위[[:alpha:]]{1,}","기망행위")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기록[[:alpha:]]{1,}","기록")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기능[[:alpha:]]{1,}","기능")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기관지","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기관[[:alpha:]]{1,}","기관")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기계[[:alpha:]]{1,}","기계")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "근무[[:alpha:]]{1,}","근무")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "근거[[:alpha:]]{1,}","근거")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "그분[[:alpha:]]{1,}","그분")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "권력[[:alpha:]]{1,}","권력")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "군사[[:alpha:]]{1,}","군사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국회법[[:alpha:]]{1,}","국회법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국제인권법연구원[[:alpha:]]{1,}","국제인권법연구회")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국민[[:alpha:]]{1,}","국민")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "구태[[:alpha:]]{1,}","구태")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "구성[[:alpha:]]{1,}","구성")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "교통[[:alpha:]]{1,}","교통")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "교수[[:alpha:]]{1,}","교수")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "교육[[:alpha:]]{1,}","교육")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "공직자[[:alpha:]]{1,}","공직자")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "공정[[:alpha:]]{1,}","공정")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "공익[[:alpha:]]{1,}","공익")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "것[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "거예요","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "견제[[:alpha:]]{1,}","견제")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "공수처[[:alpha:]]{1,}","공수처")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "법관[[:alpha:]]{1,}","법관")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "삼성[[:alpha:]]{1,}","삼성")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "우리[[:alpha:]]{1,}","우리")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "이것[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "인권[[:alpha:]]{1,}","인권")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "전관예우[[:alpha:]]{1,}","전관예우")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "청와대[[:alpha:]]{1,}","청와대")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "판결[[:alpha:]]{1,}","판결")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "간첩[[:alpha:]]{1,}","간첩")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "갈등[[:alpha:]]{1,}","갈등")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "감표위원[[:alpha:]]{1,}","감표위원")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강압[[:alpha:]]{1,}","강압")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강원도[[:alpha:]]{1,}","강원도")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "강원랜드[[:alpha:]]{1,}","강원랜드")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개념[[:alpha:]]{1,}","개념")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개선[[:alpha:]]{1,}","개선")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "개편[[:alpha:]]{1,}","개편")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "객관[[:alpha:]]{1,}","객관")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "거악[[:alpha:]]{1,}","거악")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "거야[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "거지요[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "건데요[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "검거[[:alpha:]]{1,}","검거")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "겁박[[:alpha:]]{1,}","겁박")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "것들[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "것이[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "것인[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "고위[[:alpha:]]{1,}","고위직")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "구속[[:alpha:]]{1,}","구속")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국선[[:alpha:]]{1,}","국선변호")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국제인권법연구회[[:alpha:]]{1,}","국제인권법연구회")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "그것[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "그러[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "국선[[:alpha:]]{1,}","국선변호")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기망행위[[:alpha:]]{1,}","기망행위")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기무부대[[:alpha:]]{1,}","기무사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "기무사[[:alpha:]]{1,}","기무사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "꼼수[[:alpha:]]{1,}","꼼수")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "남북[[:alpha:]]{1,}","남북")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "댓글[[:alpha:]]{1,}","댓글")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "로스쿨[[:alpha:]]{1,}","로스쿨")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "로펌[[:alpha:]]{1,}","로펌")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "말씀드[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "문제[[:alpha:]]{1,}","문제")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "미국[[:alpha:]]{1,}","미국")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "미투[[:alpha:]]{1,}","미투")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "민노총[[:alpha:]]{1,}","민노총")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사개특위[[:alpha:]]{1,}","사개특위")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사ㆍ보임[[:alpha:]]{1,}","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사보임[[:alpha:]]{1,}","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사사보임","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "사ᆞ사보임","사보임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "걱정[[:alpha:]]{1,}","걱정")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "서울중앙지검[[:alpha:]]{1,}","서울중앙지검")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "서울지검[[:alpha:]]{1,}","서울중앙지검")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "선임[[:alpha:]]{1,}","선임")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "성접대[[:alpha:]]{1,}","성접대")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "수사[[:alpha:]]{1,}","수사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "아니[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "아닙[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "어떠[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "어떤[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "어젠다[[:alpha:]]{1,}","어젠다")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "없[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "여러분[[:alpha:]]{1,}","여러분")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "우리법연구회[[:alpha:]]{1,}","우리법연구회")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "이라크[[:alpha:]]{1,}","이라크")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "이러[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "일본[[:alpha:]]{1,}","일본")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "있다[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "저희[[:alpha:]]{1,}","")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "적법[[:alpha:]]{1,}","적법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "적폐[[:alpha:]]{1,}","적폐")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "정당[[:alpha:]]{1,}","정당성")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "적법[[:alpha:]]{1,}","적법")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "제도[[:alpha:]]{1,}","제도")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "조사[[:alpha:]]{1,}","조사")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "지휘[[:alpha:]]{1,}","지휘")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "청와대[[:alpha:]]{1,}","청와대")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "추진[[:alpha:]]{1,}","추진")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "특활비[[:alpha:]]{1,}","특활비")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "표결[[:alpha:]]{1,}","표결")
  n_party$word_done[i] <- str_replace_all(n_party$word_done[i],
                                          "피의사실공표[[:alpha:]]{1,}","피의사실공표")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "회의[[:alpha:]]{1,}","회의")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "위원장[[:alpha:]]{1,}","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "의원","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "위원님","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "그것","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "이것","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "인사[[:alpha:]]{1,}","인사")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "위원","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "확인[[:alpha:]]{1,}","확인")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "진행[[:alpha:]]{1,}","진행")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "유효[[:alpha:]]{1,}","유효")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "민주당","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "바른미래당","")
  n_party$word_done[i] <-  str_replace_all(n_party$word_done[i],
                                           "회의","")
}




# 빈칸 존재
n_party %>%
  select(party, word_done) %>%
  filter(nchar(word_done)>1)  ->
  nn_party

view(nn_party)
