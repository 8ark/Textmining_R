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

pkgs <- c('RmecabKo','devtools','RcppMeCab','tidytext','purrr','dplyr','tidyr','magrittr','tidyverse')
sapply(pkgs,require,character.only = TRUE)


library(rJava) # java 설치할 때, R 32/64 맞춰서 설치하삼
library(KoNLP) # # devtools::install_github('haven-jeon/KoNLP')


df <- read.csv("~/assembly_record/Textmining_R/editing/df_with_party.txt",
               sep = "\t",
               header = T)

View(df)
str(df)

# ment 부분의 데이터 프레임을 factor -> char 로 바꿔준다

df$ment <- as.character(df$ment)
str(df)



# 토큰화 통해서 정리
token_party<- df %>%
    select(name, ment, party) %>%
    unnest_tokens(input = ment,
                  output = word,
                  token = SimplePos09)  


View(token_party)

## 명사만 추출

token_party %>%
  filter(grepl("/n", word)) %>% 
  mutate(word = gsub("/.*$", "", word)) %>%
  filter(nchar(word) > 1)->
  n_word

View(n_word)



## 당별로 발언 정리

n_word %>%
  filter(party == "자유한국") %>%
  count(word, sort = T) -> 
  red_n

n_word %>%
  filter(party == "더불어민주") %>%
  count(word, sort = T) -> 
  blue_n

n_word %>%
  filter(party == "바른미래") %>%
  count(word, sort = T) -> 
  skyblue_n

n_word %>%
  filter(party == "민주평화") %>%
  count(word, sort = T) -> 
  green_n

n_word %>%
  filter(party == "정의") %>%
  count(word, sort = T) -> 
  yellow_n

### ------------ ###

table(blue_n$word)
table(green_n$word)
table(skyblue_n$word)
table(red_n$word)
table(yellow_n$word)
## '위원장', '위원장님' 이라는 단어 많이 사용, 위원님 도.
## 이런 단어는 분석에 의미없는단어이니 제거
## '회의' '의원', '그것', '이것', '오늘', '각 당 이름?', '거예요', '때문'

whole_party_1 <- list(blue_n,green_n,skyblue_n,red_n, yellow_n)

whole_party

filter_blue <- subset(blue_n,word != "위원")
filter_blue <- subset(filter_blue,word != "위원장님")
filter_blue <- subset(filter_blue,word != "위원님")
filter_blue <- subset(filter_blue,word != "그것")
filter_blue <- subset(filter_blue,word != "이것")
filter_blue <- subset(filter_blue,word != "의원님")
filter_blue <- subset(filter_blue,word != "때문")
filter_blue <- subset(filter_blue,word != "오늘")

filter_green <- subset(green_n,word != "위원")
filter_green <- subset(filter_green,word != "위원장")
filter_green <- subset(filter_green,word != "위원장님")
filter_green <- subset(filter_green,word != "위원님")
filter_green <- subset(filter_green,word != "그것")
filter_green <- subset(filter_green,word != "이것")
filter_green <- subset(filter_green,word != "의원님")
filter_green <- subset(filter_green,word != "때문")
filter_green <- subset(filter_green,word != "오늘")

filter_skyblue <- subset(skyblue_n,word != "때문")
filter_skyblue <- subset(filter_skyblue,word != "위원")
filter_skyblue <- subset(filter_skyblue,word != "위원장")
filter_skyblue <- subset(filter_skyblue,word != "위원장님")
filter_skyblue <- subset(filter_skyblue,word != "위원님")
filter_skyblue <- subset(filter_skyblue,word != "그것")
filter_skyblue <- subset(filter_skyblue,word != "이것")
filter_skyblue <- subset(filter_skyblue,word != "의원님")
filter_skyblue <- subset(filter_skyblue,word != "때문")
filter_skyblue <- subset(filter_skyblue,word != "오늘")

filter_red <- subset(red_n,word != "때문")
filter_red <- subset(filter_red,word != "위원")
filter_red <- subset(filter_red,word != "위원장")
filter_red <- subset(filter_red,word != "위원장님")
filter_red <- subset(filter_red,word != "위원님")
filter_red <- subset(filter_red,word != "그것")
filter_red <- subset(filter_red,word != "이것")
filter_red <- subset(filter_red,word != "의원님")
filter_red <- subset(filter_red,word != "때문")
filter_red <- subset(filter_red,word != "오늘")
filter_red <- subset(filter_red,word != "거예요")


filter_blue
filter_green
filter_skyblue
filter_red

ranking <- data.frame(filter_blue$word, filter_green$word, filter_skyblue$word,
                      filter_red$word)

sq <- seq(max(length(filter_blue$word), length(filter_green$word), length(filter_skyblue$word), length(filter_red$word)))

Ranking <-data.frame(filter_blue$word[sq], filter_green$word[sq],filter_skyblue$word[sq],filter_red$word[sq]) 

View(Ranking)
## 검경소위 = 검경
## 공수처법 = 고위공직자비리수사처법 = 고위공직자
## 원천무효 = 무효
## 부정부패 = ...도?


n_word_1 <- subset(n_word,word != "때문")
n_word_1 <- subset(n_word_1,word != "위원")
n_word_1 <- subset(n_word_1,word != "위원장")
n_word_1 <- subset(n_word_1,word != "위원장님")
n_word_1 <- subset(n_word_1,word != "위원님")
n_word_1 <- subset(n_word_1,word != "그것")
n_word_1 <- subset(n_word_1,word != "이것")
n_word_1 <- subset(n_word_1,word != "의원님")
n_word_1 <- subset(n_word_1,word != "때문")
n_word_1 <- subset(n_word_1,word != "오늘")
n_word_1 <- subset(n_word_1,word != "거예요")
n_word_1 <- subset(n_word_1,word != "회의")

n_word_1




n_word_1 %>%
  filter(party == "더불어민주") -> blue_party

blue_ment <- blue_party$word


n_word_1 %>%
  filter(party == "민주평화") -> green_party

green_ment <- green_party$word

n_word_1 %>%
  filter(party == "바른미래") -> skyblue_party

skyblue_ment <- skyblue_party$word

n_word_1 %>%
  filter(party == "자유한국") -> red_party

red_ment <- red_party$word



# 저장 #

write.table(red_ment, "~/project/test/textmining_news/red.txt",
            row.names = FALSE,
            quote = FALSE)

write.table(blue_ment, "~/project/test/textmining_news/blue.txt",
            row.names = FALSE,
            quote = FALSE)

write.table(skyblue_ment, "~/project/test/textmining_news/skyblue.txt",
            row.names = FALSE,
            quote = FALSE)

write.table(green_ment, "~/project/test/textmining_news/green.txt",
            row.names = FALSE,
            quote = FALSE)

