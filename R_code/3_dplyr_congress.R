rm(list =ls())


# install.packages('RmecabKo')
# install.packages('devtools')
# devtools::install_github("junhewk/RmecabKo", INSTALL_opts=c('--no-lock'))

# install.packages('RcppMeCab')
# install.packages('tidytext')
# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('purrr')
# install.packages('magrittr')
# install.packages('tidyverse')
# install.packages('rJava')
# # devtools::install_github('haven-jeon/KoNLP'), KoNLP가 그냥 인스톨로는 진행하기 어려운 상황
# library(rJava) # java 설치할 때, R 32/64 맞춰서 설치하삼


pkgs <- c('RmecabKo','devtools','RcppMeCab','tidytext','purrr','dplyr','tidyr','magrittr','tidyverse', 'rJava', 'KoNLP')
sapply(pkgs,require,character.only = TRUE)




df <- read.csv("~/assembly_record/Textmining_R/editing/df_with_party.txt",
               sep = "\t",
               header = T)

View(df)
str(df)

# ment 부분의 데이터 프레임을 factor -> char 로 바꿔준다

df$ment <- as.character(df$ment)
df$party <- as.character(df$party)

str(df)



# 토큰화 통해서 정리
#token_party<- df %>%
#    select(name, ment, party) %>%
#    unnest_tokens(input = ment,
#                  output = word,
#                  token = SimplePos09)  



# View(token_party)

## 명사만 추출

#token_party %>%
#  filter(grepl("/n", word)) %>% 
##  mutate(word = gsub("/.*$", "", word)) %>%
#  filter(nchar(word) > 1)->
#  n_word

View(n_word)


# 당별로 발언 정리


df %>%
  filter(party == "자유한국") %>%
  select(ment) ->
  red_n

red_new <- paste(red_n$ment, collapse =" ")
View(red_new)

View(red_n)

df %>%
  filter(party == "더불어민주") %>%
  select(ment) ->
  blue_n

blue_new <- paste(blue_n$ment, collapse =" ")

dim(blue_new)


df %>%
  filter(party == "바른미래") %>%
  select(ment) ->
  skyblue_n

skyblue_new <- paste(skyblue_n$ment, collapse =" ")


df %>%
  filter(party == "공동교섭(정의-민주평화)") %>%
  select(ment) ->
  yellow_n


yellow_new <- paste(yellow_n$ment, collapse =" ")

### ------------ ###



write.table(red_new, "~/assembly_record/Textmining_R/party/red.txt",
            row.names = FALSE,
            quote = FALSE,
            header = F)

write.table(blue_new, "~/assembly_record/Textmining_R/party/blue.txt",
            row.names = FALSE,
            quote = FALSE,
            header = F)

write.table(skyblue_new, "~/assembly_record/Textmining_R/party/skyblue.txt",
            row.names = FALSE,
            quote = FALSE)


write.table(yellow_new, "~/assembly_record/Textmining_R/party/yellow.txt",
            row.names = FALSE,
            quote = FALSE)


