rm(list =ls())

# 2_editing을 기억해보자면
# 우리에게는 df_with_party가 있었고...
# 문서 별 Corpus 만드는 과정을 시도해야 해서 그 친구들을 다시 불러야 한다.
# 정당 별로 문서 나눠준 다음에 모델링 작업해야 한다.

# 솔직히 마음이 아프다. 기껏 명사 기준으로 정리 다 해놨는데...(X고생했는데...)
# 그래도 해보자.

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

# 세상에, 기껏 만들어놓은 변수명이 다 날아갔다.
# 그래도 그냥 하자. 어차피 쪼갤 애들이다.

library(dplyr)

df_with_party %>%
  filter(V3 == "자유한국") %>%
  select(V2) -> Red

df_with_party %>%
  filter(V3 == "더불어민주") %>%
  select(V2) -> Blue

df_with_party %>%
  filter(V3 == "바른미래") %>%
  select(V2) -> Skyblue

df_with_party %>%
  filter(V3 == "공동교섭(정의-민주평화)") %>%
  select(V2) -> Lightgreen
# 세상에, 정의-민주평화는 세 문단 밖에 없네


# 자, 이제 나눠보자

write.table(Red, "C:/Users/ycg00/Documents/Textmining_R/party/Red.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            quote = F)

write.table(Blue, "C:/Users/ycg00/Documents/Textmining_R/party/Blue.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            quote = F)


write.table(Skyblue, "C:/Users/ycg00/Documents/Textmining_R/party/Skyblue.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            quote = F)

write.table(Lightgreen, "C:/Users/ycg00/Documents/Textmining_R/party/Lightgreen.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            quote = F)


