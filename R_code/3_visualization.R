# 텍스트 시각화

View(nn_party)

# 정당 별 발언 
library(gridExtra)

par(mfrow=c(2,2))

# 더불어민주당
nn_party %>%
  filter(party == "더불어민주") %>%
  count(word_done, sort = T) -> n_blue # 워드 클라우드 할 때도 써야 해서

n_blue %>%
  top_n(10) %>%
  ggplot(aes(reorder(word_done, -n), n)) +
  geom_bar(stat = "identity", fill='steelblue') +
  ggtitle("더불어민주당 주요 단어 TOP10") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+
  labs(x = "주요 단어", face="bold") ->a


# 자유한국당  
nn_party %>%
  filter(party == "자유한국") %>%
  count(word_done, sort = T) -> n_red

n_red %>%
  top_n(10) %>%
  ggplot(aes(reorder(word_done, -n), n)) +
  geom_bar(stat = "identity", fill='darkred') +
  ggtitle("자유한국당 주요 단어 TOP10") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkred"))+
  labs(x = "주요 단어", face="bold") -> b


# 바른미래당
nn_party %>%
  filter(party == "바른미래") %>%
  count(word_done, sort = T) -> n_skyblue

n_skyblue %>%
  top_n(10) %>%
  ggplot(aes(reorder(word_done, -n), n)) +
  geom_bar(stat = "identity", fill='skyblue') +
  ggtitle("바른미래당 주요 단어 TOP10") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "skyblue"))+
  labs(x = "주요 단어", face="bold") -> c


# 공동교섭
nn_party %>%
  filter(party == "공동교섭(정의-민주평화)") %>%
  count(word_done, sort = T) -> n_lightgreen

n_lightgreen %>%
  top_n(10) %>%
  ggplot(aes(reorder(word_done, -n), n)) +
  geom_bar(stat = "identity", fill='lightgreen') +
  ggtitle("공동교섭(정의-민주평화)당 주요 단어 TOP10") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "lightgreen"))+
  labs(x = "주요 단어", face="bold") -> d


grid.arrange(a, b, c, d, nrow=2, ncol=2)


# 워드 클라우드
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(6, "Paired")

par(mfrow=c(2,2))

# 더불어민주당
wordcloud(words = n_blue$word_done,
          freq = n_blue$n,
          min.freq = 10,
          max.words = 100,
          random.order = F,
          random.color = F,
          scale = c(10,1),
          colors = brewer.pal(6, "Blues"))

# 자유한국당
wordcloud(words = n_red$word_done,
          freq = n_red$n,
          min.freq = 10,
          max.words = 100,
          random.order = F,
          random.color = F,
          scale = c(10,1),
          colors = brewer.pal(6, "Reds"))

# 바른미래당
wordcloud(words = n_skyblue$word_done,
          freq = n_skyblue$n,
          min.freq = 3,
          max.words = 100,
          random.order = F,
          random.color = F,
          scale = c(10,1),
          colors = brewer.pal(6, "GnBu"))

# 교섭단체(정의-민주평화)
wordcloud(words = n_lightgreen$word_done,
          freq = n_lightgreen$n,
          min.freq = 10,
          max.words = 100,
          random.order = F,
          random.color = F,
          scale = c(10,1),
          colors = brewer.pal(6, "Greens"))