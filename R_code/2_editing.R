# 합친 회의록 데이터를 정리하자

df <- read.csv("~/assembly_record/Textmining_R/editing/law_all.txt",
                 sep = "\t",
                 header = F)

head(df)


library(stringr)
# V1 에서
df1 <- str_replace_all(df$V1, "&#9711;", "")
df2 <- str_replace_all(df1, " 위원", "")
df3 <- str_replace_all(df2, " 의원", "")
df4 <- str_replace_all(df3, "\\(", "")
df5 <- str_trim(df4, side = c("both"))

# 해당 회의에 참여한 인물들을 알 수 있음
table(df5)


# 각 의원의 정당을 맞춰서 칼럼을 추가하자

df_party <- data.frame(df5 = c("강효상", "곽상도", "권은희", "김도읍", "노회찬", "박범계", "박주미", "박지원", "백혜련", "송기석", "송기헌", "안호영", "여상규", "염동열", "오신환", "윤상직", "윤한홍", "이상민", "李恩宰", "이장우", "이재정", "이종걸", "이철규", "이철희", "임재훈", "장제원", "鄭宗燮", "정태옥", "조응천", "지상욱", "진선미", "채이배", "표창원", "咸珍圭"),
                  party = c("자유한국", "자유한국", "바른미래", "자유한국", "정의", "더불어민주", "정의", "국민의", "더불어민주", "국민의", "더불어민주","더불어민주", "자유한국", "자유한국", "바른미래", "자유한국", "자유한국", "더불어민주", "자유한국", "자유한국", "더불어민주", "더불어민주", "자유한국", "더불어민주", "바른미래","자유한국","자유한국", "자유한국","더불어민주","바른미래","더불어민주","바른미래","더불어민주", "자유한국"))



# V2 에서
df6 <- str_replace_all(df$V2, "&#8228;", "")



# 정리가 끝난 각 컬럼을 재결합
df_new <- cbind(df5, df6)


df_nnew <- merge(df_new, df_party, by ='df5', all.x=T)

table(df_nnew$party)

# 당없는 사람들과 위원장은 다 제거하자
# party가 없는 NA상태의 행 모두 제거


df_with_party <- na.omit(df_nnew)

# 열 이름 바꾸기

colnames(df_with_party) = c("name", "ment", "party")


# editing 끝난 데이터 테이블 저장

write.table(df_with_party,
            "~/assembly_record/Textmining_R/editing/df_with_party.txt",
            sep = "\t",
            row.names = F,
            col.names = T,
            quote = F)