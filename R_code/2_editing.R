rm(list =ls())


# 합친 회의록 데이터를 정리하자

df <- read.csv("~/assembly_record/Textmining_R/editing/law_all.txt",
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
View(df_with_party)

# editing 끝난 데이터 테이블 저장

write.table(df_with_party,
            "~/assembly_record/Textmining_R/editing/df_with_party.txt",
            sep = "\t",
            row.names = F,
            col.names = T,
            quote = F)
