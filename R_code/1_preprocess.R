rm(list =ls())

setwd('~/assembly_record/Textmining_R/law')

pkgs <- c('dplyr','tidyr','stringr','tm')
sapply(pkgs,require,character.only = TRUE)


src_dir <-c("~/assembly_record/Textmining_R/law")
src_dir

src_file <-list.files(src_dir)
src_file

src_file_cnt <- length(src_file)
src_file_cnt



for (i in 1:src_file_cnt) {
  
  df <- readLines(paste(src_dir, "/", src_file[i], sep = ""))
  df <- df[df != '']
  
  open <- str_which(df,'\\d{2}시\\d{2}분 개의')
  clsd <- str_which(df,'\\d{2}시\\d{2}분 산회')
  
  df <- df[open : clsd]
  df <- lapply(df,str_trim)
  
  aa <- str_which(df,'&#9711') %>% as.numeric()
  bb <- c((str_which(df,'&#9711')[-1])-1,length(df)) %>% as.numeric()
  
  ttt <- list()
  ttt <- sapply(1:length(aa),function(x)
    df[aa[x]:bb[x]]
  )
  tt <- lapply(ttt,str_c,collapse = " ")
  
  sepp <- lapply(tt,function(x)
    (str_locate_all(x,' ')[[1]][,1])[2]-1) %>% unlist()
  
  name <- sapply(1:length(sepp),function(x)
    str_sub(tt[[x]],1,sepp[x])
  )
  
  ment <- sapply(1:length(sepp),function(x)
    str_sub(tt[[x]],sepp[x]+2,str_length(tt[[x]])))
  
  res <- data.frame(name = name,
                    ment = ment)
  
  write.table(res,
              paste(src_dir, "/", "~/assembly_record/Textmining_R/editing/law_all.txt", sep = ""),
              sep = "\t",
              row.names = F,
              col.names = F,
              quote = F,
              append = T)
  
  rm(res)
  
  print(i)

}