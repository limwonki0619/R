setwd('C:/Users/limwk/Desktop/hyundai_all')
getwd()

library(syuzhet)
library(tm)
library(tidytext)
library(qdap)
library(tidyverse)
library(wordcloud)
library(tibble)
library(plotrix)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggraph)
library(widyr)
library(tidyr)
library(igraph)
library(reshape2)
install.packages("reshape2")
install.packages("igraph")
install.packages("tidyr")
install.packages("ggraph")
install.packages("widyr")
install.packages("plotly")
library(plotly)

tweets_1418_set <- rbind(hyundai_2014_total,hyundai_2015_total,hyundai_2016_total,
                         hyundai_2017_total,hyundai_2018_total[,1:10])

spliter <- function(contents){
  contents <- str_split_fixed(contents,"https://",2)[,1]
  contents <- str_split_fixed(contents,"http://",2)[,1]
  contents <- str_split_fixed(contents,"pic",2)[,1]
} #https, http, pic 기준으로 나누기

split_text_1418 <- spliter(tweets_1418_set$text)
stop_words_list <- c(readLines("word_to_remove.txt"), stopwords("english")) #나의 불용어 사전 로드 
#----------------------
clean_text <- function(text){
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  text <- bracketX(text) #괄호 내 모든 텍스트 제거
  text <- replace_abbreviation(text) #축약어를 대응되는 전체문자로 풀어냄
  text <- replace_contraction(text) #단어 축약을 원래 상태로 되돌림
  text <- replace_symbol(text) #일반 기호를 대응되는 단어로 교체 
  text <- tolower(text)
  text <- str_replace_all(text,"[^a-zA-Z\\s]", " ") #알파벳을 제외한 나머지 모두 삭제
  text <- str_replace_all(text, "santa cruz", "santacruz")
  text <- str_replace_all(text, "santa fe", "santafe")
  text <- str_replace_all(text, "cars", "car")
  text <- str_replace_all(text, "hyundaiglobal", "hyundai")
  text <- str_replace_all(text, "hyundaibluelink", "bluelink")
  text <- str_replace_all(text, "blue link", "bluelink")
  text <- str_replace_all(text, "hyundai sweepstakes", "hyundaisweepstakes")
  text <- str_replace_all(text, "hyundaisweepstakes", "hyundai sweepstakes")
  text <- str_replace_all(text, "boycottsouthkorea", "")
  text <- str_replace_all(text, "boycottpteongchangolym", "")
  text <- str_replace_all(text, "becausefutboll", "")
  text <- str_replace_all(text, "samsung", "")
  text <- str_replace_all(text, "lg", "")
  text <- str_replace_all(text, "footboll", "")
  text <- str_replace_all(text, "superbowl", "")
  text <- str_replace_all(text, "super bwol", "")
  text <- stemDocument(text)
  text <- removeWords(text,stop_words_list)
  return(text)
} #변경 시 check
#----------------------
clean_df <- data.frame(date_time = tweets_1418_set$timestamp,
                       tweet_text = split_text_1418)
start_date <- as.POSIXct('2014-01-01 00:00:00')
end_date <- as.POSIXct('2018-10-31 00:00:00')

#flood_tweets = hyundai_tweets
clean_tweets_1418 <- clean_df %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d")) %>%
  filter(date_time >= start_date & date_time <= end_date ) %>% 
  mutate(tweet_text = clean_text(split_text_1418)) %>%
  mutate(year = substr(date_time,1,4)) %>% 
  mutate(month = substr(date_time,6,7)) %>% 
  mutate(day = substr(date_time,9,10))

tokenize <- function(clean_tweets_1418){ 
  clean_tweets_1418 %>%
    dplyr::select(tweet_text,year,month,day) %>% 
    unnest_tokens(word, tweet_text) %>% 
    anti_join(stop_words) %>% 
    filter(!word %in% stop_words_list) 
} #토큰화

tweet_clean_1418 <- tokenize(clean_tweets_1418)

clean_tweets_14 <- filter(clean_tweets_1418,year==2014)
clean_tweets_15 <- filter(clean_tweets_1418,year==2015)
clean_tweets_16 <- filter(clean_tweets_1418,year==2016)
clean_tweets_17 <- filter(clean_tweets_1418,year==2017)
clean_tweets_18 <- filter(clean_tweets_1418,year==2018)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stop_words_list)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}
make_corpus <- function(text){
  text_clean <- clean_text(text)
  text_source <- VectorSource(text_clean)
  text_corpus <- VCorpus(text_source)
  corpus <- clean_corpus(text_corpus)
}
#--------------------- 연관어

#최근 5년간 긍정단어 뽑기 
positive_text <- tweet_clean_1418 %>% filter(date2 >= "2014-01-01" & date2 <= "2018-11-01")  %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% filter(sentiment=="positive")

negative_text <- tweet_clean_1418 %>% filter(date2 >= "2014-01-01" & date2 <= "2018-11-01")  %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% filter(sentiment=="negative")

as.character(positive_text$word)
as.character(negative_text$word)


{clean_tweets_1418_1 <- clean_tweets_1418[grep("love",clean_tweets_1418$tweet_text),]
clean_tweets_1418_2 <- clean_tweets_1418[grep("support",clean_tweets_1418$tweet_text),]
clean_tweets_1418_3 <- clean_tweets_1418[grep("nice",clean_tweets_1418$tweet_text),]
clean_tweets_1418_4 <- clean_tweets_1418[grep("sweet",clean_tweets_1418$tweet_text),]
clean_tweets_1418_5 <- clean_tweets_1418[grep("cool",clean_tweets_1418$tweet_text),]
clean_tweets_1418_6 <- clean_tweets_1418[grep("top",clean_tweets_1418$tweet_text),]
clean_tweets_1418_7 <- clean_tweets_1418[grep("fun",clean_tweets_1418$tweet_text),]
clean_tweets_1418_8 <- clean_tweets_1418[grep("wow",clean_tweets_1418$tweet_text),]
clean_tweets_1418_9 <- clean_tweets_1418[grep("afford",clean_tweets_1418$tweet_text),]
clean_tweets_1418_10 <- clean_tweets_1418[grep("safe",clean_tweets_1418$tweet_text),]
clean_tweets_1418_11 <- clean_tweets_1418[grep("won",clean_tweets_1418$tweet_text),]
clean_tweets_1418_12 <- clean_tweets_1418[grep("free",clean_tweets_1418$tweet_text),]
clean_tweets_1418_13 <- clean_tweets_1418[grep("honor",clean_tweets_1418$tweet_text),]
clean_tweets_1418_14 <- clean_tweets_1418[grep("proud",clean_tweets_1418$tweet_text),]
clean_tweets_1418_15 <- clean_tweets_1418[grep("award",clean_tweets_1418$tweet_text),]} #긍정 

{clean_tweets_1418_11 <- clean_tweets_1418[grep("bad",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_21 <- clean_tweets_1418[grep("worst",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_31 <- clean_tweets_1418[grep("disappoint",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_41 <- clean_tweets_1418[grep("die",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_51 <- clean_tweets_1418[grep("fail",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_61 <- clean_tweets_1418[grep("limit",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_71 <- clean_tweets_1418[grep("suck",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_81 <- clean_tweets_1418[grep("lie",clean_tweets_1418$tweet_text),]
 clean_tweets_1418_91 <- clean_tweets_1418[grep("hate",clean_tweets_1418$tweet_text),]
clean_tweets_1418_101 <- clean_tweets_1418[grep("miss",clean_tweets_1418$tweet_text),]
clean_tweets_1418_111 <- clean_tweets_1418[grep("wrong",clean_tweets_1418$tweet_text),]
clean_tweets_1418_121 <- clean_tweets_1418[grep("fuck",clean_tweets_1418$tweet_text),]
clean_tweets_1418_131 <- clean_tweets_1418[grep("defect",clean_tweets_1418$tweet_text),]
clean_tweets_1418_141 <- clean_tweets_1418[grep("poor",clean_tweets_1418$tweet_text),]
clean_tweets_1418_151 <- clean_tweets_1418[grep("kill",clean_tweets_1418$tweet_text),]}#부정


clean_tweets_1418_negative <- rbind( clean_tweets_1418_11,
                                     clean_tweets_1418_21,
                                     clean_tweets_1418_31,
                                     clean_tweets_1418_41,
                                     clean_tweets_1418_51,
                                     clean_tweets_1418_61,
                                     clean_tweets_1418_71,
                                     clean_tweets_1418_81,
                                     clean_tweets_1418_91,
                                    clean_tweets_1418_101,
                                    clean_tweets_1418_111,
                                    clean_tweets_1418_121,
                                    clean_tweets_1418_131,
                                    clean_tweets_1418_141,
                                    clean_tweets_1418_151)

negative_corpus_1418 <- make_corpus(clean_tweets_1418_negative$tweet_text) #corpus 생성
negative_top_word_1418 <-  top_30_words(negative_corpus_1418)
negative_co.matrix_1418 <- negative_top_word_1418   %*% t(negative_top_word_1418)

install.packages("qgraph")
library(qgraph)

par(family="Apple SD Gothic Neo")
qgraph(negative_co.matrix_1418, labels=rownames(negative_co.matrix_1418 ),
       diag=FALSE, 
       layout='spring', #관련단어가 가까이 나타남
       threshold=3,
       vsize=log(diag(negative_co.matrix_1418 ))*1.8,
       label.cex=1.5)




clean_tweets_1418_service <- clean_tweets_1418[grep("servic",clean_tweets_1418$tweet_text),]
clean_tweets_14_service <- clean_tweets_14[grep("servic",clean_tweets_14$tweet_text),]
clean_tweets_15_service <- clean_tweets_15[grep("servic",clean_tweets_15$tweet_text),]
clean_tweets_16_service <- clean_tweets_16[grep("servic",clean_tweets_16$tweet_text),]
clean_tweets_17_service <- clean_tweets_17[grep("servic",clean_tweets_17$tweet_text),]
clean_tweets_18_service <- clean_tweets_18[grep("servic",clean_tweets_18$tweet_text),]

corpus_14 <- make_corpus(clean_tweets_14$tweet_text) #corpus 생성
corpus_15 <- make_corpus(clean_tweets_15$tweet_text) #corpus 생성
corpus_16 <- make_corpus(clean_tweets_16$tweet_text) #corpus 생성
corpus_17 <- make_corpus(clean_tweets_17$tweet_text) #corpus 생성
corpus_18 <- make_corpus(clean_tweets_18$tweet_text) #corpus 생성

word_freq <- function(corpus){
  doc_tdm <- TermDocumentMatrix(corpus)
  doc_m <- as.matrix(doc_tdm)
  doc_term_freq <- rowSums(doc_m)
  doc_word_freqs <- data.frame(word = names(doc_term_freq),num = doc_term_freq) %>% arrange(desc(num))
  return(doc_word_freqs)
} #워드클라우드요

service_corpus_1418 <- make_corpus(clean_tweets_1418_service$tweet_text)
service_corpus_14 <- make_corpus(clean_tweets_14_service$tweet_text)
service_corpus_15 <- make_corpus(clean_tweets_15_service$tweet_text)
service_corpus_16 <- make_corpus(clean_tweets_16_service$tweet_text)
service_corpus_17 <- make_corpus(clean_tweets_17_service$tweet_text)
service_corpus_18 <- make_corpus(clean_tweets_18_service$tweet_text)

top_30_words <- function(corpus){
  tdm <- TermDocumentMatrix(corpus, control=list(wordLengths=c(2, 10), weighting=weightBin))
  tdm.matrix <- as.matrix(tdm)
  word.count <- rowSums(tdm.matrix)
  word.order <- order(word.count, decreasing=TRUE)
  freq.words <- tdm.matrix[word.order[1:30],]
  return(freq.words)} #동시출현단어용

service_top_word_1418 <-  top_30_words(service_corpus_1418)
service_top_word_14 <-  top_30_words(service_corpus_14)
service_top_word_15 <-  top_30_words(service_corpus_15)
service_top_word_16 <-  top_30_words(service_corpus_16)
service_top_word_17 <-  top_30_words(service_corpus_17)
service_top_word_18 <-  top_30_words(service_corpus_18)

service_co.matrix_1418 <- service_top_word_1418   %*% t(service_top_word_1418)
service_co.matrix_14 <- service_top_word_14   %*% t(service_top_word_14)
service_co.matrix_15 <- service_top_word_15   %*% t(service_top_word_15)
service_co.matrix_16 <- service_top_word_16   %*% t(service_top_word_16)
service_co.matrix_17 <- service_top_word_17   %*% t(service_top_word_17)
service_co.matrix_18 <- service_top_word_18   %*% t(service_top_word_18)

strwrap(corpus_14[[2]]) #코퍼스 확인

install.packages("qgraph")
library(qgraph)

par(family="Apple SD Gothic Neo")
qgraph(service_co.matrix_1418, labels=rownames(service_co.matrix_1418 ),
       diag=FALSE, 
       layout='spring', #관련단어가 가까이 나타남
       threshold=3,
       vsize=log(diag(service_co.matrix_1418 ))*1.8,
       label.cex=1.5)


#한번에 해보기

str(tweet_clean_1418)
tweet_clean_1418$date <- as.Date(paste0(tweet_clean_1418$year,"-",tweet_clean_1418$month,"-",tweet_clean_1418$day))
tweet_clean_1418$date2 <- as.Date(paste0(tweet_clean_1418$year,"-",tweet_clean_1418$month,"-01"))

#연도별 긍부정 감정(afinn)
k <- rbind(afinn_score_14,afinn_score_15,afinn_score_16,afinn_score_17,afinn_score_18) 
kk <- data.frame(k,year=c(2014,2015,2016,2017,2018))

month_score_data <- read.csv("hyundai_month_afinn_score_14_18.csv",header=T)
year_score_data <- read.csv("hyundai_year_afinn_score_14_18.csv",header=T)

#연도별 긍부정 감정점수 (afinn)
tweet_clean_1418  %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(year>= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>%
  mutate(year_score = (`-5`*-5)+(`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)+(`5`*5)) %>% 
  mutate(n = (`-5`+`-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`+`5`)) %>% 
  mutate(year_score2 = year_score/n) %>%
  mutate(max_ratio = max(year_score2)/max(year_score_data$us_year_sales)*2) %>%
  ggplot(aes(x = as.numeric(year))) +
    geom_line(aes(y=0)) +
    geom_line(aes(y=year_score2),colour="coral",size=0.75) +
    geom_line(aes(y=year_score_data$us_year_sales*max_ratio),colour="Mediumaquamarine",size=0.75) +
    geom_point(aes(y=year_score2),colour="red",size=2)+
    geom_point(aes(y=year_score_data$us_year_sales*max_ratio),colour="MediumTurquoise",size=3)+
  scale_y_continuous(sec.axis= sec_axis(~.*100, name="미국 판매량 (만 대)" )) +
  theme_bw() +
  labs(y="감정지수 (by afinn)",x="Year",colour="gray20",title="연도별 현대차 판매량과 감정지수 비교",
       caption = "출처 = 현대차 공식 홈페이지 - 미국 현지 판매량") +
  theme(axis.title.y=element_text(angle=90, face="italic", colour="black",size=16)) +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 30, color = "gray20"))+
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "gray20"))


score_data <- read.csv("hyundai_afinn_score_14_18.csv",header=T)

#월별 긍부정 감정(afinn)
tweet_clean_1418  %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(date2 >= "2017-10-01" & date2 <= "2018-11-01")  %>%
  count(date2, score) %>%
  spread(score, n, fill = 0) %>%
  mutate(sentiment = (`-5`*-5)+(`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)) %>% 
  mutate(n = (`-5`+`-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`)) %>% 
  mutate(score2 = sentiment/n) %>% 
  mutate(max_ratio = max(score2)/max(month_score_data$us_sales)) %>%
   ggplot(aes(x=date2)) +
     geom_line(aes(y=0),colour="gray10",size=.5) +
     geom_line(aes(y=score2),colour="coral",size=0.75) +
        geom_point(aes(y=score2),colour="red",size=2) + 
        #stat_smooth(aes(y=score2),method=lm,level=0,colour="gray20",size=.75) +
     geom_line(aes(y=month_score_data$us_sales*max_ratio),colour="Mediumaquamarine",size=0.75) +
        geom_point(aes(y=month_score_data$us_sales*max_ratio),colour="Mediumturquoise",size=3) + 
        #stat_smooth(aes(y=month_score_data$us_sales*max_ratio),method=lm,level=0,colour="gray20",size=.75) +
  scale_y_continuous(sec.axis= sec_axis(~.*10, name="미국 판매량 (만 대)" )) +
  theme_bw() +
    labs(y="감정지수 (by afinn)",x="Date",colour="gray20",title="월별 현대차 판매량과 감정지수 비교",
       caption = "출처 = 현대차 공식 홈페이지 - 미국 현지 판매량") +
    theme(axis.title.y=element_text(angle=90, face="italic", colour="black",size=16)) +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 30, color = "gray20"))+
    theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "gray20"))

#일별 감정점수
tweet_clean_1418  %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(date >= "2014-01-01" & date <= "2018-11-01")  %>%
  count(date, score) %>%
  spread(score, n, fill = 0) %>%
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`) %>% 
  mutate(score = sentiment/n) %>% 
  ggplot(aes(date, score, fill = score>0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("red","#27408b")) +
  theme_bw() +
    labs(y="감정지수 (by afinn)",x="Date",colour="gray20",title="2017.03.01~03.31 현대차 소비자 감정지수 (daily)") +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "gray20")) +
    ylim(-5,5)
  

#일별 감정 빈도 분석 
tweet_clean_1418 %>% filter(date >= "2014-01-01" & date <= "2018-11-01") %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n),hjust=1.5,size=5,colour="gray20")  +
  facet_wrap(~sentiment, scales = "free_y") +
   labs(title = paste("감정단어분석 2017년 3월"),y = "빈도") +
    theme_bw()+
    coord_flip()+
      theme(plot.title = element_text(face = "bold",size = 15)) + #플랏 타이트
      theme(axis.text = element_text(colour = "gray20",size = 15, face = "bold")) + #축 레이블 크기
      theme(axis.title.x=element_text(size=18))





#일별 빈도 분석 
tweet_clean_1418 %>% filter(date >= "2017-03-01" & date <= "2017-04-01") %>% 
  filter(!word %in% stop_words_list) %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n),hjust=1.5,size=5,colour="gray20")  +
  labs(title = paste("단어 빈도분석 2017년 3월"),y = "빈도") +
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(face = "bold",size = 15)) + #플랏 타이트
  theme(axis.text = element_text(colour = "gray20",size = 15, face = "bold")) + #축 레이블 크기
  theme(axis.title.x=element_text(size=18))+
  ylim(0,50)

#감정 워드클라우드 
tweet_clean_1418 %>% filter(date >= "2017-03-01" & date <= "2017-04-01") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 100, scale=c(1,.8), rot.per=.5,
                   radom.order=F, colors=c('Orange2','skyblue4'))

# 월별 점수분포 
tweet_clean_1418  %>%
  filter(word %in% "servic") %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`) %>% 
  mutate(score = sentiment/n) 


#최근 5년간 긍정단어 뽑기 
positive_text <- tweet_clean_1418 %>% filter(date2 >= "2014-01-01" & date2 <= "2018-11-01") %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% filter(sentiment=="positive")

negative_text <- tweet_clean_1418 %>% filter(date2 >= "2014-01-01" & date2 <= "2018-11-01")  %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% filter(sentiment=="negative")


tweet_clean_1418 %>% filter(date2 >= "2014-01-01" & date2 <= "2018-11-01")  %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n),hjust=-.25,size=5,colour="gray20")  +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = paste("감정단어분석 2017년 3월"),y = "빈도") +
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(face = "bold",size = 15)) + #플랏 타이트
  theme(axis.text = element_text(colour = "gray20",size = 13, face = "bold")) + #축 레이블 크기
  theme(axis.title.x=element_text(size=15))

write(as.character(positive_text$word),"positive_text.txt",sep="\n",append=F)
write(as.character(negative_text$word),"negative_text.txt",sep="\n",append=F)

as.character(positive_text$word)
as.character(negative_text$word)

service_token_1418
service_token_14 <- filter(service_token_1418,year==2014)
service_token_15 <- filter(service_token_1418,year==2015)
service_token_16 <- filter(service_token_1418,year==2016)
service_token_17 <- filter(service_token_1418,year==2017)
service_token_18 <- filter(service_token_1418,year==2018)

tweet_clean_14 <- filter(tweet_clean_1418,year==2014)
tweet_clean_15 <- filter(tweet_clean_1418,year==2015)
tweet_clean_16 <- filter(tweet_clean_1418,year==2016)
tweet_clean_17 <- filter(tweet_clean_1418,year==2017)
tweet_clean_18 <- filter(tweet_clean_1418,year==2018)

tweet_clean_1418  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`) %>% 
  mutate(score = sentiment/n) 

service_token_14  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`) %>% 
  mutate(score = sentiment/n) 

service_token_15  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`) %>% 
  mutate(score = sentiment/n) 

service_token_16  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`) %>% 
  mutate(score = sentiment/n) 

service_token_17  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)+(`4`*4)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`+`4`) %>% 
  mutate(score = sentiment/n)

service_token_18  %>% 
  inner_join(get_sentiments("afinn")) %>%
  filter(year >= "2014" & year <= "2018")  %>%
  count(year, score) %>%
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4)+(`-3`*-3)+(`-2`*-2)+(`-1`*-1)+(`1`*1)+(`2`*2)+(`3`*3)) %>% 
  mutate(n = `-4`+`-3`+`-2`+`-1`+`1`+`2`+`3`) %>% 
  mutate(score = sentiment/n)