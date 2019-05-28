getwd()
setwd("C:/Users/limwk/Desktop/R/file/")
getwd()
install.packages("stringr")
library(stringr)
install.packages("rJava")
library(rJava)
install.packages("rvest")  
library(rvest)
install.packages("KoNLP")
library(KoNLP)
install.packages("dplyr")
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud")
library(wordcloud)
useSejongDic()
library(data.table)


#---------------------------------chapter1 url parsing

hk_url <- "http://search.hankyung.com/apps.frm/search.news?query=%EC%A0%84%EA%B8%B0%EC%9E%90%EB%8F%99%EC%B0%A8&page="

hk_urls <- NULL
for (x in 1:5){
  hk_urls[x+1] <- paste(hk_url, as.character(x),sep = "",encoding="utf-8")
}

urls <- NULL
for(url in hk_urls){
  html <- read_html(url)
  urls <- c(urls, html %>% html_nodes(".txt_wrap") %>% html_nodes("a") %>% html_attr("href")%>% unique())
}
urls <- urls[-grep("tag",urls)]
urls <- urls[-grep("magazine",urls)]
urls <- urls[-grep("autotimes",urls)]
urls <- urls[-grep("marketinsight",urls)]
urls <- urls[-grep("snacker",urls)]
urls <- repair_encoding(urls,from="utf-8")
html2 <- NULL
links <- NULL

txts <- NULL
for(links in urls){
  html2 <- read_html(links,encoding = 'euc-kr')
  txts <-  c(txts,html2 %>% html_nodes(".news_view") %>% html_text())
}

text <- c(text, html2 %>% html_nodes("#newsView") %>% html_text())

news <- cbind("검색어"="전기자동차",url=news_url,content=unlist(news_text))
news <- as.data.frame(news)

write.csv(news,"news.csv")

#----------------------------------chapter2 text mining 

text <- gsub("\\n","",text)
text <- gsub("\\d+","",text)
text <- gsub("\\.","",text)
text <- gsub("\r","",text)
text <- gsub("\t","",text)
text <- gsub("[A-z]","",text)
text <- gsub("[[:cntrl:]]","",text)
text2 <- sapply(text,extractNoun,USE.NAMES = F)
text3 <- unlist(text2)
text4 <- Filter(function(x){nchar(x)>=2 & nchar(x)<=5},text3)
text4 <- gsub("한경닷컴","",text4)
text4 <- gsub("무단","",text4)
text4 <- gsub("전재","",text4)
text4 <- gsub("구독","",text4)
text4 <- gsub("뉴스래빗","",text4)
text4 <- gsub("모바일한경","",text4)

text4 <- gsub("기사","",text4)
text4 <- gsub("들이","",text4)
text4 <- gsub("글방","",text4)
text4 <- gsub("하게","",text4)
text4 <- gsub("무엇을","",text4)
text4 <- gsub("지난달","",text4)
text4 <- gsub("하기","",text4)
text4 <- gsub("번째","",text4)

text5 <- data.table(text4)
wordcount <- table(text5)
head(sort(wordcount,decreasing=T),50)

write.table(wordcount,"wordfile.txt")    #text file saving

pal <- brewer.pal(9,"Set1")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0,min.freq=12,
          random.order=F,random.color=T,colors=pal)

z <- c(1:3,NA)
is.na(z)
z==NA
c(1,1,1,2)==2
c(2,4,6,8)+c(1,3,5,7,9)
length(pi)
"+"(2,3)
x<- 1:100
sum(x>50)
sum(x>=50)
x>50
f <- function(x,a)return((x-a)^2)
f(1:2,3)
x <- c(1,2,3,NA)
mean(x)
subset()
k <- factor(c("A","B","C"))
class(k)
mode(k)
subset()

library(reshape)
md <- data.frame(id=(c(1,1,2,2)),time=c(1,2,1,2),x1=c(5,3,6,2),x2=c(6,5,1,4))
melt <- melt(md,id=c("id","time"))
cast(melt,id+time~variable)
cast(melt,id+variable~time)

head(airquality,5)
md <- melt(airquality,id="Day")
head(md)
cast(md,Day~variable,mean,na.rm=T)
cast()