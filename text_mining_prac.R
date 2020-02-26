library(glue)
library(rvest)


keyword_2 <- "서울+송파구"
url_2 <- glue("https://search.naver.com/search.naver?where=news&sm=tab_jum&query={keyword_2}")

pg_num <- 1

# 1 뉴스탭의 Keyword 
url_2 <- glue("https://search.naver.com/search.naver?where=news&sm=tab_jum&query={keyword_2}&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=56&start={pg_num}&refresh_start=0")


# 2 뉴스탭의 Keyword가 나온 화면에서 전체기사가 나온 페이지를 html_nodes로 입력하고, 이어서 네이버 뉴스의 소스를 html_nodes로 입력함.
news_urls_2 <- url_2 %>% 
  read_html() %>% 
  html_nodes(".news.mynews.section._prs_nws") %>% 
  html_nodes("._sp_each_url") %>% 
  html_attr("href")
  
news_urls_2 <- news_urls_2[grep("https://news.naver.com/main/read.nhn",news_urls_2)]


# 텍스트 추출
text_url_2 <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=102&oid=001&aid=0011427735"

text_url_2 <- news_urls_2[1] ### ??? 이부분 이해안됨

text_url_2 %>% 
  read_html() %>% 
  html_nodes("#articleBodyContents._article_body_contents") %>% 
  html_text()




## 뉴스에 대한 text 가져오기 
# text_url <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=105&oid=020&aid=0003237460"

text_url <- news_urls[2]

text_url %>% 
  read_html() %>% 
  html_nodes("#articleBodyContents._article_body_contents") %>% 
  html_text()



news_text <- c()

for(i in 1:5) {
  text_url <- news_urls[i]
  
  texts <- text_url %>% 
    read_html() %>% 
    html_nodes("#articleBodyContents._article_body_contents") %>% 
    html_text()
  
  news_text <- append(news_text, texts)
  # news_text <- append(news_text, texts)
}