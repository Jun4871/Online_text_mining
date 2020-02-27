library(rvest)
library(glue)

# 뉴스 url 가져오기

# 뉴스 검색어
keyword <- "공팔리터"
page_number <- 1

# 뉴스 URL 
url <- glue("https://search.naver.com/search.naver?where=news&sm=tab_jum&query={keyword}")

# 페이지수 고려한 뉴스 URL (페이지 버튼 누르면 길게 나옴)
url <- glue("https://search.naver.com/search.naver?&where=news&query={keyword}&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=105&start={page_number}&refresh_start=0")

# 검색한 뉴스 URL 에 대한 `각 뉴스`의 URL 검색
news_urls <- url %>% 
  read_html() %>%  # url에 대한 html 화
  html_nodes(".news.mynews.section._prs_nws") %>% # 뉴스들 집합체들의 css 
  html_nodes("._sp_each_url") %>% # 네이버 뉴스가 있을 법한 곳에 대한 css
  html_attr("href") # 네이버 뉴스의 href(url)



news_urls <- news_urls[grep("https://news.naver.com/main", news_urls)]






## 뉴스에 대한 text 가져오기 
# text_url <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=105&oid=020&aid=0003237460"

# text_url <- news_urls[2]
# 
# text_url %>% 
#   read_html() %>% 
#   html_nodes("#articleBodyContents._article_body_contents") %>% 
#   html_text()



news_text <- c()

for(i in 1:5) {
  text_url <- news_urls[i]
  
  texts <- text_url %>% 
    read_html() %>% 
    html_nodes("#articleBodyContents._article_body_contents") %>% 
    html_text()
  
  # for문으로 가져온 text를 append함수를 써서 이어 붙히기
  news_text <- append(news_text, texts)
  # news_text <- append(news_text, texts)
}




# asdf <- c()
# 
# for(i in 1: 100) {
#   asdf <- append(asdf, i)
# print(asdf)
# # Sys.sleep((1))
#   }

