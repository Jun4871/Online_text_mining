library(rvest)
library(tidyverse)
library(KoNLP)
library(wordcloud2)
library(glue)
library(arules)
library(arulesViz)

# 네이버 URL
# url <- "https://search.naver.com/search.naver?where=news&sm=tab_jum&query=%EA%B3%B5%ED%8C%94%EB%A6%AC%ED%84%B0"

keyword <- "코로나"
# page_number <- c(1,11,21,31,41,51,61,71,81,91)
page_number <- seq(1, 100, 10) # seq(a, b, c) a부터 b까지 c칸씩 띄어라

# 1 11 21 31 


# url <- glue("https://search.naver.com/search.naver?&where=news&query={keyword}&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=105&start={x}&refresh_start=0")
# 
# 
# # 네이버 뉴스 URL
# news_urls <- url %>% 
#   read_html() %>% 
#   html_nodes(".news.mynews.section._prs_nws") %>% 
#   html_nodes("._sp_each_url") %>% 
#   html_attr("href")
# 
# 
# # 네이버 뉴스_index
# naver_news_index <- grep("https://news.naver.com/main", news_urls)
# 
# # 네이버 뉴스 url만
# news_urls <- news_urls[naver_news_index]


# map 함수 적용
map_news_urls <- map(page_number, function(x) {
  url <- glue("https://search.naver.com/search.naver?&where=news&query={keyword}&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=105&start={x}&refresh_start=0")
  
  news_urls <- url %>% 
    read_html() %>% 
    html_nodes(".news.mynews.section._prs_nws") %>% 
    html_nodes("._sp_each_url") %>% 
    html_attr("href")
  
  # 네이버 뉴스_index
  naver_news_index <- grep("https://news.naver.com/main", news_urls)
  
  # 네이버 뉴스 url만
  news_urls <- news_urls[naver_news_index]
}) %>% 
  unlist() %>% # 리스트에서 백터화
  unique()




# # 네아버 뉴스 첫번째 기사 text 가져오기
# news_urls[1] %>%
#   read_html() %>%
#   html_nodes("#articleBodyContents._article_body_contents") %>%
#   html_text()

# 네이버 뉴스 1번부터 5번까지 텍스트 가져오기

# 빈 벡터 생성
news_text_set <- c()

for (i in 1:length(map_news_urls)) {
  news_text <- map_news_urls[i] %>%
    read_html() %>%
    html_nodes("#articleBodyContents._article_body_contents") %>%
    html_text()
  
  if(length(news_text) == 0) {
    text_set <- "내용 없음"
  }
  
  # 빈벡터에 뉴스 text를 하나씩 붙힘 
  news_text_set <- append(news_text, news_text_set)
  # Sys.sleep(1)
}


# 필요없는 문자열 제거
cleanging_data <- news_text_set %>% 
  str_remove_all("flash 오류를 우회하기 위한 함수 추가") %>% # 문자열 제거
  str_remove_all("function _flash_removeCallback") %>% 
  str_remove_all("[a-zA-Z]") %>% # 정규표현식 영어제거
  str_remove_all("\\d") %>% 
  str_remove_all("무단 전재 및 재배포 금지") %>% 
  str_remove_all("내용 없음") %>% 
  str_replace_all("\\W" , " ") # 정규표현식 : 문자가 아닌 것을 공백으로 변경





# # 필요없는 문자열 제거2  (은, 는 가, 을 , 를, 일보 )
# noun_data <- extractNoun(cleanging_data)
# noun_data_2 <- noun_data[[5]] %>% 
#   str_remove_all("은") %>% 
#   str_remove_all("는") %>% 
#   str_remove_all("가") %>% 
#   str_remove_all("를") %>% 
#   str_remove_all("일보")
# 
# # 글자 수가 2개 이상인 것만 추출
# # nchar(noun_data_2)
# noun_data_2 %>% 
#   subset(nchar(noun_data_2) >= 2) # filter함수와 매우 유사



# 필요없는 문자열 제거2  (은, 는 가, 을 , 를, 일보 )
# map 함수를 사용하여 병렬 처리
# map(벡터, 함수)

nouns <- map(cleanging_data,function(x) {
  # x 자리에 cleanging_data가 들어가 일괄적으로 처리함. 
  noun_data <- extractNoun(x) %>% 
    str_remove_all("은") %>% 
    str_remove_all("는") %>% 
    str_remove_all("가") %>% 
    str_remove_all("를") %>% 
    str_remove_all("일보")
  
  noun_data <- noun_data %>% 
    subset(nchar(noun_data) >= 2)
})


# 테이블 형태로 변환
word_count <- nouns %>% 
  unlist() %>% # 리스트를 벡터로 전환
  table() # 각 단어에 대한 빈도확인


# 편리하게 데이터 프레임으로 전환
df_word <- as.data.frame(word_count, stringsAsFactors = FALSE)

# 컬럼명 변경
colnames(df_word) <- c("word", "freq")


# 데이터 정제
word_freq <- df_word %>% 
  filter(freq >= 2) %>%
  arrange(desc(freq)) 







# 상위 n 개 데이터 파악
test_df <- word_freq %>% 
  top_n(20) %>% # 헤드함수와 같음
  select(word) %>% 
  t() %>%  # 행과 열의 위치 변경
  as.character()




# 장바구니 알고리즘 적용
# 상위 n 개 데이터 사용

# nouns[[1]]에 test_df 포함되어 있는지 확인.
# nouns[[1]] %in% test_df

# nouns[[1]] %>%
#   subset(nouns[[1]] %in% test_df) %>%
#   unique() %>%
#   head(10)


item_list <- map(1:5, function(x){
  # x 자리에 1에서 5가 들어감.
  nouns[[x]] %>%
    subset(nouns[[x]] %in% test_df) %>%
    unique() %>%
    head(10)
})


# item의 트렌잭션화

item <- as(item_list, "transactions")
inspect(item)


# apriori 함수 수행( 지지도 0.1, 신뢰도 0.8 이상인 연관성 규칙 구하기)
result_items <- apriori(item, parameter = list(support=0.1, confidence=0.8))

inspect(result_items[1:80])


# 시각화
set.seed(200)
plot(result_items[1:80], method = "graph")



# 워드클라우드
wordcloud2(word_freq,
           fontFamily = "Malgun Gothic",
           size = 0.5)

ggplot(head(word_freq,30), aes(x=reorder(word,freq),y=freq,fill=word),colour=gradient) +
  geom_col() +
  theme_bw() + 
  labs(x="",y="단어 빈도",title = paste0(keyword," 네이버 뉴스 단어 빈도")) + 
  coord_flip() + 
  theme(legend.position = "none") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1))







