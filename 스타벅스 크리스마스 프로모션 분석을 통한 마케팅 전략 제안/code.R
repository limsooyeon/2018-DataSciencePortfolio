# 사용 된 라이브러리
library(twitteR)
library(KoNLP)
library(wordcloud)
library(tm)
library(rvest)
library(wordcloud2)
library(stringr)
library(plyr)

## 스타벅스에서 중점적으로 마케팅하는 아이템

sb = read.csv("starbucks.csv")
sb_news = sb$본문
sb_news = as.character(sb_news)

useSejongDic()
sb_nouns = extractNoun(sb_news) 
sb_nouns = unlist(sb_nouns)
sb_nouns = sb_nouns[nchar(sb_nouns)>=2]   ## 명사 중에서 두 글자이상만 검색
sb_nouns = gsub("스타벅스.*","", sb_nouns)  ## 필요 없는 단어 공백처리
sb_nouns = gsub("27","", sb_nouns)
sb_nouns = gsub("29","", sb_nouns)

cnts <- table(unlist(sb_nouns))  ## 명사별 count 결과 저장
cnts_ <- cnts[cnts > 20]  ## 빈도수가 20보다 큰 것만 추출
wordcloud2(data.frame(word=names(cnts_), freq=as.numeric(cnts_)),
           color = "random-light", backgroundColor = "black", shape="cloud")

## 구글 'starbucks christmas' 뉴스 분석
url_news1 =
  "https://www.google.co.kr/search?q=starbucks+christmas&rlz=1C1NHXL_koKR739KR739&source=lnt&tbs=cdr%3A1%2Ccd_min%3A10%2F1%2F2017%2Ccd_max%3A&tbm="
news1=read_html(url_news1) %>% html_nodes(".r") %>% html_text()
url_news2 =
  "https://www.google.co.kr/search?q=starbucks+christmas&rlz=1C1NHXL_koKR739KR739&tbs=cdr:1,cd_min:10/1/2017&ei=IYUiWoSZGsa90ASVpoTwCg&start=10&sa=N&biw=563&bih=635"
news2=read_html(url_news2) %>% html_nodes(".r") %>% html_text()
url_news3 =
  "https://www.google.co.kr/search?q=starbucks+christmas&rlz=1C1NHXL_koKR739KR739&tbs=cdr:1,cd_min:10/1/2017&ei=U4wiWrK7NcS70ASz2bW4DA&start=20&sa=N&biw=563&bih=635"
news3=read_html(url_news3) %>% html_nodes(".r") %>% html_text()
url_news4= 
  "https://www.google.co.kr/search?q=starbucks+christmas&rlz=1C1NHXL_koKR739KR739&tbs=cdr:1,cd_min:10/1/2017&ei=h4wiWsG4B8Kj0QTZgrLABQ&start=30&sa=N&biw=563&bih=635"
news4 = read_html(url_news4) %>% html_nodes(".r") %>% html_text()
url_news5 = 
  "https://www.google.co.kr/search?q=starbucks+christmas&rlz=1C1NHXL_koKR739KR739&tbs=cdr:1,cd_min:10/1/2017&ei=yIwiWo7dK4Wi0gTKoqrICQ&start=40&sa=N&biw=563&bih=635"
news5 = read_html(url_news5) %>% html_nodes(".r") %>% html_text()

news = rbind(news1, news2, news3, news4, news5)      # 구글 뉴스 5페이지의 기사
textMining = as.character(news)
myCorpus<- Corpus(VectorSource(textMining))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
removeune <- function(x) gsub("christmas", "", x)
myCorpus <- tm_map(myCorpus, removeune)
removesb <- function(x) gsub("starbucks", "", x)
myCorpus <- tm_map(myCorpus, removesb)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

tdm<- TermDocumentMatrix(myCorpus)
m <- as.matrix(tdm)

wordFreq <- sort(rowSums(m), decreasing=TRUE)
wordcloud2(data.frame(word=names(wordFreq), freq=as.numeric(wordFreq)),
           color = "random-light", backgroundColor = "black", shape="cloud")

## ‘스노우 돌체라떼’ 트위터 분석 
consumer_key = 'FEeSv6D4hxCWhd4fhhN129iWT'
consumer_secret = 'rCTyjfLq9NTE04G6S2EBkibbf7cUQ5Ytc0ar7IHU3f72HzehHL'
access_token = '912142852477431808-VBGXXvO3kv1oxnbpTaFWaurDjzRg0s4'
access_secret = 'ihvYgaJ9itar6IdlPMsY5aUfeXnIAGgBbSWSKHwuU9NyU'
setup_twitter_oauth(consumer_key,consumer_secret, access_token, access_secret)

search_string = enc2utf8('스노우돌체라떼')
num_tweets = 100
tweets = searchTwitter(search_string,n = num_tweets, lang = 'ko')
tweets

twitter.df <- twListToDF(tweets)
twitter.text <- twitter.df$text
twitter.text <- unlist(twitter.text)
twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("https*.", "", twitter.text)
twitter.text <- gsub("lunah*.", "", twitter.text)
twitter.text <- gsub("짤이", "", twitter.text)
twitter.text <- gsub("스노우*.", "", twitter.text)
twitter.text <- gsub("ㅋㅋ*.", "", twitter.text)
twitter.text <- gsub("스타벅스*.", "", twitter.text)
twitter.text <- gsub("스벅", "", twitter.text)
twitter.text <- gsub("돌체*.", "", twitter.text)
twitter.text <- gsub("체라떼", "", twitter.text)
twitter.text <- gsub("이거", "", twitter.text)
twitter.text <- gsub("그거", "", twitter.text)
twitter.text <- gsub('co*.','',twitter.text)
twitter.text <- gsub('ke*.','',twitter.text)
twitter.text <- gsub('h1*.','',twitter.text)
twitter.text <- gsub('dL*.','',twitter.text)
twitter.text <- gsub('md*.','',twitter.text)
twitter.text <- gsub('WJ*.','',twitter.text)
twitter.text <- gsub('el*.','',twitter.text)
twitter.text <- gsub('fzl*.','',twitter.text)
twitter.text <- gsub('A9*.','',twitter.text)
twitter_nouns <- Map(extractNoun, twitter.text)
twitter_word <- unlist(twitter_nouns, use.name=F)
twitter_word <- gsub("[[:punct:]]","", twitter_word)
twitter_word <- Filter(function(x){nchar(x)>=2}, twitter_word)
# 단어별 카운팅
twitter_count <- table(twitter_word)
cnts <- table(unlist(twitter_word))  ## 명사별 count 결과 저장
cnts_ <- cnts[cnts > 5]  ## 빈도수가 5보다 큰 것만 추출
wordcloud2(data.frame(word=names(cnts_), freq=as.numeric(cnts_)), size= 1, color = "random-light", backgroundColor = "black", shape="cloud")

## ‘홀리 피치 애플사이더’ 트위터 분석
consumer_key = 'FEeSv6D4hxCWhd4fhhN129iWT'
consumer_secret = 'rCTyjfLq9NTE04G6S2EBkibbf7cUQ5Ytc0ar7IHU3f72HzehHL'
access_token =    '912142852477431808-VBGXXvO3kv1oxnbpTaFWaurDjzRg0s4'
access_secret =    'ihvYgaJ9itar6IdlPMsY5aUfeXnIAGgBbSWSKHwuU9NyU'
setup_twitter_oauth(consumer_key,consumer_secret, access_token, access_secret)

search_string = enc2utf8('홀리피치애플사이더')
num_tweets = 100
tweets = searchTwitter(search_string, n=num_tweets, since='2017-10-01', until='2017-12-01', lang = 'ko')
tweets

twitter.df <- twListToDF(tweets)
twitter.text <- twitter.df$text
twitter.text <- unlist(twitter.text)
twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("https*.", "", twitter.text)

twitter_nouns <- Map(extractNoun, twitter.text)
twitter_word <- unlist(twitter_nouns, use.name=F)
twitter_word <- gsub("[[:punct:]]","", twitter_word)
twitter_word <- Filter(function(x){nchar(x)>=2}, twitter_word)

twitter_word <- gsub("홀리", "", twitter_word)
twitter_word <- gsub("피치", "", twitter_word)
twitter_word <- gsub("애플", "", twitter_word)
twitter_word <- gsub("사이", "", twitter_word)
twitter_word <- gsub("스타벅스", "", twitter_word)
twitter_word <- gsub("Zzarabii", "", twitter_word)
twitter_word <- gsub("31", "", twitter_word)
twitter_word <- gsub("비주", "", twitter_word)
twitter_word <- gsub("12", "", twitter_word)
twitter_word <- gsub("11", "", twitter_word)
twitter_word <- gsub("리스", "", twitter_word)
twitter_word <- gsub("내일", "", twitter_word)
twitter_word <- gsub("작년", "", twitter_word)
twitter_word <- gsub("홀리피치애플사이더", "", twitter_word)
twitter_word <- gsub("군요", "", twitter_word)
twitter_word <- gsub("따스", "", twitter_word)
twitter_word <- gsub("크마스", "", twitter_word)
twitter_word <- gsub("co", "", twitter_word)
twitter_word <- gsub("스벅", "", twitter_word)
twitter_word <- gsub("StarbucksKinfo", "", twitter_word)
twitter_word <- gsub("DavidDHKK", "", twitter_word)
twitter_word <- gsub("29", "", twitter_word)
twitter_word <- gsub("coB4YU9MoaV", "", twitter_word)
twitter_word <- gsub("B4YU9MoaV", "", twitter_word)

twitter_count <- table(twitter_word)        # 단어별 카운팅
cnts <- table(unlist(twitter_word))  ## 명사별 count 결과 저장
cnts_ <- cnts[cnts > 5]  ## 빈도수가 5보다 큰 것만 추출
wordcloud2(data.frame(word=names(cnts_), freq=as.numeric(cnts_)), size= 10, color = "random-light", backgroundColor = "black", shape="cloud")

## ‘starbucks holiday cups' 트위터 감성분석
library(twitteR)
library(KoNLP)
library(wordcloud)
library(tm)
library(rvest)
library(wordcloud2)
library(stringr)
library(plyr)
# 트위터 연동
requestURL= "https://api.twitter.com/oauth/request_token"
accessURL= "https://api.twitter.com/oauth/access_token"
authURL ="https://api.twitter.com/oauth/authorize"

consumerKey="quKLpDhmQpBUP4LIbkQcW62ej"
consumerSecret="s8ZWjNDXvm7qTQIxHyyz7fFf1fvdsHe2E2BicwBv9Qq25qlP2X"
accToken="619920385-jfGc1Yuc9YPIG4Fx5DFmtyQxkMr42lv3RSFdjQbb"
accSecret="rOvVwEhv04qnTuXjdOu1fIr3cGFZPGKuithOYeb4qyTMI"
setup_twitter_oauth(consumerKey, consumerSecret,accToken, accSecret)

# 감성분석
score.sentiment = function(sentences, pos.words, neg.words)
{
  scores = laply(sentences,
  function(sentence, pos.words, neg.words)
  {
  # remove punctuation
  sentence = gsub("[[:punct:]]", "", sentence)
  # remove control characters
  sentence = gsub("[[:cntrl:]]", "", sentence)
  # remove digits?
  sentence = gsub('\\d+', '', sentence)
  
  tryTolower = function(x)
  {
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
  y = tolower(x)
  # result
  return(y)
  }
  
  # use tryTolower with sapply
  sentence = sapply(sentence, tryTolower)
  # split sentence into wods with str_split (stringr package)
  word.list = str_split(sentence, "\\s+")
  words = unlist(word.list)
  
  # compare words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  # get the position of the matched term or NA
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  #final score
  score = sum(pos.matches)-sum(neg.matches)
  return(score)
  }, pos.words, neg.words)
  
  scores.df = data.frame(text=sentences, score = scores)
  return(scores.df)
}

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

tweets = searchTwitter("starbucks holiday cups", n=100, since='2017-10-01', until='2017-12-01', lang="en")

tweets_txt = sapply(tweets, function(x) x$getText())
write.csv(tweets_txt, "starbucksTweets.txt")
write.csv(tweets_txt, "starbucksTweets.csv")

starbucks.score = score.sentiment(tweets_txt, pos.words, neg.words)
table(starbucks.score$score)
mean(starbucks.score$score)
hist(starbucks.score$score)

library(ggplot2)
qplot(starbucks.score$score)