###### Bakery Transactons EDA ######

## Kaggle EDA참고 https://www.kaggle.com/danilodiogo/bakery-transactions-eda-and-association-rules/notebook


# Packages
library(tidyverse)
library(lubridate)
library(cowplot)
library(arules)
library(arulesViz)
library(grid)
library(dplyr)
library(ggplot2)

# Load Data
data <- read.csv("BreadBasket_DMS.csv")


# < 1.컬럼 설명 >

str(data)
# Date : 날짜
# Time : 시간
# Transaction : 구매 단위
# Item :구매 아이템

summary(data)
head(data)

#----------------------------------------------------------------------------------------------------#

# < 2.데이터 파악 및 자료형 변환 >

data <- data %>% mutate(Date = as.Date(Date), Time = hms(Time)) 
str(data)
# Date는 Date자료형으로, Time은  hour, minute, second 가 붙여짐

c(sort(data$Date)[1], sort(data$Date, decreasing=T)[1])
# 데이터 총 기간 : 2016-10-30 부터 2017-04-09 까지

# 구매단위 분포
max(data$Transaction) # 총 9684개 구매건수
trans = sort(table(data$Transaction), decreasing = T)
trans = as.data.frame(trans)

ggplot(trans, aes(trans$Freq)) +
  geom_histogram(stat="count") +
  labs(x = "Freq of trans", y = "Count", title = "Freq of transation") +
  scale_x_continuous(breaks = c(0:12))
summary(trans$Freq)

# 최소 1개 이상, 최대 12개의 아이템을 구매했으며, 평균 2.234개씩 구매합니다.


#----------------------------------------------------------------------------------------------------#

# < 3. 월별 판매량 및 구매단위수 그래프 >

a <- data %>%
  mutate(
    Month = as.factor(month(Date))
  ) %>%
  group_by(Month, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Month, fill = Month)
  ) +
  geom_histogram(stat="count") +
  theme(
    legend.position="none"
  ) +
  labs(x = "Month", y = "Transactions", title = "Transactions per month")

b <- data %>%
  mutate(
    Month = as.factor(month(Date))
  ) %>%
  group_by(Month, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Month, y = Transactions, fill = Month)
  ) +
  geom_boxplot() +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Month", y = "Items / transaction", title = "Items per transaction (per month)")

plot_grid(a,b)

# 11월에 가장 많은 판매량을 기록했습니다.
# 월별 구매단위수가 모두 right skewed 이며, 특히 12월에 더 심한 skewed를 보입니다.


#----------------------------------------------------------------------------------------------------#

# < 4. 요일별 판매량 및 구매단위수 그래프 >

c <- data %>%
  mutate(
    WeekDay = as.factor(weekdays(Date))
  ) %>%
  group_by(WeekDay, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = WeekDay, fill = WeekDay)
  ) +
  geom_histogram(stat="count") +
  theme(
    legend.position="none",
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  labs(x = "Weekday", y = "Transactions", title = "Transactions per weekday")

d <- data %>%
  mutate(
    WeekDay = as.factor(weekdays(Date))
  ) %>%
  group_by(WeekDay, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = WeekDay, y = Transactions, fill = WeekDay)
  ) +
  geom_boxplot() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position="none" 
  ) +
  labs(x = "Weekday", y = "Items / transaction", title = "Items per transaction (per weekday)")

plot_grid(c, d)

# 토요일의 판매량이 가장 높습니다.
# 다른 요일과 비교하여 월, 화, 금요일에는 한 사람당 구매량이 적습니다. 


#----------------------------------------------------------------------------------------------------#


# < 5. 시간대 별 판매량 그래프 >

e <- data %>%
  mutate(
    Hour = as.factor(hour(Time))
  ) %>%
  group_by(Hour, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Hour, fill = Hour)
  ) +
  geom_histogram(stat="count") +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Hour", y = "Transactions", title = "Transactions per hour")

f <- data %>%
  mutate(
    Hour = as.factor(hour(Time))
  ) %>%
  group_by(Hour, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Hour, y = Transactions, fill = Hour)
  ) +
  geom_boxplot() +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Hour", y = "Items / transaction", title = "Items per transaction (per hour)")

plot_grid(e, f)

# 해당 bakery는 오전 7시부터 오전 1시까지 영업합니다.
# 오전 10시부터 오후 5시까지는 다른 시간대보다 더 많은 상품을 한번에 구매하는 경향이 있습니다.


#----------------------------------------------------------------------------------------------------#

# < 6. 아이템 분포 >

data %>%
  group_by(Item) %>%
  summarize(
    Count = n()
  ) %>%
  arrange(desc(Count))  %>%
  head(n = 10) %>%
  ggplot(
    aes(
      x = reorder(Item, Count), y = Count, fill = Item
    )
  ) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(
    legend.position = "none" 
  ) +
  labs(x = "Item", y = "Transactions", title = "Most popular items")

# 가장 인기있는 품목은 순서대로 커피, 빵, 차, 케이크 입니다.


#----------------------------------------------------------------------------------------------------#
# < 7. NONE 제거 >

data.new <- data %>%
  filter(!Item %in% "NONE") %>%
  group_by(Transaction) %>%
  mutate(
    ItemsPurchased = n(),
    Combined = ifelse(ItemsPurchased > 1, TRUE, FALSE)
  ) %>% 
  ungroup()

#----------------------------------------------------------------------------------------------------#

# < 8. 아이템을 2개 이상 구매한 거래에서 가장 많이 판매된 상품 >

data.new %>%
  group_by(Item, ItemsPurchased) %>%
  summarize(
    Transaction.Moments = n()
  ) %>%
  filter(Transaction.Moments > 100 & ItemsPurchased > 1) %>%
  ggplot(
    aes(x = Item, y = Transaction.Moments, fill = Item)
  ) +
  geom_col() +
  facet_grid(ItemsPurchased ~.) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position="none" 
  ) 

# 2개 이상의 아이템을 구매할 때, 커피를 함께 구매할 확률이 높을 것으로 예상할 수 있습니다.


#----------------------------------------------------------------------------------------------------#
# select <- dplyr::select
# < 10. 단독 구매 상품 >
combined.items <- data.new %>%
  group_by(Item, ItemsPurchased) %>%
  summarize(
    Transaction.Moments = n()
  ) %>%
  filter(Transaction.Moments > 100 & ItemsPurchased > 1) %>%
  select(Item) %>%
  unique()

data.new %>%
  group_by(Item, Combined) %>%
  summarize(
    Transaction.Moments = n()
  ) %>%
  filter(Combined == FALSE) %>%
  anti_join(y = combined.items, by = "Item") %>%
  arrange(desc(Transaction.Moments))  %>%
  head(n = 10) %>%
  ggplot(
    aes(x = reorder(Item, -Transaction.Moments), y = Transaction.Moments, fill = Item)
  ) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position="none" 
  ) +
  labs(x = "Item", y= "count")

# 'Farm House'의 제품, Scandinavian dish, 바게트는 단독으로 잘 팔리는 아이템입니다.
# 추후 연관 분석을 통해 다른 상품과 함께 판매할 전략을 세운다면, 매출 상승에 도움이 될 것으로 기대할 수 있습니다.


#----------------------------------------------------------------------------------------------------#

# < 트랜잭션 데이터로 변경 >

data.tran <- split(data.new$Item, data.new$Transaction)
data.tran <- sapply(data.tran, unique)
head(data.tran)
transaction.data <- as(data.tran,"transactions")
# 데이터 형식이 apriori를 분석하는데 적합하지 않기 때문에 최적화된 transactions 데이터로 변경했습니다.

# < Apriori 적용 >
# 지지도는 0.003 이상인 경우 규칙 발견뿐만 아니라 아이템 선정이 어렵, 그 밑의 값은 분석의미가 떨어짐.
# 신뢰도는 0.6 이상인 경우 50퍼센트 이상으로 규칙생성
association.rules <- apriori(transaction.data, 
                             parameter = list(
                               supp = 0.003, # 최소 지지도(아이템 빈도수)
                               conf = 0.6)) # 최소 신뢰도

# 94개의 품목과 9465개의 transaction으로 apriori 규칙을 적용하였습니다.
# 13개의 규칙이 생성되었습니다.

# support 값이 높은 순서대로 규칙을 정렬합니다.
association.rules <- sort(association.rules, by = 'support', decreasing = TRUE) 
summary(association.rules)

# 물품 2개로 이루어진 규칙 6개와 물품 3개로 이루어진 규칙 7개가 나온 것을 확인할 수 있습니다.

inspect(sort(association.rules, by = 'support'))
inspect(sort(association.rules, by = 'confidence'))
inspect(sort(association.rules, by = 'lift'))

# 여러 제품들을 사는 사람들이 일반적으로 커피를 같이 주문하는 것을 확인할 수 있습니다.
# support 값과 lift값에 대한 순서가 다른 것을 확인할 수 있습니다.

# < 연관분석 관련 그래프 그리기 >
plot(association.rules)
plot(association.rules, method="graph")

# 살라미와 페타 치즈를 구입할 때와, 지역 상품을 구입할 때 각각 커피와의 관계에서 신뢰도가 0.8정도로 높았다.
# 또한, 향상도 역시 1.7, 1.6 정도로 양의 상관관계임을 알 수 있다.
# 따라서 위 두가지의 구매 행동이 일어났을 때, 커피를 함께 살 확률이 높다고 할 수 있다.
# 그러나 샘플수가 각각 31, 51개로 매우 적기 때문에, 일반화가 제한된다.

# 토스트를 구입할 때 커피를 구입하는 경우의 신뢰도와 향상도는 각각 0.7, 1.4이다.
# 따라서 함께 구입할 확률이 높고, 서로 약한 양의 상관관계임을 파악할 수 있다.
# 샘플 수 또한 224개로 이 분석에서 유일하게 유의미한 결과라고 볼 수 있다.


# < IS 측도 > : sqrt(supp * lift) 서포트와 리프트간의 상이한 값을 조정해주는 값
# 앞서 신뢰도와 향상도 순으로 결과를 나열했을때, 순위가 바뀌는 것을 볼 수 있습니다.
# 따라서 추가적으로 IS측도를 구해봅니다.
ISmeasure<- association.rules %>% quality %>% 
  rowwise() %>% 
  summarize(ISmeasure = sqrt(support*lift))
ISmeasure <- transform(ISmeasure, Items = c('{Toast}', '{Cake,Hot chocolate}', '{Salad}', '{Keeping It Local}', '{Cake,Sandwich}', '{Hot chocolate,Pastry}', '{Cookies,Juice}', '{Cookies,Hot chocolate}', '{Sandwich,Soup}', '{Extra Salami or Feta}', '{Tartine}', '{Bakewell}', '{Hot chocolate,Medialuna}'))
ISmeasure[, c(2,1)]
# 토스트와 커피가 0.18로 다른 항목들에 비하여 높습니다.
# 위의 해석과 마찬가지로 토스트와 커피의 연관규칙이 비교적 가장 적합한 결과라고 볼 수 있습니다.


#----------------------------------------------------------------------------------------------------#

# < 분석 한계 >
# 데이터가 편향되어 있고, 샘플 수가 부족하여 구체적인 분석 결과를 도출하는데 어려움이 있었다.
# 고객데이터가 없어서 다양한 마케팅 전략 도출에 실패했다.


# < 의의 >
# 추천시스템 개발에 필요한 supp, conf, lift를 학습할 수 있었다.
# 연관규칙 분석을 위해 트랜젝션 데이터를 다뤄보는 경험이 되었다. 
