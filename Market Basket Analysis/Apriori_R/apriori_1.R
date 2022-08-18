library(readr)
library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(sqldf)
library(htmlwidgets)
library(rpivotTable)




df <- read_csv('export_orders.csv') ###Загрузка файла
dff <- df[,c(1,5,21)] ###Удаление лишних данных

###подключаем arules
library(arules)
summary(dff)

############ общее кол-во проданных товаров
dff
sum(dff$Qty)

############ общее кол-во проданных товаров по позициям
data_1 <- dff %>%
  group_by(`Product Name`) %>%
  summarize(sales_total = sum(`Qty`),
            orders_count = n()
           ) %>%
arrange(desc(sales_total))

############ Топ10 по продажам

top10products <- data_1[1:10,]
top10products
plot(top10products$`Product Name`, top10products$sales_total, type = "h")
top10productsvector <-as.vector(top10products$`Product Name`)
###Смотрим кол-во строк = кол-ва уникальных транзакций

dt <- split(dff$`Product Name`, dff$`Order ID`)
dt


#creating the baskets
dt2 = as(dt,"transactions")
summary(dt2)
inspect(dt2) ########список транзакций с файлами

###График топ 10

itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN = 10)

#########правила

rules = apriori(dt2, parameter=list(support=0.00001, confidence=0.8))
rules = apriori(dt2, parameter=list(support=0.0001, confidence=0.1, minlen = 2))
rules = apriori(dt2, parameter=list(support=0.0000, confidence=0.1, maxlen = 2))

rules3 = as(rules, "data.frame")
inspect(rules[1:10])


rules = eclat(dt2, parameter = list(support = 0.00004, minlen = 2))
<- inspect(rules[1:10])

rules3 = as(rules, "data.frame")

#########правила сортированные по топу
inspect( subset( rules, subset = rhs %pin% top10productsvector ))
inspect( subset( rules, subset = rhs %pin% "Кроссовки ST Runner v2 Full L" ))





rules = apriori(data = dt2, parameter = list(support = 0.0001, confidence = 0.65))
inspect(sort(rules, by = 'lift'))


#########товары 
basket_rules3 = apriori(dt2, parameter=list(supp=0.000001,conf = 0.8, maxlen = 2),
                        appearance = list(default="lhs",rhs=top10productsvector),
                        control = list(verbose=F))

summary(basket_rules3)
inspect(basket_rules3)

top <- inspect(sort(basket_rules3,by = 'count'))[1:10]
top

basket_rules4 = apriori(dt2, parameter=list(supp=0.000001,conf = 0.1, maxlen = 2),
                        appearance = list(default="rhs",lhs="Кроссовки Thunder Spectra"),
                        control = list(verbose=F))
basket_rules4
inspect(basket_rules4)

