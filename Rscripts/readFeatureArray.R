library(RMySQL)
library(dplyr)
library(tidyr)
library(magrittr)

#read array-from-feature-extractor:
test <- read.csv2("~/Desktop/test.csv", header = F)
#fetch labels from mysql:
mydb = dbConnect(MySQL(), user='root', password='Christin4', dbname='docs', host="127.0.0.1")
test$id <- dbGetQuery(mydb, "SELECT id FROM document;") #fingers crossed for ordering...
docs_cat <- dbGetQuery(mydb, "SELECT * FROM documentclassification;")
tmp <- head(docs_cat) %>% group_by(document) %>% mutate(n=paste0("cat_",1:n()))
tmp %>%   ungroup %>%
  spread(., key = n, value = category) %>%
  gather(., key = document)  

spread(dat1, key = numbers, value = value)


docs <- dbGetQuery(mydb, "SELECT * FROM document limit 1;")
col_table <- dbGetQuery(mydb, "select * from information_schema.columns;")
base_table <- dbGetQuery(mydb, "select * from information_schema.tables;")
View(base_table)
#number of rows in test differs from number of rows in docs - men det passer med selve docs..?


