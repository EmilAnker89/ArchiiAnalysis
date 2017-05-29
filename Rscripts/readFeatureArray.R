library(RMySQL)
library(dplyr)
library(tidyr)
library(magrittr)

#read array-from-feature-extractor:
test <- read.csv2("~/Desktop/test.csv", header = F)
#fetch labels from mysql:
read_user <- readRDS("~/read_mysql.rds")

mydb = dbConnect(MySQL(), user=read_user$user, password=read_user$pw, dbname=read_user$db, host=read_user$host)
test$id <- dbGetQuery(mydb, "SELECT id FROM document;")$id #fingers crossed for ordering...
docs_cat <- dbGetQuery(mydb, "SELECT * FROM documentclassification;")
cats <- dbGetQuery(mydb, "SELECT * FROM category;")
docs_cat <- left_join(docs_cat, cats[,c("id", "name", "index", "tier")], by=c("category"="id"))
tmp <- docs_cat
tmp$tier <- paste0("tier_",tmp$tier)
tmp$name <- paste0(tmp$index,": ",tmp$name)
tmp <- spread(tmp[,c("document", "name", "tier")], tier, name)

test2 <- left_join(test, tmp, by = c("id" = "document"))









# docs <- dbGetQuery(mydb, "SELECT * FROM document limit 1;")
# col_table <- dbGetQuery(mydb, "select * from information_schema.columns;")
# base_table <- dbGetQuery(mydb, "select * from information_schema.tables;")
# View(base_table)
#number of rows in test differs from number of rows in docs - men det passer med selve docs..?


