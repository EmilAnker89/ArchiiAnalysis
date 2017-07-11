library(RMySQL)
library(dplyr)
library(tidyr)
#library(magrittr)

#read array-from-feature-extractor:
test <- read.csv2("~/Desktop/feature_extracted.csv", header = F, stringsAsFactors = F)
test <- apply(test, 2, as.numeric) %>% as.data.frame
#fetch labels from mysql:
read_user <- readRDS("~/read_mysql.rds")

mydb = dbConnect(MySQL(), user=read_user$user, password=read_user$pw, dbname=read_user$db, host=read_user$host)
test$id <- dbGetQuery(mydb, "SELECT id FROM document;")$id #fingers crossed for ordering...
tmp <- dbGetQuery(mydb, "SELECT id, path FROM userdocument;")
test <- dplyr::left_join(test, tmp)
docs_cat <- dbGetQuery(mydb, "SELECT * FROM documentclassification;")
cats <- dbGetQuery(mydb, "SELECT * FROM category;")
docs_cat <- left_join(docs_cat, cats[,c("id", "name", "index", "tier")], by=c("category"="id"))
tmp <- docs_cat
tmp$tier <- paste0("tier_",tmp$tier)
tmp$name <- paste0(tmp$index,": ",tmp$name)
tmp <- spread(tmp[,c("document", "name", "tier")], tier, name)

df <- left_join(test, tmp, by = c("id" = "document"))


non_labels <- setdiff(names(df),c("id", "path", "tier_0", "tier_1", "tier_2", "tier_3"))

#kunne være en t-SNE i stedet for PCA (måske bedre til plots):
pca <- stats::prcomp(df[,non_labels],
                           center = TRUE,
                           scale. = FALSE, retx=T)

saveRDS(pca$x %>% as.data.frame, file = "Rscripts/plotly_app/pca.rds")
saveRDS(df[,c("id", "path", "tier_0", "tier_1", "tier_2", "tier_3")], file = "Rscripts/plotly_app/labels.rds")







