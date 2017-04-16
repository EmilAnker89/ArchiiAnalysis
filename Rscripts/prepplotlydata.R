#dir("out/data_import") #show files in a directory
train <- "out/data_import/contract_cat_cooptrain_data.csv"
labels <- "out/data_import/contract_cat_cooptrain_labels.txt"
#features <- "out/data_import/contract_binarytrain_vocabulary.txt"

df <- read_data(train=train, labels=labels)
non_labels <- setdiff(names(df),"contract_label")
train <- df[,non_labels]

#kunne være en t-SNE i stedet for PCA (måske bedre til plots):
train.pca <- stats::prcomp(train,
                           center = TRUE,
                           scale. = FALSE, retx=T)
saveRDS(train.pca$x %>% as.data.frame, "Rscripts/plotly_app/pca2.rds")
labels <- read.table(labels, header=F, stringsAsFactors = F)[,1]
saveRDS(labels, "Rscripts/plotly_app/labels2.rds")




