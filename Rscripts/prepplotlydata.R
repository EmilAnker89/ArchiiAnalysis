dir("out/data_import") #show files in a directory
train <- "out/data_import/contract_binarytrain_data.csv"
labels <- "out/data_import/contract_binarytrain_labels.txt"
features <- "out/data_import/contract_binarytrain_vocabulary.txt"

df <- read_data(train, labels, features)
non_labels <- setdiff(names(df),"contract_label")
train <- df[,non_labels]

#kunne være en t-SNE i stedet for PCA (måske bedre til plots):
train.pca <- stats::prcomp(train,
                           center = TRUE,
                           scale. = FALSE, retx=T)

#gem til brug i app:
saveRDS(train.pca$x %>% as.data.frame, file = "Rscripts/plotly_app/pca.rds")
labels <- read.table(labels, header=F, stringsAsFactors = F)[,1]
saveRDS(labels, file = "Rscripts/plotly_app/labels.rds")

#prøv en anden (multiclass):

train <- "out/data_import/contract_cattrain_data.csv"
labels <- "out/data_import/contract_cattrain_labels.txt"
features <- "out/data_import/contract_cattrain_vocabulary.txt"

df <- read_data(train, labels, features)
non_labels <- setdiff(names(df),"contract_label")
train <- df[,non_labels]

#kunne være en t-SNE i stedet for PCA (måske bedre til plots):
train.pca <- stats::prcomp(train,
                           center = TRUE,
                           scale. = FALSE, retx=T)

#gem til brug i app:
saveRDS(train.pca$x %>% as.data.frame, file = "Rscripts/plotly_app/pca.rds")
labels <- read.table(labels, header=F, stringsAsFactors = F)[,1]
saveRDS(labels, file = "Rscripts/plotly_app/labels.rds")
out <- condense(train.pca, 10, 20)
saveRDS(out, file="Rscripts/plotly_app/rotations.rds")
