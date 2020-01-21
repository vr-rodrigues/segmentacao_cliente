setwd('D:/projetos/market_analysis')
getwd()
set.seed(42)

# ------------
# Bibliotecas
# ------------
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)
library(factoextra)
library(FactoMineR)
library(ggthemes)

# -------------
# Data Wrangling
# -------------

df <- fread("customers.csv", sep=',')
df <- subset(df, select = -c(Channel, Region))
log_df <- log(df)

head(df)
# Fitando modelo de K-Means
#fviz_nbclust(log_df, kmeans, method = "silhouette")
k2 <- kmeans(log_df, centers = 2, nstart = 25)


result <- data.frame(k2$cluster)
df$ind <- seq.int(nrow(df))
log_df$ind <- seq.int(nrow(log_df))
result$ind <- seq.int(nrow(result))

log_df <- merge(log_df, result, by='ind')
log_df <- subset(log_df, select=-c(ind))

df <- merge(df, result, by='ind')
df <- subset(df, select=-c(ind))


df_pca <- PCA(log_df[,-7], graph=F)

log_df$k2.cluster <- ifelse(log_df$k2.cluster==1,"Perfil 1", "Perfil 2")
df$k2.cluster <- ifelse(df$k2.cluster==1,"Perfil 1", "Perfil 2")

df_sum <- aggregate(.~k2.cluster, df, mean)
df_sum <- melt(df_sum, id.var=c('k2.cluster'))
df_sum$value <- round(df_sum$value/100)

# ------------------
# Gráficos
# -----------------

fviz_nbclust(log_df, kmeans, method = "silhouette")

fviz_pca_biplot(df_pca, col.ind = df$k2.cluster,
                label = "var",
                col.var = "black", repel = TRUE,
                addEllipses=TRUE, ellipse.level=0.90) +
  labs(title ="PCA e Segmentação de Clientes via K-Means" ,x="Dim1", y="Dim2")


ggplot(data=df_sum, aes(x=variable, y=value, fill=variable)) + 
  geom_bar(stat="identity") + scale_fill_economist() +
  facet_grid(~k2.cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title ="Gasto médio por segmento de produtos e perfil de clientes ", y = "Gasto em cem u.m.", x = NULL)
  
