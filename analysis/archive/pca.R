pca <- princomp(embeddings %>% select(-Img, -Video, -Frame) %>% scale())
plot(pca)
pca$loadings %>% View()
fviz_eig(pca, addlabels = TRUE)


pca <- prcomp(embeddings %>% select(-Img, -Video, -Frame), scale = TRUE)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

cumsum(var_explained)
#I would need 193 components for 90 % variance


plot(pca)
pca$loadings %>% View()
fviz_eig(pca, addlabels = TRUE)
