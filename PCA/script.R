setwd("C:/Users/TOSHIBA/Documents/Stochastic_labs/PCA") #path to folder with data file

input_data = read.csv("data.csv", header=TRUE)

prep_data = input_data

for (i in 1:nrow(prep_data))
{
  koef = prep_data[[1]][1]/prep_data[[1]][i]
  for (j in 1:ncol(prep_data))
  {
    prep_data[[j]][i]=prep_data[[j]][i]*koef
  }
}

prep_data = prep_data[, 2:ncol(prep_data)]

c = cor(prep_data, method = "spearman")

pca = prcomp(prep_data, center = TRUE, scale = TRUE)

pca_summary = summary(pca)

plot(pca, type = "l")

biplot(pca, xlim = c(-1,1))
