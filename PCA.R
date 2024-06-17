# Библиотеки

library(ggplot2)
library(factoextra)
library(ggplot2)
library(caret)
library(plotly)
library(corrplot)
library(gridExtra)
library(cluster)
library(NbClust)
library(clusterCrit)
library(clusterSim)
library(fpc)

# Датасет для обработки методом главных компонент

View(RFM_df)
dim(RFM_df)

# Нормализация признаков

preproc_scaled <- preProcess(RFM_df, method = c("center", "scale"))
df_scaled <- predict(preproc_scaled, RFM_df)

# Применение метода PCA

pca = prcomp(df_scaled)
summary(pca)

# Датафрейм с кумулятивными суммами для графика

pca_var <- data.frame(
  "Number_of_Components" = 1:length(pca$sdev),
  "Cumulative_Explained_Variance" = cumsum(pca$sdev^2 / sum(pca$sdev^2))
)

# График кумулятивной суммы от числа компонент

p <- plot_ly(pca_var, x = ~Number_of_Components, y = ~Cumulative_Explained_Variance, type = "scatter", mode = "lines+markers",
             line = list(color = "purple", width = 2),
             marker = list(color = "purple", size = 4)) %>%
  layout(xaxis = list(title = "Number of Components", tickvals = seq(0, 7, 1), range = c(0.5, length(pca$sdev) + 0.5)),
yaxis = list(title = "Cumulative Explained Variance"),
plot_bgcolor = "white")

p

# Датафрейм с долей объясненной дисперсии для графика

pca_var2 <- data.frame(
  "Principal_Components" = paste0("PCA", 1:length(pca$sdev)),
  "Explained_Variance_Ratio" = pca$sdev^2 / sum(pca$sdev^2)
)

# Графики объясненной дисперсии каждой компонентой

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

p <- plot_ly(pca_var2, x = ~Principal_Components, y = ~Explained_Variance_Ratio, type = "bar",
             marker = list(color = ~Explained_Variance_Ratio, colorscale = "Viridis")) %>%
  layout(xaxis = list(title = "Principal Components"),
         yaxis = list(title = "Percent of Explained Variance"), range = c(0, 1),
plot_bgcolor = "white")

p

# Собственные значения

eigen_value = pca$sdev
eigen_value

# Собственные вектора

eigen_vectors = pca$rotation
eigen_vectors

# Корреляция между данными и главными компонентами 

cor(df_scaled, pca$x)

# Результаты PCA для переменны

var=get_pca_var(pca)

# График демонстрации наиболее важных переменных для каждого измерения

corrplot(var$cos2, is.corr=FALSE)

#График  демонстрации наиболее важных переменных для первых двух главных компонент

fviz_cos2(pca, choice = "var", axes = 1:2)

#Вклад переменных в PC1

a=fviz_contrib(pca, choice = "var", axes = 1)# top= 5 to limit to five var.

#CВклад переменных в PC2

b=fviz_contrib(pca, choice = "var", axes = 2)

#Построение столбчатых графиков переменных вкладов в PCA1 и PCA2

grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')

# Информация о главных компонентах и о вкладе исходных переменных в эти компоненты

pca_30 = prcomp(head(df_scaled,30))
biplot(pca_30)

# Определение количества главных компонент

screeplot(pca, type="l", main = "Screeplot for Bank Data")
abline(1, 0, col="red", lty=2)

# Определить количество главных компонент, которые объясняют более 80% дисперсии

n_comp = 4

# Преобразовать данные с использованием 4 главных компонент

pca_data <- predict(pca, newdata = df_scaled)[, 1:n_comp]

# Получить размерность преобразованных данных

dim(pca_data)

# Создать датафрейм уменьшенной размерности pca_df

pca_df <- data.frame(pca_data)
names(pca_df) <- paste0("PC", 1:n_comp)
head(pca_df)

# Демонстрация разницы кластеризации до и после применения метода PCA

# Инициализировать список для записи величин инерции

inertias <- list()

# Выполнить k-метод кластеризации для различных значений k

for (k in 2:10) {
  kmeans_res <- kmeans(df_scaled, centers = k, nstart = 25, iter.max = 300)
  inertias[[k]] <- kmeans_res$tot.withinss
}

# Преобразовать список в вектор

inertias <- unlist(inertias)
inertias

# График метода локтя

ggplot(data.frame(k = 2:10, inertias = inertias), aes(x = k, y = inertias)) +
  geom_point(color = "purple") +
  geom_line(color = "purple") +
  scale_x_continuous(breaks = 2:10) +
  labs(x = "Number of Clusters", y = "Inertia", title = "Elbow Method") +
  theme_minimal() +
  geom_vline(xintercept = 5, color = "black", linetype = "dashed", linewidth = 1)


# Выполнить k-метод кластеризации с 5 кластерами для df_scaled

kmeans_res <- kmeans(df_scaled, centers = 5, nstart = 25, iter.max = 300)

# Вывести первые строки набора данных df_scaled с результатами кластеризации

head(df_scaled)

# Вычислить индекс силуэтки для первых 100 точек набора данных df_scaled

sil_res_df <- silhouette(head(kmeans_res$cluster, 20000), dist(head(df_scaled, 20000)))

# Вывести результат

print(paste0("Индекс силуэтки: ", mean(sil_res_df[, "sil_width"])))

# Вычислить индекс Дэвиса-Баудина

db_index_df <- index.DB(df_scaled, kmeans_res$cluster)$DB
print(paste0("Индекс Дэвиса-Баудина: ", db_index_df))

# Выполнить k-метод кластеризации с 5 кластерами для pca_df

kmeans_res_pca <- kmeans(pca_df, centers = 5, nstart = 25, iter.max = 300)

sil_res_pca <- silhouette(head(kmeans_res_pca$cluster, 20000), dist(head(pca_df, 20000)))
print(paste0("Индекс силуэтки: ", mean(sil_res_pca[, "sil_width"])))

db_index_pca <- index.DB(pca_df, kmeans_res_pca$cluster)$DB
print(paste0("Индекс Дэвиса-Баудина: ", db_index_pca))

# Итоговая таблица с метриками

View(results)
