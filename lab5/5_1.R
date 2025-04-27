# -------------------- Библиотеки --------------------
library(NbClust)          # Метод консенсуса
library(cluster)          # silhouette()
library(scatterplot3d)    # 3D визуализация
library(parameters)       # (опционально) описательная статистика
library(easystats)        # (опционально) вспомогательные функции

# -------------------- Загрузка и осмотр данных --------------------
data <- read.csv("~/my_git/dataR/lab5/heart.csv")
str(data)
head(data)
summary(data)

# -------------------- Стандартизация --------------------
scaled_data <- scale(data)

# -------------------- Метод локтя --------------------
wss <- numeric()
for (k in 1:10) {
  km_res <- kmeans(scaled_data, centers = k, nstart = 25)
  wss[k] <- km_res$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Количество кластеров",
     ylab = "Сумма внутрикластерных квадратов",
     main = "Метод локтя")

# -------------------- Метод силуэта --------------------
avg_sil <- numeric()
for (k in 2:10) {
  km_res <- kmeans(scaled_data, centers = k, nstart = 25)
  sil <- silhouette(km_res$cluster, dist(scaled_data))
  avg_sil[k] <- mean(sil[, 3])
}
plot(2:10, avg_sil[2:10], type = "b", pch = 19,
     xlab = "Количество кластеров",
     ylab = "Средняя силуэта",
     main = "Метод силуэта")

# -------------------- Статистика разрыва --------------------
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistic")

# -------------------- Метод консенсуса --------------------
nc <- NbClust(data = scaled_data, distance = "euclidean",
              min.nc = 2, max.nc = 6,
              method = "kmeans", index = "all")

# Столбчатая диаграмма голосов от различных индексов
votes <- table(nc$Best.nc[1, ])
barplot(votes,
        main = "Распределение голосов индексов NbClust",
        xlab = "Количество кластеров",
        ylab = "Количество индексов",
        col = "steelblue",
        ylim = c(0, max(votes) + 2))


# -------------------- Иерархическая кластеризация --------------------
set.seed(42)
sample_indices <- sample(1:nrow(data), size = 40)  # выберем 50 случайных объектов
sampled_data <- scaled_data[sample_indices, ]
sampled_ages <- data$age[sample_indices]

# Пересчёт расстояний и иерархии для выборки
d_sample <- dist(sampled_data)
hc_sample <- hclust(d_sample, method = "ward.D2")
plot(hc_sample, labels = sampled_ages,
     main = "Упрощённая дендрограмма (выборка из 40 объектов)",
     xlab = "Возраст", sub = "")
rect.hclust(hc_sample, k = 3, border = 2:4)


# -------------------- Средние значения по кластерам --------------------
normalized_data <- as.data.frame(scaled_data)
agg_norm <- aggregate(normalized_data, by = list(Cluster = clusters), FUN = mean)
barplot(
  as.matrix(agg_norm[, -1]),
  beside = TRUE,
  col = 2:(optimal_k + 1),
  main = "Средние значения по кластерам",
  xlab = "Переменные",
  ylab = "Нормализованные значения",
  legend.text = paste("Кластер", agg_norm$Cluster),
  args.legend = list(x = "topright", inset = c(0, 0))
)

# -------------------- Boxplots по переменным --------------------
par(mfrow = c(2, 3))  # Сетка 2x3
for (i in 1:ncol(scaled_data)) {
  boxplot(scaled_data[, i] ~ clusters,
          main = colnames(scaled_data)[i],
          col = 2:(optimal_k + 1),
          xlab = "Кластер",
          ylab = "Значение")
}
par(mfrow = c(1, 1))  # Возврат к одной панели

# -------------------- K-means кластеризация --------------------
set.seed(123)
km <- kmeans(scaled_data, centers = optimal_k, nstart = 25)
library(ggfortify)

autoplot(km, data = scaled_data, frame = TRUE, frame.type = 'norm',
         main = "K-means кластеризация", label = FALSE)

pairs(scaled_data, col = km$cluster, pch = 19, main = "K-means кластеризация (2D)")

# Выбираем первые 3 переменные для 3D графика
scaled_data_3d <- scaled_data[, c(1,2,4)]

# Строим 3D Scatterplot
scatterplot3d(scaled_data_3d, 
              color = km$cluster, 
              pch = 19, 
              main = "3D кластеризация (K-means)", 
              xlab = colnames(scaled_data)[1], 
              ylab = colnames(scaled_data)[2], 
              zlab = colnames(scaled_data)[4])



