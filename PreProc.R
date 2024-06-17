# Библиотеки

library(HSAUR)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(data.table)
library(reshape2)
library(reshape2)
library(GGally)
library(plotly)
library(factoextra)
library(caret)
library(corrplot)
library(cluster)
library(NbClust)
library(clusterCrit)
library(clusterSim)
library(fpc)

# Загрузка даннных

df = read.csv("bank_transactions.csv")

head(df)

str(df)
dim(df)
describe(df)

# Проверка данных на 'чистоту'

check <- function(df) {
  l <- list()
  columns <- colnames(df)
  for (col in columns) {
    dtypes <- class(df[[col]])
    nunique <- length(unique(df[[col]]))
    sum_null <- sum(df[[col]] == 0, na.rm = TRUE) + nrow(subset(df, df[[col]] == "nan")) + nrow(subset(df, df[[col]] == ""))
    l[[length(l) + 1]] <- data.frame(column = col, dtypes = dtypes, nunique = nunique, sum_null = sum_null, stringsAsFactors = FALSE)
  }
  df_check <- do.call(rbind, l)
  return(df_check)
}

View(check(df))

# Удаление пустых значений

df[df=="nan"] = NA
df[df==''] = NA
df[df==0] = NA

df = na.omit(df)
dim(df)

sum(duplicated(df))

# Преобразование признаков дат

df$TransactionDate <- as.Date(df$TransactionDate, format= "%d/%m/%Y")
df$CustomerDOB <- as.Date(df$CustomerDOB, format= "%d/%m/%Y")

head(df)

# Прербразование возраста клиентов для получения количественного параметра

df$CustomerDOB <- as.Date(ifelse(as.numeric(format(df$CustomerDOB, "%Y")) >= 00, as.numeric(format(df$CustomerDOB, "%Y")) + 2000, as.numeric(format(df$CustomerDOB, "%Y")) + 1900), format = "%Y-%m-%d")
df$TransactionDate <- as.Date(ifelse(as.numeric(format(df$TransactionDate, "%Y")) >= 00, as.numeric(format(df$TransactionDate, "%Y")) + 2000, as.numeric(format(df$TransactionDate, "%Y")) + 1900), format = "%Y-%m-%d")
df$CustomerAge <- as.numeric(format(as.Date(df$TransactionDate), "%Y")) - as.numeric(format(as.Date(df$CustomerDOB), "%Y"))

head(df)

# Удаление признака TransactionTime и неправильно определенных полов клиентов

df <- subset(df, select = -TransactionTime)
sort(table(df$CustGender), decreasing = TRUE)
df <- subset(df, subset = CustGender != 'T')

theme_set(theme_minimal())

df = read.csv("RFM_df.csv")

# 1 Создание диаграммы бокс-усов CustAccountBalance

boxplot(log(df$CustAccountBalance),
        main = "Boxplot of CustAccountBalance",
        xlab = "CustAccountBalance",
        horizontal = TRUE)

filtered_df <- subset(df, CustAccountBalance < 100000)
ggplot(data = filtered_df, aes(x = "", y = CustAccountBalance)) +
  stat_boxplot(geom = "errorbar",      # Error bars
               width = 0.2) +
  geom_boxplot(fill = "#4271AE",       # Box color
               outlier.colour = "red", # Outliers color
               alpha = 0.9) +          # Box color transparency
  ggtitle("Boxplot of CustAccountBalance") + # Plot title
  xlab("") +   # X-axis label
  coord_flip() # Horizontal boxplot


setnames(df, "TransactionAmount..INR.", "TransactionAmount")

# 2 Создание диаграммы бокс-усов TransactionAmount

boxplot(log(df$TransactionAmount),
        main = "Boxplot of TransactionAmount",
        xlab = "TransactionAmount",
        horizontal = TRUE)

filtered_df <- subset(df, TransactionAmount < 5000)
ggplot(data = filtered_df, aes(x = "", y = TransactionAmount)) +
  stat_boxplot(geom = "errorbar",      # Error bars
               width = 0.2) +
  geom_boxplot(fill = "#35EF98",       # Box color
               outlier.colour = "red", # Outliers color
               alpha = 0.9) +          # Box color transparency
  ggtitle("Boxplot of TransactionAmount") + # Plot title
  xlab("") +   # X-axis label
  coord_flip() # Horizontal boxplot

# 3 Создание диаграммы бокс-усов CustomerAge

ggplot(data = df, aes(x = "", y = CustomerAge)) +
  stat_boxplot(geom = "errorbar",      # Error bars
               width = 0.2) +
  geom_boxplot(fill = "#ffe033",       # Box color
               outlier.colour = "red", # Outliers color
               alpha = 0.9) +          # Box color transparency
  ggtitle("Boxplot of CustomerAge") + # Plot title
  xlab("") +   # X-axis label
  coord_flip() # Horizontal boxplot

# Фильтрация возникших неверных данных

df <- subset(df, CustomerAge >= 0)
df <- subset(df, CustomerDOB != "1800-01-01")

df <- df %>%
  mutate(TransactionDate1 = TransactionDate,
         TransactionDate2 = TransactionDate)

# Группировка и агрегирование данных

df$TransactionDate1 <- df$TransactionDate
df$TransactionDate2 <- df$TransactionDate

# Группируем данные по CustomerID и вычисляем различные метрики

RFM_df <- df %>%
  group_by(CustomerID) %>%
  summarise(
    Transaction_Count = n(),
    CustGender = first(CustGender),
    CustLocation = first(CustLocation),
    CustAccountBalance_Mean = mean(CustAccountBalance, na.rm = TRUE),
    TransactionAmount_Mean = mean(`TransactionAmount (INR)`, na.rm = TRUE),
    CustomerAge_Median = median(CustomerAge, na.rm = TRUE),
    Last_Transaction_Date = max(TransactionDate2, na.rm = TRUE),
    First_Transaction_Date = min(TransactionDate1, na.rm = TRUE),
    Median_Transaction_Date = median(TransactionDate, na.rm = TRUE)
  )

# Сбрасываем группировку и выводим первые 5 строк

RFM_df <- RFM_df %>% ungroup()
head(RFM_df)

# Переименовываем столбец TransactionID в Frequency

colnames(RFM_df)[colnames(RFM_df) == "TransactionID"] <- "Frequency"

# Вычисляем разницу между датами последней и первой транзакции

RFM_df$Recency <- as.integer(difftime(RFM_df$Last_Transaction_Date,
                                      RFM_df$First_Transaction_Date,
                                      units = "days"))

# Вычисление корреляции

correlation <- cor(RFM_df)

# Создание тепловой карты корреляции

ggplot(data = reshape2::melt(correlation),
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red",
                       mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(value, 3)),
            color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_fixed()

# Диаграмма рассеяния числовых признаков

sample_df <- RFM_df %>% sample_n(size = 5000)
ggpairs(sample_df, title = "Pairplot of numeric values")

# Распределение частотности заказа

ggplot(data = RFM_df, aes(x = Frequency)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
  labs(title = "Frequency",
       x = "Frequency",
       y = "Count",
       fill = "Frequency") +
  theme_minimal()

# Создание гистограммы распределения возраста покупателей

p <- plot_ly(RFM_df, x = ~CustomerAge, color = ~CustGender, type = "histogram") %>%
  layout(title = "Age Distribution of Customers",
         xaxis = list(title = "Age"),
         yaxis = list(title = "Count"),
         barmode = "stack")
p

factors <- ifelse(RFM_df$CustGender == 1, "Male", "Female")

# Оценка соотношения полов клиентов

ggplot(data = RFM_df, aes(x = CustGender, fill = factors)) +
  geom_bar(width = 0.7) +
  labs(title = "Gender Distribution of Customers",
       x = "Gender",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue", "tomato"))

# Итоговая таблица

View(head(RFM_df, 10))