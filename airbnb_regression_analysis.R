# ============================================================================
# Airbnb Pricing Regression Analysis
# SDS 301 Modern Regression Analysis: Final Project
# ============================================================================

# Добавляем пользовательскую библиотеку в путь (если пакеты установлены туда)
userLib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", 
                     paste(R.version$major, R.version$minor, sep = "."))
if (dir.exists(userLib)) {
  .libPaths(c(userLib, .libPaths()))
}

# Загрузка необходимых библиотек
library(readr)      # для чтения CSV файлов
library(dplyr)      # для работы с данными
library(ggplot2)    # для графиков
library(corrplot)   # для корреляционных матриц
library(car)        # для диагностики моделей
library(MASS)       # для stepwise regression
library(broom)      # для работы с моделями
library(gridExtra)  # для компоновки графиков

# ============================================================================
# 1. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ
# ============================================================================

cat("=== ЗАГРУЗКА ДАННЫХ ===\n")

# Загрузка основного датасета
listings <- read_csv("data/listings_gz.csv", show_col_types = FALSE)
cat("Загружено строк:", nrow(listings), "\n")
cat("Загружено столбцов:", ncol(listings), "\n")

# Просмотр структуры данных
cat("\n=== СТРУКТУРА ДАННЫХ ===\n")
str(listings, give.attr = FALSE)

# Просмотр первых строк
cat("\n=== ПЕРВЫЕ СТРОКИ ===\n")
head(listings)

# ============================================================================
# 2. ПРЕДВАРИТЕЛЬНАЯ ОБРАБОТКА ДАННЫХ
# ============================================================================

cat("\n=== ПРЕДВАРИТЕЛЬНАЯ ОБРАБОТКА ===\n")

# Преобразование цены из формата "$132.00" в числовой
# Удаляем символы $ и запятые, преобразуем в числовой формат
listings$price_numeric <- as.numeric(gsub("[$,]", "", listings$price))
cat("Пропусков в цене:", sum(is.na(listings$price_numeric)), "\n")

# Удаляем строки с пропущенными ценами
listings_clean <- listings %>% 
  filter(!is.na(price_numeric) & price_numeric > 0)

cat("Строк после удаления пропусков:", nrow(listings_clean), "\n")

# Проверка на выбросы в цене
cat("\n=== СТАТИСТИКА ПО ЦЕНЕ ===\n")
summary(listings_clean$price_numeric)

# ============================================================================
# 3. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================

cat("\n=== EXPLORATORY DATA ANALYSIS ===\n")

# Создаем директорию для графиков
if (!dir.exists("plots")) {
  dir.create("plots")
}

# 3.1 Распределение цены
p1 <- ggplot(listings_clean, aes(x = price_numeric)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Распределение цен на жилье",
       x = "Цена (евро)", y = "Частота") +
  theme_minimal()

p2 <- ggplot(listings_clean, aes(x = log(price_numeric))) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Распределение логарифма цен",
       x = "log(Цена)", y = "Частота") +
  theme_minimal()

ggsave("plots/01_price_distribution.png", 
       grid.arrange(p1, p2, ncol = 2), 
       width = 12, height = 5)

# 3.2 Цена по типу комнаты
p3 <- ggplot(listings_clean, aes(x = room_type, y = price_numeric)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Цена по типу комнаты",
       x = "Тип комнаты", y = "Цена (евро)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/02_price_by_room_type.png", p3, width = 10, height = 6)

# 3.3 Цена по районам
# Берем топ-10 районов по количеству объявлений
top_neighbourhoods <- listings_clean %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  head(10) %>%
  pull(neighbourhood_cleansed)

listings_top_neigh <- listings_clean %>%
  filter(neighbourhood_cleansed %in% top_neighbourhoods)

p4 <- ggplot(listings_top_neigh, 
             aes(x = reorder(neighbourhood_cleansed, price_numeric, median), 
                 y = price_numeric)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Цена по районам (топ-10)",
       x = "Район", y = "Цена (евро)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave("plots/03_price_by_neighbourhood.png", p4, width = 10, height = 8)

# 3.4 Числовые переменные
cat("\n=== ЧИСЛОВЫЕ ПЕРЕМЕННЫЕ ===\n")
numeric_vars <- listings_clean %>%
  dplyr::select(price_numeric, accommodates, bathrooms, bedrooms, beds, 
         minimum_nights, number_of_reviews, reviews_per_month,
         calculated_host_listings_count, availability_365,
         review_scores_rating)

cat("Количество числовых переменных:", ncol(numeric_vars), "\n")
print(summary(numeric_vars))

# 3.5 Корреляционная матрица
cor_matrix <- cor(numeric_vars, use = "complete.obs")
png("plots/04_correlation_matrix.png", width = 1000, height = 1000)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")
dev.off()

# 3.6 Графики зависимостей
p5 <- ggplot(listings_clean, aes(x = accommodates, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Цена vs Вместимость",
       x = "Вместимость", y = "Цена (евро)") +
  theme_minimal()

p6 <- ggplot(listings_clean, aes(x = bedrooms, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Цена vs Количество спален",
       x = "Спальни", y = "Цена (евро)") +
  theme_minimal()

p7 <- ggplot(listings_clean, aes(x = number_of_reviews, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Цена vs Количество отзывов",
       x = "Количество отзывов", y = "Цена (евро)") +
  theme_minimal()

p8 <- ggplot(listings_clean, aes(x = review_scores_rating, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Цена vs Рейтинг",
       x = "Рейтинг", y = "Цена (евро)") +
  theme_minimal()

ggsave("plots/05_scatterplots.png", 
       grid.arrange(p5, p6, p7, p8, ncol = 2), 
       width = 12, height = 10)

cat("\nEDA завершен. Графики сохранены в папку plots/\n")

# ============================================================================
# 4. ПОДГОТОВКА ДАННЫХ ДЛЯ МОДЕЛИРОВАНИЯ
# ============================================================================

cat("\n=== ПОДГОТОВКА ДАННЫХ ДЛЯ МОДЕЛИРОВАНИЯ ===\n")

# Создаем датасет для моделирования
model_data <- listings_clean %>%
  dplyr::select(
    # Зависимая переменная
    price = price_numeric,
    
    # Категориальные переменные
    room_type,
    neighbourhood_cleansed,
    host_is_superhost,
    instant_bookable,
    
    # Числовые переменные
    accommodates,
    bathrooms,
    bedrooms,
    beds,
    minimum_nights,
    number_of_reviews,
    reviews_per_month,
    calculated_host_listings_count,
    availability_365,
    review_scores_rating,
    review_scores_accuracy,
    review_scores_cleanliness,
    review_scores_checkin,
    review_scores_communication,
    review_scores_location,
    review_scores_value
  ) %>%
  # Удаляем строки с пропусками в ключевых переменных
  filter(!is.na(accommodates) & 
         !is.na(bedrooms) & 
         !is.na(beds) &
         !is.na(room_type) &
         !is.na(neighbourhood_cleansed))

cat("Строк в датасете для моделирования:", nrow(model_data), "\n")
cat("Пропусков:\n")
print(colSums(is.na(model_data)))

# Заполняем пропуски в bathrooms медианой
model_data$bathrooms[is.na(model_data$bathrooms)] <- median(model_data$bathrooms, na.rm = TRUE)

# Заполняем пропуски в review scores медианой
review_cols <- c("review_scores_rating", "review_scores_accuracy", 
                 "review_scores_cleanliness", "review_scores_checkin",
                 "review_scores_communication", "review_scores_location",
                 "review_scores_value")

for (col in review_cols) {
  model_data[[col]][is.na(model_data[[col]])] <- median(model_data[[col]], na.rm = TRUE)
}

# Преобразуем логические переменные
model_data$host_is_superhost <- as.logical(model_data$host_is_superhost == "t")
model_data$instant_bookable <- as.logical(model_data$instant_bookable == "t")

cat("\nФинальный размер датасета:", nrow(model_data), "строк\n")
cat("Пропусков после обработки:", sum(is.na(model_data)), "\n")

# Сохраняем подготовленный датасет
write_csv(model_data, "data/model_data.csv")
cat("Подготовленный датасет сохранен в data/model_data.csv\n")

# ============================================================================
# 5. ПОСТРОЕНИЕ МОДЕЛЕЙ РЕГРЕССИИ
# ============================================================================

cat("\n=== ПОСТРОЕНИЕ МОДЕЛЕЙ РЕГРЕССИИ ===\n")

# 5.1 Базовая модель (только числовые переменные)
cat("\n--- Модель 1: Базовая модель (числовые переменные) ---\n")
model1 <- lm(price ~ accommodates + bedrooms + beds + bathrooms + 
             minimum_nights + number_of_reviews + reviews_per_month +
             calculated_host_listings_count + availability_365 +
             review_scores_rating,
             data = model_data)

summary(model1)
cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")

# 5.2 Модель с категориальными переменными
cat("\n--- Модель 2: С категориальными переменными ---\n")
model2 <- lm(price ~ accommodates + bedrooms + beds + bathrooms + 
             minimum_nights + number_of_reviews + reviews_per_month +
             calculated_host_listings_count + availability_365 +
             review_scores_rating + room_type + neighbourhood_cleansed +
             host_is_superhost + instant_bookable,
             data = model_data)

summary(model2)
cat("R-squared:", summary(model2)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")

# 5.3 Модель с логарифмической трансформацией цены
cat("\n--- Модель 3: Логарифмическая трансформация цены ---\n")
model3 <- lm(log(price) ~ accommodates + bedrooms + beds + bathrooms + 
             minimum_nights + number_of_reviews + reviews_per_month +
             calculated_host_listings_count + availability_365 +
             review_scores_rating + room_type + neighbourhood_cleansed +
             host_is_superhost + instant_bookable,
             data = model_data)

summary(model3)
cat("R-squared:", summary(model3)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model3)$adj.r.squared, "\n")

# 5.4 Модель с взаимодействиями
cat("\n--- Модель 4: С взаимодействиями ---\n")
model4 <- lm(log(price) ~ accommodates + bedrooms + beds + bathrooms + 
             minimum_nights + number_of_reviews + reviews_per_month +
             calculated_host_listings_count + availability_365 +
             review_scores_rating + room_type + neighbourhood_cleansed +
             host_is_superhost + instant_bookable +
             room_type:accommodates + room_type:bedrooms,
             data = model_data)

summary(model4)
cat("R-squared:", summary(model4)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model4)$adj.r.squared, "\n")

# 5.5 Stepwise regression для выбора переменных
cat("\n--- Модель 5: Stepwise regression ---\n")
# Удаляем пропуски для stepwise regression
model_data_complete <- model_data[complete.cases(model_data), ]

model_null <- lm(log(price) ~ 1, data = model_data_complete)
model_full <- lm(log(price) ~ accommodates + bedrooms + beds + bathrooms + 
                 minimum_nights + number_of_reviews + reviews_per_month +
                 calculated_host_listings_count + availability_365 +
                 review_scores_rating + review_scores_accuracy +
                 review_scores_cleanliness + review_scores_checkin +
                 review_scores_communication + review_scores_location +
                 review_scores_value + room_type + neighbourhood_cleansed +
                 host_is_superhost + instant_bookable,
                 data = model_data_complete)

model5 <- stepAIC(model_null, 
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both", trace = FALSE)

summary(model5)
cat("R-squared:", summary(model5)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model5)$adj.r.squared, "\n")

# Сохраняем результаты моделей
models_list <- list(
  model1 = model1,
  model2 = model2,
  model3 = model3,
  model4 = model4,
  model5 = model5
)

save(models_list, file = "models.RData")
cat("\nМодели сохранены в models.RData\n")

# ============================================================================
# 6. ДИАГНОСТИКА МОДЕЛЕЙ
# ============================================================================

cat("\n=== ДИАГНОСТИКА МОДЕЛЕЙ ===\n")

# Выбираем лучшую модель для диагностики (model5 - stepwise)
best_model <- model5

# 6.1 Графики остатков
png("plots/06_residuals_diagnostics.png", width = 1200, height = 1000)

par(mfrow = c(2, 2))

# Остатки vs предсказанные значения
plot(fitted(best_model), residuals(best_model),
     xlab = "Предсказанные значения", ylab = "Остатки",
     main = "Остатки vs Предсказанные значения")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot для проверки нормальности остатков
qqnorm(residuals(best_model), main = "Q-Q Plot остатков")
qqline(residuals(best_model), col = "red")

# Масштабно-местоположение график
plot(fitted(best_model), sqrt(abs(residuals(best_model))),
     xlab = "Предсказанные значения", 
     ylab = "sqrt(|Остатки|)",
     main = "Масштабно-местоположение график")

# Остатки vs порядок наблюдений
plot(residuals(best_model), 
     xlab = "Порядок наблюдений", ylab = "Остатки",
     main = "Остатки vs Порядок наблюдений")
abline(h = 0, col = "red", lty = 2)

dev.off()

# 6.2 Проверка на мультиколлинеарность
cat("\n--- Проверка на мультиколлинеарность (VIF) ---\n")
vif_values <- vif(best_model)
print(vif_values)
cat("\nVIF > 10 указывает на проблему мультиколлинеарности\n")

# 6.3 Проверка на выбросы
cat("\n--- Проверка на выбросы ---\n")
# Cook's distance
cooks_d <- cooks.distance(best_model)
outliers_cooks <- which(cooks_d > 4/nrow(model_data))
cat("Выбросы по Cook's distance (> 4/n):", length(outliers_cooks), "\n")

# Стандартизированные остатки
standard_residuals <- rstandard(best_model)
outliers_standard <- which(abs(standard_residuals) > 3)
cat("Выбросы по стандартизированным остаткам (> 3):", length(outliers_standard), "\n")

# График Cook's distance
png("plots/07_cooks_distance.png", width = 800, height = 600)
plot(cooks_d, type = "h", 
     xlab = "Наблюдение", ylab = "Cook's Distance",
     main = "Cook's Distance")
abline(h = 4/nrow(model_data), col = "red", lty = 2)
dev.off()

# 6.4 Тест на нормальность остатков
cat("\n--- Тест на нормальность остатков (Shapiro-Wilk) ---\n")
# Берем выборку из 5000 наблюдений для теста (Shapiro-Wilk работает только до 5000)
sample_size <- min(5000, length(residuals(best_model)))
shapiro_test <- shapiro.test(sample(residuals(best_model), sample_size))
print(shapiro_test)

# 6.5 Тест на гетероскедастичность (Breusch-Pagan)
cat("\n--- Тест на гетероскедастичность (Breusch-Pagan) ---\n")
bp_test <- ncvTest(best_model)
print(bp_test)

cat("\nДиагностика завершена. Графики сохранены в папку plots/\n")

# ============================================================================
# 7. СРАВНЕНИЕ МОДЕЛЕЙ
# ============================================================================

cat("\n=== СРАВНЕНИЕ МОДЕЛЕЙ ===\n")

# Создаем таблицу сравнения моделей
model_comparison <- data.frame(
  Model = c("Model 1 (базовая)", 
            "Model 2 (+ категориальные)", 
            "Model 3 (log трансформация)",
            "Model 4 (+ взаимодействия)",
            "Model 5 (stepwise)"),
  R_squared = c(summary(model1)$r.squared,
                summary(model2)$r.squared,
                summary(model3)$r.squared,
                summary(model4)$r.squared,
                summary(model5)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared,
                    summary(model3)$adj.r.squared,
                    summary(model4)$adj.r.squared,
                    summary(model5)$adj.r.squared),
  AIC = c(AIC(model1),
          AIC(model2),
          AIC(model3),
          AIC(model4),
          AIC(model5)),
  BIC = c(BIC(model1),
          BIC(model2),
          BIC(model3),
          BIC(model4),
          BIC(model5))
)

print(model_comparison)

# Сохраняем таблицу сравнения
write_csv(model_comparison, "model_comparison.csv")
cat("\nТаблица сравнения моделей сохранена в model_comparison.csv\n")

# F-тесты для сравнения моделей
cat("\n--- F-тесты для сравнения моделей ---\n")
cat("Примечание: F-тесты требуют, чтобы модели были построены на одном датасете.\n")
cat("Модели 3, 4 и 5 используют log(price), поэтому их можно сравнивать.\n")

# Model 3 vs Model 4 (обе на model_data)
tryCatch({
  anova_test_3_4 <- anova(model3, model4)
  cat("\nModel 3 vs Model 4:\n")
  print(anova_test_3_4)
}, error = function(e) {
  cat("Не удалось сравнить Model 3 и Model 4:", e$message, "\n")
})

# Model 3 vs Model 5 (нужно перестроить model3 на model_data_complete)
tryCatch({
  model3_complete <- update(model3, data = model_data_complete)
  anova_test_3_5 <- anova(model3_complete, model5)
  cat("\nModel 3 vs Model 5:\n")
  print(anova_test_3_5)
}, error = function(e) {
  cat("Не удалось сравнить Model 3 и Model 5:", e$message, "\n")
})

# ============================================================================
# 8. CROSS-VALIDATION
# ============================================================================

cat("\n=== CROSS-VALIDATION ===\n")

# Простая k-fold cross-validation (k=5)
set.seed(123)
n <- nrow(model_data)
k <- 5
folds <- sample(rep(1:k, length.out = n))

cv_errors <- numeric(k)

for (i in 1:k) {
  train_data <- model_data[folds != i, ]
  test_data <- model_data[folds == i, ]
  
  # Обучаем модель на тренировочных данных
  cv_model <- update(best_model, data = train_data)
  
  # Предсказываем на тестовых данных
  predictions <- exp(predict(cv_model, newdata = test_data))
  actual <- test_data$price
  
  # Вычисляем RMSE
  cv_errors[i] <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
}

cat("RMSE для каждой fold:\n")
print(cv_errors)
cat("Средний RMSE:", mean(cv_errors), "\n")
cat("Стандартное отклонение RMSE:", sd(cv_errors), "\n")

# Сохраняем результаты CV
cv_results <- data.frame(
  Fold = 1:k,
  RMSE = cv_errors
)
write_csv(cv_results, "cv_results.csv")
cat("Результаты cross-validation сохранены в cv_results.csv\n")

# ============================================================================
# 9. ФИНАЛЬНАЯ МОДЕЛЬ И ИНТЕРПРЕТАЦИЯ
# ============================================================================

cat("\n=== ФИНАЛЬНАЯ МОДЕЛЬ ===\n")
cat("\nВыбранная модель: Model 5 (Stepwise Regression)\n")
cat("\nКоэффициенты модели:\n")
print(summary(best_model)$coefficients)

# Доверительные интервалы
cat("\nДоверительные интервалы (95%):\n")
print(confint(best_model))

# Сохраняем детальные результаты финальной модели
final_model_summary <- tidy(best_model)
final_model_summary$conf_low <- confint(best_model)[, 1]
final_model_summary$conf_high <- confint(best_model)[, 2]

write_csv(final_model_summary, "final_model_coefficients.csv")
cat("\nКоэффициенты финальной модели сохранены в final_model_coefficients.csv\n")

cat("\n=== АНАЛИЗ ЗАВЕРШЕН ===\n")
cat("Все результаты сохранены:\n")
cat("- Графики: папка plots/\n")
cat("- Модели: models.RData\n")
cat("- Сравнение моделей: model_comparison.csv\n")
cat("- Cross-validation: cv_results.csv\n")
cat("- Коэффициенты финальной модели: final_model_coefficients.csv\n")

