# SDS 301: Airbnb Pricing Regression Analysis

## Структура проекта

```
├── data/                    # Исходные данные
│   ├── listings_gz.csv     # Детальная информация о листингах
│   ├── listings.csv        # Краткая информация о листингах
│   ├── reviews.csv         # Отзывы
│   ├── reviews_gz.csv      # Детальные отзывы
│   └── neighbourhoods.csv  # Районы
├── results/                 # Результаты анализа
│   ├── plots/              # Графики EDA и диагностики
│   ├── model_comparison.csv
│   ├── cv_results.csv
│   └── final_model_coefficients.csv
├── airbnb_regression_analysis.R  # Основной скрипт анализа
└── README.md
```

## Описание

Проект включает:
1. **EDA (Exploratory Data Analysis)** - исследовательский анализ данных
2. **Моделирование** - построение множественных линейных регрессионных моделей
3. **Диагностика** - проверка моделей на мультиколлинеарность, выбросы, нормальность остатков
4. **Выбор модели** - сравнение моделей с помощью F-тестов и cross-validation

## Результаты

### Лучшая модель
- **Model 4** (с взаимодействиями): R² = 0.538, Adjusted R² = 0.534
- **Model 5** (stepwise): R² = 0.533, Adjusted R² = 0.529

### Метрики качества
- Средняя абсолютная ошибка (MAE): ~148 евро
- Среднеквадратичная ошибка (RMSE): ~1995 евро

## Запуск

1. Установите необходимые R пакеты:
```r
install.packages(c('readr', 'dplyr', 'ggplot2', 'corrplot', 'car', 'MASS', 'broom', 'gridExtra'))
```

2. Запустите основной скрипт:
```r
source("airbnb_regression_analysis.R")
```

## Авторы
# Yerkezhan Burambay 
# Zhanahmetov Ansar
# Ruslan Kudaibergenov
# Alibek Aitbekov

<<<<<<< HEAD
- **Yerkezhan Burambay**
- **Zhanahmetov Ansar**
- **Ruslan Kudaibergenov**
- **Alibek Aitbekov**

SDS 301 Modern Regression Analysis - Final Project
=======
>>>>>>> c14e237a0486b3a1f623261016a7005074650918

