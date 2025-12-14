# Dockerfile для R окружения
# Базовый образ с R
FROM rocker/r-ver:4.5.2

# Установка системных зависимостей
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Установка R пакетов
RUN R -e "install.packages(c('readr', 'dplyr', 'ggplot2', 'corrplot', 'car', 'MASS', 'broom', 'gridExtra'), repos='https://cran.rstudio.com/')"

# Создание рабочей директории
WORKDIR /app

# Копирование файлов проекта
COPY data/ ./data/
COPY airbnb_regression_analysis.R ./

# По умолчанию запускаем анализ
CMD ["Rscript", "airbnb_regression_analysis.R"]

