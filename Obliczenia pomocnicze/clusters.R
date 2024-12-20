library(stats)
library(dplyr)
library(ggplot2)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df_no_2020<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")



# klastrowanie marzec-lipiec ----------------------------------------------

data <- df
# Funkcja do klastrowania i przygotowania danych dla każdego miesiąca
cluster_month <- function(month, data, k = 2) {
  month_data <- data %>%
    filter(M.c == month) %>%
    select(Rok, Wskaznik)
  
  clusters <- kmeans(month_data$Wskaznik, centers = k)
  
  month_data$Cluster <- as.factor(clusters$cluster)
  month_data$Month <- month
  
  return(month_data)
}

months <- c(3, 4, 5, 6, 7)
clustered_data <- do.call(rbind, lapply(months, cluster_month, data = data))
clustered_data$Month <- factor(clustered_data$Month, levels = months, labels = month_names[months])

# Wizualizacja wyników klasteryzacji
ggplot(clustered_data, aes(x = Rok, y = Wskaznik, color = Cluster)) +
  geom_point(size = 2) +  
  labs(title = "Klasteryzacja wartości wskaźnika (Marzec - Lipiec, 2000-2021)",
       x = "Rok",
       y = "Wartość wskaźnika") +
  scale_x_continuous(breaks = seq(2000, 2021, by = 1)) +  # Dodanie lat na osi X
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),  
    axis.text.x = element_text(angle = 90, hjust = 1)  # Obrót tekstu na osi X
  ) +
  facet_wrap(~ Month, scales = "free_y", ncol = 1)


# regresja liniowa dla kazdego miesiaca -----------------------------------

data <- df

# Funkcja do regresji i predykcji
predict_monthly <- function(data, month) {
  # Filtrujemy dane dla wybranego miesiąca
  monthly_data <- data %>% filter(M.c == month)
  
  # Model regresji liniowej
  model <- lm(Wskaznik ~ Rok, data = monthly_data)
  
  # Przewidywane lata
  future_years <- data.frame(Rok = c(2022, 2023), M.c = month)
  
  # Przewidywanie wartości
  predictionsWith2020 <- predict(model, newdata = future_years)

  return(data.frame(Rok = future_years$Rok, M.c = future_years$M.c, Wskaznik = predictionsWith2020))
}

# Aplikowanie funkcji dla każdego miesiąca
predictionsWith2020 <- lapply(1:12, function(m) predict_monthly(data, m))

# Łączenie wyników
predictionsWith2020_df <- do.call(rbind, predictionsWith2020)

# Łączenie oryginalnych danych z predykcjami
combined_data <- bind_rows(data, predictions_df)

# wykres

combined_data %>%
  ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
  geom_line() +
  facet_wrap(~ Rok) +
  theme_light() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = 'Month', y = 'Indicator')


# regresja liniowa bez 2020 -----------------------------------------------

data <- df_no_2020

# Funkcja do regresji i predykcji
predict_monthly <- function(data, month) {
  # Filtrujemy dane dla wybranego miesiąca
  monthly_data <- data %>% filter(M.c == month)
  
  # Model regresji liniowej
  model <- lm(Wskaznik ~ Rok, data = monthly_data)
  
  # Przewidywane lata
  future_years <- data.frame(Rok = c(2022, 2023), M.c = month)
  
  # Przewidywanie wartości
  predictionsWith2020 <- predict(model, newdata = future_years)
  
  return(data.frame(Rok = future_years$Rok, M.c = future_years$M.c, Wskaznik = predictionsWith2020))
}

# Aplikowanie funkcji dla każdego miesiąca
predictionsWithout2020 <- lapply(1:12, function(m) predict_monthly(data, m))

# Łączenie wyników
predictionsWuthout2020_df <- do.call(rbind, predictionsWithout2020)

# Łączenie oryginalnych danych z predykcjami
combined_data <- bind_rows(data, predictions_df)

# wykres

combined_data %>%
  ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
  geom_line() +
  facet_wrap(~ Rok) +
  theme_light() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = 'Month', y = 'Indicator')





