# install.packages('prophet')

library(prophet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

df
df_dates <- df
df_dates$date <- as.Date(paste(df$Rok, df$M.c, "01", sep = "-"))
df_dates <- subset(df_dates, select = c(date, Wskaznik))
colnames(df_dates) <- c('ds', 'y')
model_prophet <- prophet(df_dates)
future <- make_future_dataframe(model_prophet,
                                periods = 24,
                                freq = 'month')
forecast <- predict(model_prophet, future)
forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')] %>%
  tail(n = 24)

prophet_plot_components(model_prophet, forecast)

# Bez 2020 -----------------------------------------------------------------

df_no_2020<- df %>% subset(Rok!=2020)

df_dates2 <- df_no_2020
df_dates2$date <- as.Date(paste(df_no_2020$Rok, df_no_2020$M.c, "01", sep = "-"))
df_dates2 <- subset(df_dates2, select = c(date, Wskaznik))
colnames(df_dates2) <- c('ds', 'y')
model_prophet2 <- prophet(df_dates2)
future2 <- make_future_dataframe(model_prophet2,
                                periods = 24,
                                freq = 'month')
forecast2 <- predict(model_prophet2, future2)
forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')] %>%
  tail(n = 24)

prophet_plot_components(model_prophet2, forecast2)

# Wykres bez 2020------------------------------------------------------------------

forecast_plot <- forecast2[c('ds', 'yhat')]
forecast_plot$Rok <- year(forecast_plot$ds)
forecast_plot$M.c <- month(forecast_plot$ds)

ggplotly(
  forecast_plot %>%
    ggplot(aes(x = M.c, y = yhat, color = Rok)) +
    geom_line() +
    facet_wrap(~ Rok) +
    theme_light() +
    theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = 'Month', y = 'Indicator')
)

# Wykres z 2020 -----------------------------------------------------------

forecast_plot2 <- forecast[c('ds', 'yhat')]
forecast_plot2$Rok <- year(forecast_plot2$ds)
forecast_plot2$M.c <- month(forecast_plot2$ds)

ggplotly(
  forecast_plot2 %>%
    ggplot(aes(x = M.c, y = yhat, color = Rok)) +
    geom_line() +
    facet_wrap(~ Rok) +
    theme_light() +
    theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = 'Month', y = 'Indicator')
)

# Sprawdzenie róznic dla danych z 2020 i bez 2020 -------------------------

predicted_years_plot <- subset(forecast_plot,  Rok %in% c(2022, 2023))$yhat -
  subset(forecast_plot2, Rok %in% c(2022, 2023))$yhat
predicted_years_plot <- as.data.frame(predicted_years_plot)
predicted_years_plot <- 
  predicted_years_plot %>% 
  cbind(c(rep(2022, 12), rep(2023, 12))) %>%
  cbind(c(1:12, 1:12))
colnames(predicted_years_plot) <- c('Wskaznik', 'Rok', 'M.c')

predicted_years_plot %>%
  ggplot(aes(x = M.c, y = predicted_years_plot$'Wskaznik')) +
  geom_point() +
  facet_wrap(~ Rok) +
  theme_light() +
  labs(x = 'Month', y = 'Indicator')



predicted_years_plot2 <- data.frame(
  Wskaznik_bez2020 = tail(forecast_plot2$yhat, n = 24),
  Wskaznik_z2020 = tail(forecast_plot$yhat, n = 24),
  Rok = c(rep(2022, 12), rep(2023, 12)),
  M.c = c(1:12, 1:12)
)

predicted_years_plot2 %>%
  ggplot(aes(x = M.c)) +
  geom_point(aes(y = Wskaznik_bez2020, color = "Wskaznik_bez2020")) +
  geom_point(aes(y = Wskaznik_z2020, color = "Wskaznik_z2020")) +
  geom_segment(aes(x = M.c, xend = M.c, y = Wskaznik_bez2020, yend = Wskaznik_z2020, color = "Segment")) +
  facet_wrap(~Rok) +
  labs(x = "M.c", y = "Wskaznik", title = "Wskaznik bez 2020 vs Wskaznik z 2020") +
  scale_color_manual(values = c("Wskaznik_bez2020" = "red", "Wskaznik_z2020" = "blue", "Segment" = "purple"),
                     labels = c("Roznica", "Przewidziany wskaznik kiedy\nbierzemy pod uwage rok 2020",
                                "\nPrzewidziany wskaznik kiedy\npomijamy rok 2020"),
                     name = "Wartosci wskaznika", drop = FALSE) +
  scale_x_continuous(breaks = 1:12)
  