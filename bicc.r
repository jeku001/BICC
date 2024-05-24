# install.packages('ggplot2')
# install.packages('plotly')
# install.packages('gapminder')
# install.packages('dplyr')
# install.packages('scales')
library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

# Wykres calosciowy -------------------------------------------------------

limits <- NULL
for (i in 1:22) {
  if (i %in% seq(1, 22, by = 2)) {
    limits <- c(limits, rep(NA,12))
    next
  }
  limits <- c(limits, i+2000, rep(NA,11))
}

df$full_date <- as.Date(paste(df$Rok, df$M.c, "01", sep = "-"))

df %>%
  ggplot(aes(x = full_date, y = Wskaznik)) +
  geom_line() +
  labs(x = 'Time', y = 'Wskaznik') +
  scale_x_date()

# sprawdzenie rozkladu wskaznika

yr <- 2000
while (yr < 2022) {
  print(shapiro.test((df %>% filter(Rok == yr))$Wskaznik))
  yr <- yr + 1
}
shapiro.test(df$Wskaznik[df$Rok==2020])


# tabela srednich i odchylen wskaznikow - srednie roczne

View(df %>%
  group_by(Rok) %>%
  summarise(mean_indicator = mean(Wskaznik),
            sd_indicator = sd(Wskaznik)))

# wykres zmiany wskaznikow
length(df[,1])
ggplotly(
  df %>%
    ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
    geom_line() +
    facet_wrap(~ Rok) +
    theme_light() +
    theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = 'Month', y = 'Indicator')
  )
