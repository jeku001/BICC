install.packages('ggplot2')
install.packages('plotly')
install.packages('gapminder')
install.packages('dplyr')
library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

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

ggplotly(
  df %>%
    ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
    geom_line() +
    facet_wrap(~ Rok) +
    theme_light() +
    theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = 'Month', y = 'Indicator')
  )
