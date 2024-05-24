df

library(tidyr)
library(dplyr)

# Przeksztalcenie danych na rok ~ miesiac
# (przydatne moze kiedys ale tu jednak nie)

dfdf <- df %>%
  pivot_wider(names_from = M.c, values_from = Wskaznik)

dfdf <- as.data.frame(dfdf)
rownames(dfdf) <- dfdf$Rok
dfdf$Rok <- NULL
dfdf

# ANOVA

df_anova <- aov(Wskaznik ~ factor(Rok) + factor(M.c), data = df)
summary(df_anova)

# Zmienne rok i miesiac istotne dla wartosci wskaznika