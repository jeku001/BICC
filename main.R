
library(readxl)
data <- read_excel("BICC_2024_dane.xlsx", 
                   range = "C6:C270")
dataDF = data.frame(data)

