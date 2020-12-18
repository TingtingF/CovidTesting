## installere pakker
install.packages("pacman")
library(pacman) 
pacman::p_load(readxl, reshape2,dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,sdcTable,knitr, kableExtra, easySdcTable,  SmallCountRounding,Epi,plotly,writexl)

# Får et blikk av datasett
data <- read_excel("Covid.xlsx")
head(data )
summary(data ) 
str(data)


# totalt antall tilfeller og maksimale tilfeller og død etter kontinenter
antall <- data %>% 
  rename(Kontinent=continentExp,År = year, Måned= month) %>% 
  group_by(År,Måned,Kontinent,) %>% 
  summarise(tilfeller_sum = sum(cases), tilfeller_mean = mean(cases), tilfeller_max = max(cases),død_sum = sum(deaths), død_mean = mean(deaths),død_max = max(deaths)) %>% 
  arrange(År,Måned)

# ekspotere resultat til excel-format
write_xlsx(antall,"resultat_antall.xlsx")


#Plotting antall tilfeller og dødsfall ifølge dato for å identifisere trender. 
# Sørge for riktig dato-format 
data$date <- as.Date(with(data, paste(year, month, day,sep="-")), "%Y-%m-%d")
summary(data$date)

# subsetting av datasettet.
Norge <- data %>%
  filter(countriesAndTerritories == "Norway") 

spain <- data %>%
  filter(countriesAndTerritories == "Spain") 

italy <- data %>%
  filter(countriesAndTerritories == "Italy") 


us <- data %>%
  filter(countriesAndTerritories == "United_States_of_America") 

# plot Norge og US

USplot <- ggplot(us, 
                 aes(date, as.numeric(CumulativeNumber))) +
  geom_line(color="blue") + 
  theme_minimal(base_size = 14)+ xlab(NULL) + ylab(NULL) 
  
Norge_US <- USplot + geom_line(data=Norge, 
                               aes(date, as.numeric(CumulativeNumber)),
                               color="red",
                               alpha = 0.5)
Nor_US_Sp <- Norge_US + geom_line(data=spain, 
                                  aes(date, as.numeric(CumulativeNumber)),
                                  color="#E69F00",
                                  alpha = 0.4)
PlotTrend <- Nor_US_Sp + geom_line(data=italy, 
                                      aes(date, as.numeric(CumulativeNumber)),
                                      color="darkorchid",
                                      alpha = 0.9) + 
                                   labs(title="Norge (ræd), US (blå), Italy(orchid), & Spain (gul)")+ 
                                   ylab("Kumulativ antall for 14 dager per 100,000 innbyggere") 

ggsave("PlotTrend.png")

ggsave("PlotTrend.jpg")
