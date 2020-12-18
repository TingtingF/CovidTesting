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


 data <- data %>% 
  mutate (Måned = factor(dplyr::case_when(
    month==1 ~ "januar",
    month==2 ~ "februar",
    month==3 ~ "mars",
    month==4 ~ "april",
    month==5 ~ "mai",
    month==6 ~ "juni",
    month==7 ~ "juli",
    month==8 ~ "august",
    month==9 ~ "september",
    month==10 ~ "oktober",
    month==11 ~ "november",
    month==12 ~ "desember") )) %>% 
    rename(År=year)


# totalt antall tilfeller og maksimale tilfeller og død etter kontinenter
antall <- data %>% 
  rename(Kontinent=continentExp) %>% 
  group_by(År,Måned,Kontinent,) %>% 
  summarise(tilfeller_sum = sum(cases), tilfeller_max = max(cases),død_sum = sum(deaths), død_max = max(deaths)) %>% 
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
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y/%m/%d")
PlotNorUS <- USplot + geom_line(data=Norge, 
                               aes(date, as.numeric(CumulativeNumber)),
                               color="deeppink",
                               alpha = 0.5) + 
                      geom_line(data=spain, 
                              aes(date, as.numeric(CumulativeNumber)),
                              color="darkorange",
                              alpha = 0.4)+
                      geom_line(data=italy, 
                                aes(date, as.numeric(CumulativeNumber)),
                                color="#009E73",
                              alpha = 0.9) + 
                     ylab("Kumulativ antall for 14 dager per 100,000 innbyggere")+
                     labs(title="Norge (rosa), US (blå), Italy(grønn), & Spain (gul)")
jpeg('PlotNorUS.jpg')
plot(PlotNorUS)
dev.off()


