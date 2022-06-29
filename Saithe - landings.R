library(icesSAG)
library(tidyverse)
library(patchwork)

#Get the assesment keys for the year
assessmentKeys <- findAssessmentKey("pok", 2021)
#https://standardgraphs.ices.dk/stockList.aspx

#Download all the data
# pok <- getStockDownloadData(assessmentKeys)
# 
# pok <- data.frame(pok)

#Find out which stock belongs to which key
#head(data.frame(getStockDownloadData(14168))) %>% select("AssessmentKey", "StockDescription")


pok_NEA <- data.frame(getStockDownloadData(14168))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

pok_North_Sea <- data.frame(getStockDownloadData(14326))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

pok_ICE <- data.frame(getStockDownloadData(14594))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

pok_ICE_2 <- read.csv("data/pok_Iceland.csv") #data from https://dt.hafogvatn.is/astand/2022/3_POK_is.html

pok_ICE <- bind_rows(pok_ICE, pok_ICE_2)

pok_Faroe <- data.frame(getStockDownloadData(16888))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")



stocknames <- c("Barents Sea and Norwegian Sea",
                "North Sea",
                "Iceland and East Greenland", 
                "Faroe Plateau")

pok <- bind_rows(pok_NEA, 
                 pok_North_Sea, 
                 pok_ICE,
                 pok_Faroe,
                 .id="stocknum") %>% 
  mutate (Stock = case_when(stocknum == 1 ~ "Barents Sea and Norwegian Sea", 
                            stocknum == 2 ~ "North Sea, Rockall and West of \nScotland, Skagerrak and Kattegat",
                            stocknum == 3 ~ "Iceland and East Greenland",
                            stocknum == 4 ~ "Faroe Plateau")) %>% 
  filter(Year < 2021) %>% 
  mutate(Landings = case_when (is.na(Landings) ~ Catches, TRUE ~ Landings)) %>% 
  mutate(Stock = as.factor(Stock)) %>% 
  mutate(fct_reorder(Stock, Landings, .fun= sum))

#Total landings for time period
pok %>% group_by(Stock) %>% summarise(sum = sum(Landings))

pok_g1 <- pok %>% filter(Year < 2022) %>% 
  
  ggplot(aes(Year, y =Landings/1000, fill = fct_reorder(Stock, Landings, .fun = sum)))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,800), breaks=seq(0, 7000, by=100),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2021.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Stock")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=24),
        legend.text=element_text(size=24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
pok_g1

jpeg("Saithe landings east.jpeg", width = 1400, height = 700)
pok_g1
dev.off()

pok_west <- read.csv("Data/pok_west Atlantic.csv")
#data from https://s3.us-east-1.amazonaws.com/nefmc.org/Prepublication-NE-Grndfsh-10-3-2019.pdf and 
#https://www.dfo-mpo.gc.ca/stats/commercial/sea-maritimes-eng.htm


pok_g2 <- 
  pok_west %>% filter(Year < 2019) %>% mutate(Area = as.factor(Country)) %>% 
  
  ggplot(aes(Year, Landings/1000, fill = fct_reorder(Area, Landings, .fun = sum)))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,50), breaks=seq(0, 100, by=10),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2020.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Country")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.2, 1.0, 0.2, 0.2), "cm"),
        panel.background = element_blank())
pok_g2


pok_g1 + pok_g2 + plot_layout(ncol = 1) & theme(legend.justification = "left")


jpeg("Saithe landings.jpeg", width = 1400, height = 1400)
pok_g1 + pok_g2 + plot_layout(ncol = 1) & theme(legend.justification = "left")
dev.off()
