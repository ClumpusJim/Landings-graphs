library(icesSAG)
library(tidyverse)

#Get the assesment keys for the year
assessmentKeys <- findAssessmentKey("cod", 2020)
#https://standardgraphs.ices.dk/stockList.aspx

#Download all the data
# cod <- getStockDownloadData(assessmentKeys)
# 
# cod <- data.frame(cod)

#Find out which stock belongs to which key
head(data.frame(getStockDownloadData(14014))) %>% select("AssessmentKey", "StockDescription")

cod_GRE_West_Ins <- data.frame(getStockDownloadData(15621)) %>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.))) %>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

cod_GRE_West_Off <- data.frame(getStockDownloadData(15622))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_GRE_East <- data.frame(getStockDownloadData(16710))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_NEA <- data.frame(getStockDownloadData(16825))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

cod_NOR_Coast_N <- data.frame(getStockDownloadData(16721))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_NOR_Coast_S <- data.frame(getStockDownloadData(14575))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_ICE <- data.frame(getStockDownloadData(14591))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Kattegat <- data.frame(getStockDownloadData(14121))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

cod_Baltic_West <- data.frame(getStockDownloadData(16708))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Baltic_East <- data.frame(getStockDownloadData(14133))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_North_Sea <- data.frame(getStockDownloadData(14324))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

cod_Fareo_Plat <- data.frame(getStockDownloadData(16886))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Scotland_West <- data.frame(getStockDownloadData(15605))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Rockall <- data.frame(getStockDownloadData(13626))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Irish <- data.frame(getStockDownloadData(14433))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel", "StockDescription","Year", "Landings", "OfficialLandings", "Catches")

cod_Eng_Chann_Celtic <- data.frame(getStockDownloadData(14534))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

stocknames <- c("Western Greenland (Inshore)",
                "Western Greenland (Offshore)",
                "East Greenaland", 
                "Barents Sea",
                "Norwegian coastal cod (northern)",
                "Norwegian coastal cod (southern)",
                "Iceland",
                "Kattegat", 
                "Western Baltic",
                "Eastern Baltic",
                "North Sea", 
                "Fareo Islands", 
                "West of Scotland",
                "Rockall",
                "Irish Sea",
                "Celtic Sea and Eastern English Channel")

cod <- bind_rows(cod_GRE_West_Ins, 
                 cod_GRE_West_Off, 
                 cod_GRE_East,
                 cod_NEA,
                 cod_NOR_Coast_N,
                 cod_NOR_Coast_S,
                 cod_ICE, 
                 cod_Kattegat,
                 cod_Baltic_West,
                 cod_Baltic_East,
                 cod_North_Sea,
                 cod_Fareo_Plat,
                 cod_Scotland_West,
                 cod_Rockall,
                 cod_Irish,
                 cod_Eng_Chann_Celtic,
                 .id="stocknum") %>% 
  mutate (Stock = case_when(stocknum == 1 ~ "Greenland, (west, Inshore)", 
                            stocknum == 2 ~ "Greenland, (west, offshore)",
                            stocknum == 3 ~ "Greenland, (east)",
                            stocknum == 4 ~ "Barents Sea (NEA)",
                            stocknum == 5 ~ "Norwegian coastal cod (northern)",
                            stocknum == 6 ~ "Norwegian coastal cod (southern)",
                            stocknum == 7 ~ "Iceland",
                            stocknum == 8 ~ "Kattegat",
                            stocknum == 9 ~ "Baltic (western)",
                            stocknum == 10 ~ "Baltic (eastern)",
                            stocknum == 11 ~ "North Sea",
                            stocknum == 12 ~ "Faroe Plateau",
                            stocknum == 13 ~ "West of Scotland",
                            stocknum == 14 ~ "Rockall",
                            stocknum == 15 ~ "Irish Sea",
                            stocknum == 16 ~ "Celtic Sea")) %>% 
  filter(Year < 2021) %>% 
  mutate(Landings = case_when (is.na(Landings) ~ Catches, TRUE ~ Landings)) %>% 
  mutate(Stock = as.factor(Stock)) %>% 
  mutate(fct_reorder(Stock, Landings, .fun= sum))

cod %>% group_by(Stock) %>% summarise(sum = sum(Landings))

levels(cod$Stock)

pal1 <-
c("#0e92ff", "#289cef", "#3da4e2", "#51acd6", "#63b2cb", 
  "#73b9c1", "#83bfb8", "#92c4ae", "#a1caa5", "#afcf9d", 
  "#bdd594", "#cbda8c", "#d8df84", "#e5e47c", "#f2e974", "#ffee6c")  

cod_g1 <- cod %>% filter(Year < 2022) %>% 
  
ggplot(aes(Year, y =Landings/1000, fill = fct_reorder(Stock, Landings, .fun = sum)))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,2600), breaks=seq(0, 7000, by=200),expand=c(0,0) ) +
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
cod_g1

jpeg("e:/Documents/lumpfish/wikipedia/Atlantic cod/Atlantic cod landings east.jpeg", width = 1400, height = 700)
cod_g1
dev.off()

northern <- read.csv("e:/Documents/lumpfish/wikipedia/Atlantic cod/Northern cod catches.csv", fileEncoding="UTF-8-BOM")
cod_3M <- read.csv("e:/Documents/lumpfish/wikipedia/Atlantic cod/cod catches.csv", fileEncoding="UTF-8-BOM")

cod_west <- bind_rows(northern, cod_3M)

View(
cod_west %>% group_by(Year) %>% summarise(sum = sum(Landings))
)

cod_g2 <- 
  cod_west %>% filter(Year < 2020) %>% mutate(Area = as.factor(Area)) %>% 
  
ggplot(aes(Year, Landings/1000, fill = fct_reorder(Area, Landings, .fun = sum)))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,1000), breaks=seq(0, 1000, by=200),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2020.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Area")+
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
cod_g2



jpeg("e:/Documents/lumpfish/wikipedia/Atlantic cod/Atlantic cod landings west.jpeg", width = 1400, height = 700)
cod_g2
dev.off()

cod_g3 <- 
  cod_west %>% filter(Year < 2020 & Area == "2J3KL") %>% mutate(Area = as.factor(Area)) %>% 
  
  ggplot(aes(Year, Landings/1000))+
  geom_area(fill = "blue")+
  scale_y_continuous (limits = c(0,900), breaks=seq(0, 1000, by=100),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1498,2021), breaks = seq(0,2020, by=50))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Area")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=24),
        legend.title=element_text(size=24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.2, 1.0, 0.2, 0.2), "cm"),
        panel.background = element_blank())
cod_g3

jpeg("e:/Documents/lumpfish/wikipedia/Atlantic cod/Northen cod landings.jpeg", width = 1000, height = 700)
cod_g3
dev.off()


