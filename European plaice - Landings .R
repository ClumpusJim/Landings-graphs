library(icesSAG)
library(tidyverse)

#Get the assesment keys for the year
assessmentKeys <- findAssessmentKey("ple", 2021)
#https://standardgraphs.ices.dk/stockList.aspx

#Download all the data
# cod <- getStockDownloadData(assessmentKeys)
# 
# cod <- data.frame(cod)

#Find out which stock belongs to which key
head(data.frame(getStockDownloadData(14396))) %>% select("AssessmentKey", "StockDescription", "AssessmentYear")


ple_Kattegat <- data.frame(getStockDownloadData(14138))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.))) %>% 
  mutate(across(contains('Type'), ~ as.character(.))) %>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize)) %>% 
  mutate(FAge = as.numeric(FAge))

ple_Baltic <- data.frame(getStockDownloadData(14571))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_North <- data.frame(getStockDownloadData(15614))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_Irish <- data.frame(getStockDownloadData(14392)) %>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.))) %>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_east_channel <- data.frame(getStockDownloadData(14598))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_west_channel <- data.frame(getStockDownloadData(14426))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_Bristol <- data.frame(getStockDownloadData(15646))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

ple_celtic_south <- data.frame(getStockDownloadData(15644))%>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.)))%>% 
  mutate(across(contains('Type'), ~ as.character(.)))%>% 
  mutate(across(contains('Custom'), ~ as.character(.))) %>% 
  mutate(Low_StockSize = as.numeric(Low_StockSize))%>% 
  mutate(FAge = as.numeric(FAge))

stocknames <- c("Kattegat, Belt Seas, and the Sound",
                "Baltic Sea",
                "North Sea and Skagerrak", 
                "Irish Sea",
                "eastern English Channel",
                "western English Channel",
                "Bristol Channel, Celtic Sea",
                "Celtic Sea South, southwest of Ireland")

dfs <- lapply(ls(pattern="^ple"), function(x) get(x))

ple <- bind_rows(dfs, .id="stocknum") %>% 
  mutate (Stock = case_when(StockKeyLabel == "ple.27.24-32" ~ "Baltic Sea", 
                            StockKeyLabel == "ple.27.7fg" ~ "Bristol Channel, Celtic Sea",
                            StockKeyLabel == "ple.27.7h-k" ~ "Celtic Sea South, southwest of Ireland",
                            StockKeyLabel == "ple.27.7d" ~ "eastern English Channel",
                            StockKeyLabel == "ple.27.7a" ~ "Irish Sea",
                            StockKeyLabel == "ple.27.21-23" ~ "Kattegat, Belt Seas, and the Sound",
                            StockKeyLabel == "ple.27.420" ~ "North Sea and Skagerrak",
                            StockKeyLabel == "ple.27.7e" ~ "western English Channel")) %>% 
  filter(Year < 2022) %>% 
  mutate(Landings = case_when (is.na(Landings) ~ Catches, TRUE ~ Landings)) %>% 
  mutate(Stock = as.factor(Stock)) %>% 
  mutate(Stock = fct_reorder(Stock, Landings, .fun= sum, na.rm = TRUE))

#data from https://www.hafogvatn.is/is/veidiradgjof/skarkoli
ple_Iceland <- read.csv("Data/ple_landings_Iceland.csv") %>% 
  select(Year = year, Landings = landings) %>% 
  group_by(Year) %>% 
  summarise(Landings = sum(Landings)) %>% 
  mutate(Stock = "Iceland")

ple <- bind_rows(ple, ple_Iceland) %>% 
  mutate(Stock = fct_reorder(Stock, Landings, .fun= sum, na.rm = TRUE))

#data from MSC https://fisheries.msc.org/en/fisheries/gela-ltd-north-east-atlantic-european-plaice/@@view
ple_Barents <- read.csv("Data/ple_landings_Barents.csv") 

ple <- bind_rows(ple, ple_Iceland, ple_Barents) %>% 
  mutate(Stock = fct_reorder(Stock, Landings, .fun= sum, na.rm = TRUE))


#Total landings for time period
ple %>% group_by(Stock) %>% summarise(sum = sum(Landings, na.rm = TRUE)) 

ple_g1 <- ple %>% filter(Year < 2022) %>% 
  
ggplot(aes(Year, y =Landings/1000, fill = Stock))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,220), breaks=seq(0, 300, by=20),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2021.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Stock")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=24),
        legend.text=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
ple_g1

jpeg("European plaice landings.jpeg", width = 1400, height = 700)
ple_g1
dev.off()


ple_ice_assess <- read.csv("Data/ple_assessment_2021.csv") %>% rename(StockSize = ssb, Year = year) %>% mutate(Stock = "Iceland", StockSize = StockSize*1000)

ple_ssb <- ple %>% filter(StockSizeDescription == "SSB") %>% bind_rows(ple_ice_assess)

ple_ssb %>% filter(Year < 2022) %>% 
ggplot(aes(x = Year, y =StockSize/1000))+
  geom_ribbon(aes(ymin = Low_StockSize/1000, ymax = High_StockSize/1000), fill = "cyan", alpha = 0.4)+
  geom_line(colour="black", width = 1)+
  facet_wrap(vars(Stock), scales = "free", ncol=2)+
  geom_line(aes(y = Blim/1000), linetype = "dashed")+  
  #scale_y_continuous (limits = c(0,1100), breaks=seq(0, 11000, by=100),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2022.2), breaks = seq(0,2020, by=10))+
  labs(x = "Year", y="SSB (thousand tonnes)", fill = "Stock")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=24),
        legend.text=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
