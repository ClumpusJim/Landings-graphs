library(icesSAG)
library(tidyverse)
library(patchwork)

#Get the assesment keys for the year
assessmentKeys <- findAssessmentKey("ghl", 2021)
#https://standardgraphs.ices.dk/stockList.aspx

ghl_GRE_nea <- data.frame(getStockDownloadData(14599)) %>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.))) %>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

ghl_GRE_westnordic <- data.frame(getStockDownloadData(15625)) %>% 
  mutate(ICES_Areas = as.character(ICES_Areas)) %>% 
  mutate(across(contains('Unit'), ~ as.character(.))) %>% 
  select("AssessmentKey", "AssessmentYear", "StockKeyLabel","StockDescription", "Year", "Landings", "OfficialLandings", "Catches")

stocknames <- c("North-East Arctic",
                "West Nordic")

ghl <- bind_rows(ghl_GRE_nea, 
                 ghl_GRE_westnordic,
                 .id="stocknum") %>% 
  mutate (Stock = case_when(stocknum == 1 ~ "North-East Arctic", 
                            stocknum == 2 ~ "West Nordic")) %>% 
  filter(Year < 2022) %>% 
  mutate(Landings = case_when (is.na(Landings) ~ Catches, TRUE ~ Landings)) %>% 
  mutate(Stock = as.factor(Stock)) %>% 
  mutate(fct_reorder(Stock, Landings, .fun= sum))

#Total landings for time period
ghl %>% group_by(Stock) %>% summarise(sum = sum(Landings))

ghl_g1 <- ghl %>% filter(Year < 2022) %>% 
  
  ggplot(aes(Year, y =Landings/1000, fill = fct_reorder(Stock, Landings, .fun = sum)))+
  geom_bar(stat="identity", position="stack", colour="black", width = 1)+
  scale_y_continuous (limits = c(0,140), breaks=seq(0, 200, by=20),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2021.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Stock")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=26),
        legend.text=element_text(size=26),
        legend.position = c(0.8, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
ghl_g1



ghl_west <- read.csv("Data/ghl_NAFO.csv") %>% 
  rename(Landings = Metric.Tonnes) 

ghl_g2 <- ghl_west %>% filter(Year < 2022) %>% 
  group_by(Year) %>%
  summarise(Landings = sum(Landings)) %>% 
  
  ggplot(aes(Year, y =Landings/1000))+
  geom_bar(stat="identity", position="stack", colour="black", fill = "grey60", width = 1)+
  scale_y_continuous (limits = c(0,100), breaks=seq(0, 200, by=20),expand=c(0,0) ) +
  scale_x_continuous (expand=c(0,0), limits = c(1959.3,2021.2), breaks = seq(0,2020, by=5))+
  labs(x = "Year", y="Landings (thousand tonnes)", fill = "Stock")+
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option="inferno")+
  theme(axis.line = element_line(size = 0.7, colour = "black"),
        axis.ticks = element_line(size = 1),
        axis.text = element_text (size = 18, colour = "black"),
        axis.title = element_text (size = 26, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=26),
        legend.text=element_text(size=26),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
ghl_g2


jpeg("Greenland halibut.jpeg", width = 900, height = 900)
ghl_g1 + ghl_g2 + plot_layout(ncol = 1)
dev.off()

tmp <- 
bind_rows(
  (ghl %>% group_by(Year) %>% summarise(Landings = sum(Landings))),
  (ghl_west %>% group_by(Year) %>% summarise(Landings = sum(Landings)))
) %>% group_by(Year) %>% summarise(Landings = sum(Landings)) %>% 
  filter(Year > 1959)
  
