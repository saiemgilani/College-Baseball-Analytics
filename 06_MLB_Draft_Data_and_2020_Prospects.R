# install.packages("installr")
# library(installr)
# updateR()
#load in libraries
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages('tidyverse')
library(extrafont)
extrafont::loadfonts(device = "win", quiet = TRUE)

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)

m <- png::readPNG("Tomahawk_Nation_Full.png")
img <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.1), nrow=dim(m)[1]) #0.2 is alpha
rast <- grid::rasterGrob(img, interpolate = T)

#scrape draft data from 2009 to 2019 quickly using the map function and Bill Petti's baseballr function
draft_list <- 2009:2019 %>% purrr::map(function(x) baseballr::get_draft_mlb(x))


year <- 2018  
api_call <- paste0("http://statsapi.mlb.com/api/v1/draft/", year)

payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

draft_table <- payload$drafts$rounds$picks

draft_table <- draft_table %>%
  dplyr::bind_rows() %>%
  mutate(pickRound = as.character(pickRound),
         headshotLink = as.character(headshotLink,length = 150)
  )
draft_table_filled <- dplyr::bind_rows(column_structure_draft_mlb,
                                       draft_table) %>%
  janitor::clean_names()

draft_table_filled


#convert list to a singular data frame
draft_data <- plyr::ldply(draft_list,data.frame)

#read in csvs from Github
draft_2020 <- read.csv("draft_data.csv", stringsAsFactors = F)

#data acquired from FanGraphs:
#https://www.fangraphs.com/prospects/the-board/2020-mlb-draft/summary?sort=-1,1&type=0
fg_batters <- read.csv("fg_draft_hitters.csv", stringsAsFactors = F)

fg_pitchers <- read.csv("fg_draft_pitchers.csv", stringsAsFactors = F)

#left_join first the draft data and the fangraphs hitter data
draft_2020 <- left_join(draft_2020,fg_batters,by="Name")

#same for the pitching data
draft_2020_final <- left_join(draft_2020,fg_pitchers,by="Name")

#convert column types as well as replace NAs in certain columns with values from other columns
draft_2020_final <- draft_2020_final %>% ####WATCH YOUTUBE VIDEO FOR CODE###  %>% 
  mutate(Athl.x = coalesce(Athl.x,Athl.y),
         Frame.x = coalesce(Frame.x,Frame.y),
         Perf.x = coalesce(Perf.x,Perf.y),
         FG_Rank.x = coalesce(as.integer(FG_Rank.x),FG_Rank.y),
         FG_Pos = coalesce(FG_Pos,Pos),
         Age.x = coalesce(Age.x,Age.y),
         FV.x = coalesce(FV.x,FV.y)) %>%
  #select specific variables and rename variables
  select(Name:FV.x,TJ.Date:Tops) %>% rename(FG_Rank = FG_Rank.x, Age = Age.x,
                                            Athleticism = Athl.x,
                                            Frame = Frame.x,
                                            Performance = Perf.x,
                                            FV = FV.x,
                                            MLB_Rank = Rank)

#Create plot of players by class
ggplot(draft_2020_final,aes(x=reorder(Class,Class,
                                      function(x)-length(x)))) +
  geom_bar(fill="#500F1B") +
  scale_y_continuous(limits = c(0,165),breaks=seq(0, 175, 25)) + 
  xlab("Class") +
  ylab("Number of Players") +
  labs(title = "Top 289 Prospects by Class Type, 2020 MLB Draft ",
       subtitle = "Figure: @SaiemGilani | Data: FanGraphs") +
  geom_text(stat='count', aes(label=..count..,family = "Gill Sans MT", face="bold"), vjust=-1)+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10, margin=margin(t=0.2,r=0,b=0.2,l=-1.2,unit=c("mm")), family = "Gill Sans MT"),
        legend.background = element_rect(fill = "grey85"),
        legend.key.width = unit(1.5,"mm"),
        legend.key.size = unit(2.0,"mm"),
        legend.position = c(0.33, 0.84),
        legend.margin=margin(t = 0.4,b = 0.4,l=-1.2,r=0.4,unit=c('mm')),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "#500f1b"),
        axis.title.x = element_text(size = 14,margin=margin(0,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.text.x = element_text(size = 10,margin=margin(t=2,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.title.y = element_text(size = 14,margin=margin(0,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.text.y = element_text(size = 12,margin=margin(0,0.1,0,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        plot.title = element_text(size = 14, margin=margin(t=0,r=0,b=1.5,l=0,unit=c("mm")),lineheight=-0.5, family = "Gill Sans MT", face="bold"),
        plot.subtitle = element_text(size = 12, margin=margin(t=0,r=0,b=2,l=0,unit=c("mm")), lineheight=-0.5, family = "Gill Sans MT"),
        plot.caption = element_text(size = 12, margin=margin(t=0,r=0,b=0,l=0,unit=c("mm")),lineheight=-0.5, family = "Gill Sans MT"),
        panel.background = element_rect(fill = "grey75"),
        plot.background = element_rect(fill = "grey65"),
        plot.margin=unit(c(top=0.4,right=0.4,bottom=0.4,left=0.4),"cm"))+
  annotation_custom(rast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  ggsave("2020_MLB_Draft_Prospects_by_Class.png", height = 101.6, width = 152.4,units=c('mm'),type="cairo")

