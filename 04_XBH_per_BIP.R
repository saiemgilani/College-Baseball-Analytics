### XBH/BIP
### Extra Base Hits / Balls in Play
### XBH/BIP attempts to measure how frequent a pitcher gives up hard hits,
### since we do not have Exit Velocity data at the college level (at 
### least publicity), we have to do
### our best to 'measure' how often a pitcher gets hit hard. Generally,
### extra base hits are considered hard hit balls, so we will treat them as such.

#Load in Libraries
library(baseballr)
library(dplyr)
library(extrafont)
extrafont::loadfonts(device = "win", quiet = TRUE)

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel).

#Lookup Schools for teamid
school_id_lu("Florida St.")

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(234,2019,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(Allowed_2b=0,Allowed_3b=0,Allowed_HR=0,P_OAB=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(P_OAB = as.numeric(P_OAB))
#Calculate XBH per BIP
D1 = D1 %>% mutate(XBH_BIP = (Allowed_2b+Allowed_3b+Allowed_HR)/(P_OAB - SO),
                   XBH_BIP =  round(XBH_BIP,3))

#View the metrics
D1_2019 <- D1 %>% 
  mutate(BIP = P_OAB-SO) %>% 
  arrange(desc(XBH_BIP))

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(234,2018,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(Allowed_2b=0,Allowed_3b=0,Allowed_HR=0,P_OAB=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(P_OAB = as.numeric(P_OAB))
#Calculate XBH per BIP
D1 = D1 %>% mutate(XBH_BIP = (Allowed_2b+Allowed_3b+Allowed_HR)/(P_OAB - SO),
                   XBH_BIP =  round(XBH_BIP,3))

#View the metrics
D1_2018 <- D1 %>% 
  mutate(BIP = P_OAB - SO) %>% 
  arrange(desc(XBH_BIP))

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(234,2017,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(Allowed_2b=0,Allowed_3b=0,Allowed_HR=0,P_OAB=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(P_OAB = as.numeric(P_OAB))
#Calculate XBH per BIP
D1 = D1 %>% mutate(XBH_BIP = (Allowed_2b+Allowed_3b+Allowed_HR)/(P_OAB - SO),
                   XBH_BIP =  round(XBH_BIP,3))

#View the metrics
D1_2017 <- D1 %>% 
  mutate(BIP = P_OAB-SO) %>% 
  arrange(desc(XBH_BIP))

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(234,2016,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(Allowed_2b=0,Allowed_3b=0,Allowed_HR=0,P_OAB=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(P_OAB = as.numeric(P_OAB))
#Calculate XBH per BIP
D1 = D1 %>% mutate(XBH_BIP = (Allowed_2b+Allowed_3b+Allowed_HR)/(P_OAB - SO),
                   XBH_BIP =  round(XBH_BIP,3))

#View the metrics
D1_2016 <- D1 %>%
  mutate(BIP = P_OAB-SO) %>% 
  arrange(desc(XBH_BIP))

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(234,2015,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(Allowed_2b=0,Allowed_3b=0,Allowed_HR=0,P_OAB=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(P_OAB = as.numeric(P_OAB))
#Calculate XBH per BIP
D1 = D1 %>% mutate(XBH_BIP = (Allowed_2b+Allowed_3b+Allowed_HR)/(P_OAB - SO),
                   XBH_BIP =  round(XBH_BIP,3))

#View the metrics
D1_2015 <- D1 %>% 
  mutate(BIP = P_OAB-SO) %>% 
  arrange(desc(XBH_BIP))

D1_2015_19 <- bind_rows(D1_2019, D1_2018,D1_2017,D1_2016,D1_2015)
D1_2015_19 <- D1_2015_19 %>%
              mutate_at(vars(year), as.integer)


D1_2015_19_CJ <- D1_2015_19 %>% (Player == "Van Eyk, CJ")
D1_2015_19_CJ

fip_guts = read.csv("fip_constants.csv", stringsAsFactors = F)

#use dplyr's left_join function to get the fip constants by year
pitching = left_join(D1_2015_19, fip_guts, by="year")

#adjust for outs made and divide by 3 of total innings pitched outs to get accurate FIP numbers
pitching = pitching %>% 
  mutate(addl_outs = ((IP %% 1)*10),
         ip_outs = (floor(IP)*3 + addl_outs),
         FIP = ((13*`HR-A`) + (3*(BB + HB)) - (2*SO))/(ip_outs/3) + cFIP
  ) 

#rounding
pitching$FIP = round(pitching$FIP, 2)

m <- png::readPNG("Tomahawk_Nation_Full.png")
img <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.1), nrow=dim(m)[1]) #0.2 is alpha
rast <- grid::rasterGrob(img, interpolate = T)


#sample query with a plot
pitching %>% 
  filter(ip_outs >= 75, GS > 0) %>% 
  select(year, Player, school, Yr, ERA, FIP) %>% 
  arrange(FIP) %>%
  mutate(Diff = ERA - FIP) %>% 
  ggplot(aes(x = ERA, y = FIP, color = Yr)) +
  geom_point() +
  geom_text_repel(aes(label = Player), vjust = -1,  size = 3.3 )+
  scale_color_manual(values =alpha(c("mediumblue","#500f1b","darkred","grey6"),.7))+
  scale_x_continuous(limits = c(3,5.5),breaks=seq(3, 5.5, 0.5)) + 
  scale_y_continuous(limits = c(3,5.5),breaks=seq(3, 5.5, 0.5)) + 
  xlab("ERA") +
  ylab("Fielding Independent Pitching") +
  labs(title = "ERA and Fielding Independent Pitching\nFSU Pitchers 2015-2019 ",
       subtitle = "Figure: @SaiemGilani | Data: @BillPetti's baseballR package") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10, margin=margin(t=0.2,r=0,b=0.2,l=-1.2,unit=c("mm")), family = "Gill Sans MT"),
        legend.background = element_rect(fill = "grey85"),
        legend.key.width = unit(1.5,"mm"),
        legend.key.size = unit(2.0,"mm"),
        legend.position = c(0.84, .2),
        legend.margin=margin(t = 0.4,b = 0.4,l=-1.2,r=0.4,unit=c('mm')),
        legend.direction = "vertical",
        legend.box.background = element_rect(colour = "#500f1b"),
        axis.title.x = element_text(size = 12,margin=margin(0,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.text.x = element_text(size = 10,margin=margin(t=2,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.title.y = element_text(size = 12,margin=margin(0,0,1,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        axis.text.y = element_text(size = 12,margin=margin(0,0.1,0,0,unit=c("mm")), family = "Gill Sans MT", face="bold"),
        plot.title = element_text(size = 12, margin=margin(t=0,r=0,b=1.5,l=0,unit=c("mm")),lineheight=1, family = "Gill Sans MT", face="bold"),
        plot.subtitle = element_text(size = 10, margin=margin(t=0,r=0,b=2,l=0,unit=c("mm")), lineheight=1, family = "Gill Sans MT"),
        plot.caption = element_text(size = 10, margin=margin(t=0,r=0,b=0,l=0,unit=c("mm")),lineheight=-0.5, family = "Gill Sans MT"),
        panel.background = element_rect(fill = "grey75"),
        plot.background = element_rect(fill = "grey65"),
        plot.margin=unit(c(top=0.4,right=0.4,bottom=0.4,left=0.4),"cm"))+
  annotation_custom(rast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  ggsave("2015-_19_FSU_Pitching_FIP_ERA.png", height = 101.6, width = 152.4,units=c('mm'),type="cairo")



write.csv('FSU_2015_19.csv',D1_2015_19, row.names = F)
#League Averages in 2019
#D1 XBH/BIP in 2019 = 10.3%
#D2 XBH/BIP in 2019 = 10.4%
#D3 XBH/BIP in 2019 = 9.1%
