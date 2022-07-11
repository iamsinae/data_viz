# Europe gas trade flow data visualization by Sinae Lee
# Last updated: July 10/2022 
rm(list=ls())

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(rgdal)
library(spdplyr)
library(zoo)
library(scales)

########## set directories ########## 
dir <- "/Users/sinae/Dropbox/Personal/2019_2022/WB/Junior data scientist June 2022/data_viz/"
inputs <- paste0(dir, "inputs/")
charts <- paste0(dir, "charts/")

########## Load inputs ########## 
# location file with country code
load(paste0(inputs,"location.Rdata")) # location file with country code
# data file with EU energy flow 
energy.dt <- read_excel(paste0(inputs, "European-Gas-Trade-Flows.xls"), range= "A1:EE196", col_types = c(rep("text",3) ,rep("numeric", 132)))
# world map shape files
wld.shp <- readOGR(paste0(inputs, "ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")) %>%
  filter(ADMIN != "Antarctica") 
# centroids file 
centroids <- read.csv(paste0(inputs,"country_centroids_az8.csv"), stringsAsFactors = F)

##########  prepare/reformat data ##########   
# format column names (in date format)
names(energy.dt)[5:ncol(energy.dt)] <- format(as.Date(as.numeric(names(energy.dt)[5:ncol(energy.dt)]), origin="1900-01-01"), "%b-%y") 

# attach unique country code to exit and entry countries 
energy <- energy.dt %>%
  left_join(select(location, area, exit.iso3 = iso3), by=c("Exit" = "area")) %>% 
  left_join(select(location, area, entry.iso3 = iso3), by=c("Entry" = "area")) %>% 
  select(Borderpoint, Exit, exit.iso3, Entry, entry.iso3, everything())

unique(energy$Exit[is.na(energy$exit.iso3)])
unique(energy$Entry[is.na(energy$entry.iso3)])

location$iso3[grepl("Kingdom", location$area)]
location$iso3[grepl("Slova", location$area)]
location$iso3[grepl("Cze", location$area)]
location$iso3[grepl("Iran", location$area)]
location$iso3[grepl("Russia", location$area)]
location$iso3[grepl("Macedonia", location$area)]
location$iso3[grepl("Macedonia", location$area)]
location$iso3[grepl("Moldova", location$area)]

dt.rev <- energy %>%
  mutate(exit.iso3 = ifelse(grepl("Kingdom", Exit), "GBR",
                            ifelse(grepl("Slovak Republic", Exit), "SVK",
                                         ifelse(grepl("Cze", Exit), "CZE",
                                                ifelse(grepl("Iran", Exit), "IRN",
                                                       ifelse(grepl("Russia",Exit), "RUS",
                                                              ifelse(grepl("Macedonia", Exit), "MKD",
                                                                     ifelse(grepl("Moldova", Exit), "MDA",
                                                                            ifelse(Exit=="Liquefied Natural Gas", "LNG", exit.iso3))))))))) %>%
  mutate(entry.iso3 = ifelse(grepl("Kingdom", Entry), "GBR",
                            ifelse(grepl("Slovak Republic", Entry), "SVK",
                                   ifelse(grepl("Cze", Entry), "CZE",
                                          ifelse(grepl("Iran", Entry), "IRN",
                                                 ifelse(grepl("Russia",Entry), "RUS",
                                                        ifelse(grepl("Macedonia", Entry), "MKD",
                                                               ifelse(grepl("Moldova", Entry), "MDA",
                                                                      ifelse(Entry=="Liquefied Natural Gas", "LNG", entry.iso3)))))))))

######### Figures #########
# Fig 1. Gas trade flows by country map 
wld.map <- fortify(wld.shp, region="ADM0_A3") %>%
  left_join(select(location, iso3, region), by=c("id" = "iso3")) 

# sum routes  
top.route <- dt.rev %>%
  mutate(route = paste0(exit.iso3, "_", entry.iso3)) %>%
  select(c(1:5,138, 113:137)) 

top.route$total<-rowSums(top.route[,7:length(top.route)], na.rm=T) 

top.route.sum <- top.route %>%
  select(6:32) %>%
  group_by(route) %>%
  summarize_all(funs(sum(.,na.rm=T))) %>%
  arrange(desc(total)) %>%
  separate(col=route, into = c("exit", "entry"), sep="_") %>%
  left_join(select(centroids, iso_a3, x=Longitude, y=Latitude), by=c("exit" = "iso_a3")) %>% # attach country coordinate for origin country
  left_join(select(centroids, iso_a3, xend=Longitude, yend=Latitude), by=c("entry" = "iso_a3")) %>% # attach country coordinate for destination country
  mutate(col = ifelse(total >100000, ">100,000", 
                      ifelse(total<=100000 & total >50000, "50,000 to 100,000",
                             ifelse(total<=50000 & total > 25000, "25,000 to 50,000",
                                    ifelse(total<=25000 & total >10000, "10,000 to 25,000",
                                           ifelse(total<=10000 & total>5000, "5,000 to 10,000",
                                                  ifelse(total<=5000 & total >1000, "1,000 to 5,000",
                                                         "<=1,000")))))),
         col=factor(col, levels = c("<=1,000","1,000 to 5,000","5,000 to 10,000","10,000 to 25,000","25,000 to 50,000","50,000 to 100,000",">100,000")))

node <- top.route.sum[ ,c("exit", "x", "y")]  %>%
  filter(!is.na(x)) %>%
  unique() %>%
  left_join(select(location, area, iso3), by=c("exit"="iso3")) %>%
  mutate(area=ifelse(exit=="IRN", "Iran",
                     ifelse(exit=="GBR", "United Kingdom",
                            ifelse(exit=="RUS", "Russia",
                                   area))))

# draw map
cols1 <- c("#D0BBB0","#FDCA7D","#7ECAB6","#29AAE1","#8173B3","#F05F6D","#283A57") 

ggplot() + 
  geom_polygon(data = wld.map, aes(x = long, y = lat, group = group), fill= "#F1F2F2", alpha = .6, color="#B4B5B5", size=0.1) + # Draw backgroupd choropleth map 
  coord_fixed(xlim = c(-10, 100), ylim = c(25, 75)) +
  geom_curve(data = top.route.sum, aes(x = x, y = y, xend = xend, yend = yend,   # Draw flow map   
                                             size = total/1000, colour=col),  curvature = 0.25, alpha = 0.7, show.legend = T, arrow = arrow(length = unit(0.15,"cm"))) +
  scale_color_manual(values=cols1, name="Gas flow (in million cubic metres)")+
  scale_size_continuous(guide = FALSE, range = c(0.1, 2.5)) + # Set width range of flow arrow
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title=element_text(size=9),
        legend.key=element_blank(),
        panel.background = element_blank()
       ) +
    geom_text(data=node, aes(x = x, y = y, label = str_wrap(paste0(area),width=14)),  
              vjust = 1.5, nudge_x = 0, nudge_y = 0, lineheight = 1, size = 2, color = "black")  
# ggsave(paste0(charts, "fig1_flow_map.pdf"), width=15,height=6.5)
  
# Fig 2: Gas flow trend over time with top 5 routes
top10.route <- top.route.sum[1:5,] %>%
  left_join(select(location, iso3, area.exit=area), by=c("exit"="iso3")) %>%
  left_join(select(location, iso3, area.entry=area), by=c("entry"="iso3")) %>%
  mutate(area.exit= ifelse(area.exit=="Russian Federation", "Russia", area.exit),
         area.entry=ifelse(area.entry=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom", area.entry)) %>%
  mutate(route=paste0(area.exit, "_", area.entry)) %>%
  select(c(36, 3:27)) %>%
  gather(key=time, value=oil, `Aug-17`:`Aug-19`) %>%
  mutate(time2=as.Date(as.yearmon(time, "%b-%y"))) %>%
  mutate(route=factor(route, levels=c("Russia_Germany","Ukraine_Slovakia","Slovakia_Austria","Germany_Czechia","Belarus_Poland")))
  
# grouped bar 
cols2<- c("#A1B2C6","#283A57","#ECD1C2","#E58B7D","#F05F6D")

ggplot(top10.route) + 
  geom_bar(aes(x=time2, y = oil, fill = as.factor(route)),
           position="dodge", stat="identity")  +
  theme_classic() +
  scale_fill_manual(values=cols2)+
  scale_y_continuous(breaks=seq(0, 6000, 1000), limits=c(0,6000), expand = c(0,0)) +
  scale_x_date(labels = date_format("%b-%y"), breaks='1 month') +
  labs(y="Gas flow (in million cubic metres)") +
    theme(legend.position="bottom",
          text = element_text(size=10),
          axis.text.x = element_text(size=8),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=8),
          axis.line.y = element_blank(),
          legend.key.size = unit(0.5, "line"),
          legend.text = element_text(size=7),
          legend.title=element_blank(),
          panel.grid.major.y  =element_line(colour="grey", size=0.25),
          axis.ticks = element_blank(),
          axis.line = element_line(size=0.5, colour="grey"))
# ggsave(paste0(charts, "fig2_top5route_bar.pdf"), width=15,height=6.5)

# Fig 3: Bubble plot of top exit/entry country (total gas)
dt.rev$Exit <- with(dt.rev, ifelse(Exit=="Slovak Republic", "Slovakia", Exit))
dt.rev$Entry <- with(dt.rev, ifelse(Entry=="Slovak Republic", "Slovakia", Entry))

# top exit country 2017-2019 
top.exit <- dt.rev[ ,c(2, 113:137)]
top.exit$total<-rowSums(top.exit[,2:length(top.exit)], na.rm=T) 

top.exit.sum <- top.exit %>%
  group_by(Exit) %>%
  summarize_all(funs(sum(.,na.rm=T))) %>%
  arrange(desc(total)) %>%
  filter(Exit != "Liquefied Natural Gas") %>%
  mutate(var="exit")

#  top entry country Aug 2017 - Aug 2019 
top.entry <- dt.rev[ ,c(4, 113:137)]
top.entry$total<-rowSums(top.entry[,2:length(top.entry)], na.rm=T)  

top.entry.sum <- top.entry %>%
  group_by(Entry) %>%
  summarize_all(funs(sum(.,na.rm=T))) %>%
  arrange(desc(total)) %>%
  mutate(var="entry") 

top.exit.entry <- top.exit.sum[1:5,] %>%
  rename(loc=Exit) %>%
  bind_rows(top.entry.sum[1:5,] %>% rename(loc=Entry)) %>%
  mutate(col=c("#F05F6D","#7ECAB6","#FDCA7D","#8173B3","#29AAE1","#FDCA7D","#D0BBB0","#29AAE1","#DA967E","#283A57"),
         loc=ifelse(loc=="Slovak Republic","Slovakia",loc))

# function for bubble plot 
fun.bubble <- function(var0){
  dt.sel <- filter(filter(top.exit.entry, var==var0)) %>%
    arrange(desc(total)) 
  order <-dt.sel$loc
  
  if (var0 == "exit") {  
  p.bubble<- ggplot(data=dt.sel, aes(x=reorder(factor(loc), `Aug-19`, first), y=var)) +
    geom_point(aes(size= total/1000, color= col), alpha=0.9) + 
    labs(y=paste0("Total gas exited,","\n","08/2017 -08/2019"))
  } else {
    p.bubble<-ggplot(data=dt.sel, aes(x=reorder(factor(loc), `Aug-19`, first), y=var)) +
     geom_point(aes(size= total/1000, colour= col), shape=21, alpha=0.9) + 
      labs(y=paste0("Total gas entered,","\n","08/2017 -08/2019"))
  }
  p.bubble <-p.bubble + 
    scale_color_identity()+ 
    scale_size_area(max_size=50) +
    coord_flip() +
    geom_text(data=dt.sel, aes(label= paste0(round(total/1000,0))), fontface="bold", color="black", hjust=0.5, vjust=0.5, size=4) +
    theme_classic()+
    theme(legend.position="none",
          text = element_text(size=10),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=9),
          axis.line = element_blank())
  ggsave(plot=p.bubble, filename=paste0(charts,"fig3_bubble_top_",var0,".pdf"),width=3,height=8)
  }

fun.bubble(var0="exit")
fun.bubble(var0="entry")

# Fig 4: Top 5 exit country rank chart 
top.exit.sum<-rename(top.exit.sum, loc=Exit)
top.entry.sum<-rename(top.entry.sum, loc=Entry)

# function to shape data frame and draw rank plot
fun.rank<- function(dt0,var0){
  dt.sel <-dt0[1:5, c(1:2,6,10,14,18,22,26)] %>%
    gather(key=date, value=oil, `Aug-17`:`Aug-19`) %>%
    mutate(date2=as.Date(as.yearmon(date, "%b-%y"))) %>%
    group_by(date2) %>%
    arrange(date2, desc(oil), loc) %>%
    mutate(ranking = row_number()) %>%
    as.data.frame() %>%
    mutate(col=ifelse(loc=="Norway","#F05F6D", ifelse(loc=="Russia","#8173B3", ifelse(loc=="Ukraine","#7ECAB6",
                      ifelse(loc=="Germany","#FDCA7D",ifelse(loc=="Slovakia","#29AAE1",ifelse(loc=="Italy","#D0BBB0", 
                      ifelse(loc=="France","#DA967E", ifelse(loc=="Turkey","#283A57", NA)))))))))
  
  ctr.order <- unique(dt.sel$loc[dt.sel$date=="Aug-17"]) 
  
  if (var0=="exit"){
    p.rank <- ggplot(data = dt.sel, aes(x = date2, y = ranking, group = loc)) +
      geom_line(aes(color = col), size = 2) +
      geom_point(aes(color = col), size = 12) 
  } else {
    p.rank <- ggplot(data = dt.sel, aes(x = date2, y = ranking, group = loc)) +
      geom_line(aes(color = col), size = 2) +
      geom_point(aes(color = col), shape=21, fill="white", size = 12) 
  }
  p.rank <- p.rank + 
    scale_color_identity()+ 
    scale_y_reverse(breaks = 1:nrow(dt.sel), labels=dt.sel$loc) +
    scale_x_date(breaks='2 month',labels = date_format("%b-%y")) +
    geom_text(data=dt.sel, aes(label= paste0(round(oil/1000,0))), fontface="bold", color="black", hjust=0.5, vjust=0.5, size=4) +
    theme_classic() +
    theme(legend.position="right",
          text = element_text(size=10),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.line = element_blank())
  ggsave(plot=p.rank, filename=paste0(charts,"fig4_rank_top_",var0,".pdf"),width=10,height=4.5)
}

fun.rank(dt0=top.exit.sum, var0="exit")
fun.rank(dt0=top.entry.sum, var0="entry")

