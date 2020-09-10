
library(devtools)
library(Lahman)
library(dplyr)
library(ggplot2)
library(XML)
library(retrosheet)
library(lubridate)
library(tidyverse)
library(reshape2)
library(openWAR)
library(xslt)


data()
batting <- Teams


so <- batting %>% 
  filter(yearID>"2010") %>%
  select(yearID, teamID, G, SO) %>%
  mutate(kpg=SO/G) %>%
  group_by(yearID) %>%
  summarise(kpg=round(mean(kpg),2))

            

# d <- readHTMLTable("https://www.baseball-reference.com/leagues/MLB/2020.shtml")
batting2020 <- readxl::read_xlsx("C:/Users/dtrei/Documents/papers/Random Data Stuff/R Projects/Baseball/Random/2020 Team Batting Data.xlsx")        
batting2020 <- batting2020[-31,]
yearID <- rep(2020, 30)
batting2020 <- cbind(yearID, batting2020)

so2020 <- batting2020 %>%
  select(yearID, Tm, G, SO) %>%
  mutate(kpg=SO/G) %>% 
  group_by(yearID) %>%
  summarise(kpg=round(mean(kpg),2))

so <- rbind(so, so2020)
so[so$teamID=="CHA",2] <- "CHC"
so[so$teamID=="CHN",2] <- "CHW"
so[so$teamID=="KCA",2] <- "KCR"
so[so$teamID=="LAN",2] <- "LAD"
so[so$teamID=="NYA",2] <- "NYM"
so[so$teamID=="NYN",2] <- "NYY"
so[so$teamID=="SDN",2] <- "SDP"
so[so$teamID=="SFN",2] <- "SFG"
so[so$teamID=="SLN",2] <- "STL"
so[so$teamID=="TBA",2] <- "TBR"
so[so$teamID=="WSN",2] <- "WAS"


# strikeouts/game by year
ggplot(so, aes(x=factor(yearID), y=kpg))+ 
  geom_bar(stat = "identity", fill = "#D50032", color = "white") + 
  geom_point(shape = 18, size = 2) +
  geom_text(aes(label=kpg), vjust = -0.65, color = "#002D72", size = 5) + 
  ylab("Strikeouts per Game") +
  ggtitle("Strikeouts per Game by Year") + 
  labs(caption = "2020 Season is July 23rd Through August 25th") +
  ylim(0,10) +
  theme_classic() + 
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11))


game_data2011 <- getRetrosheet("game", 2011)
game_data2012 <- getRetrosheet("game", 2012)
game_data2013 <- getRetrosheet("game", 2013)
game_data2014 <- getRetrosheet("game", 2014)
game_data2015 <- getRetrosheet("game", 2015)
game_data2016 <- getRetrosheet("game", 2016)
game_data2017 <- getRetrosheet("game", 2017)
game_data2018 <- getRetrosheet("game", 2018)
game_data2019 <- getRetrosheet("game", 2019)

game_data <- rbind(game_data2011, 
                   game_data2012,
                   game_data2013,
                   game_data2014,
                   game_data2015,
                   game_data2016,
                   game_data2017,
                   game_data2018,
                   game_data2019)

game_data$year <- year(ymd(game_data$Date))
game_data$month <- month(ymd(game_data$Date))
game_data$day <- day(ymd(game_data$Date))
game_data$week <- week(ymd(game_data$Date))

game_data[game_data$VisTm=="CHA",4] <- "CHC"
game_data[game_data$VisTm=="CHN",4] <- "CHW"
game_data[game_data$VisTm=="KCA",4] <- "KCR"
game_data[game_data$VisTm=="LAN",4] <- "LAD"
game_data[game_data$VisTm=="NYA",4] <- "NYM"
game_data[game_data$VisTm=="NYN",4] <- "NYY"
game_data[game_data$VisTm=="SDN",4] <- "SDP"
game_data[game_data$VisTm=="SFN",4] <- "SFG"
game_data[game_data$VisTm=="SLN",4] <- "STL"
game_data[game_data$VisTm=="TBA",4] <- "TBR"
game_data[game_data$VisTm=="WSN",4] <- "WAS"

game_data[game_data$HmTm=="CHA",7] <- "CHC"
game_data[game_data$HmTm=="CHN",7] <- "CHW"
game_data[game_data$HmTm=="KCA",7] <- "KCR"
game_data[game_data$HmTm=="LAN",7] <- "LAD"
game_data[game_data$HmTm=="NYA",7] <- "NYM"
game_data[game_data$HmTm=="NYN",7] <- "NYY"
game_data[game_data$HmTm=="SDN",7] <- "SDP"
game_data[game_data$HmTm=="SFN",7] <- "SFG"
game_data[game_data$HmTm=="SLN",7] <- "STL"
game_data[game_data$HmTm=="TBA",7] <- "TBR"
game_data[game_data$HmTm=="WSN",7] <- "WAS"

game_data_hm_so <- game_data %>%
  filter(month==4 | month==3) %>%
  mutate(Tm = HmTm) %>%
  select(year, month, Tm, HmK) %>%
  group_by(year,Tm) %>%
  summarise(K=sum(HmK), G = n())

game_data_vis_so <- game_data %>%
  filter(month==4 | month==3) %>%
  mutate(Tm = VisTm) %>%
  select(year, month, Tm, VisK) %>%
  group_by(year,Tm) %>%
  summarise(K=sum(VisK), G = n())

game_data_so <- cbind(game_data_hm_so, game_data_vis_so)
names(game_data_so) <- c("year", "Tm", "K", "G", "year1", "Tm1", "K1", "G1")
game_data_so <- game_data_so %>%
  select(year, Tm, K, G, K1, G1) %>%
  mutate(totalK = K+K1, totalG = G+G1) %>%
  group_by(year, Tm) %>%
  summarise(kpg=totalK/totalG) %>%
  group_by(year) %>%
  summarise(kpg = round(mean(kpg),2))

names(game_data_so) <- c("yearID", "kpg")
early_szn_so <- rbind(game_data_so, so2020)

#strikeouts per game by year in first month of season
ggplot(early_szn_so, aes(x=factor(yearID), y=kpg)) + 
  geom_bar(stat = "identity", fill = "#D50032", color = "white") + 
  geom_point(shape = 18, size = 2) +
  geom_text(aes(label=kpg), vjust = -0.65, color = "#002D72", size = 5) + 
  xlab("Year") +
  ylab("Strikeouts per Game") +
  ggtitle("Strikeouts per Game in March/April") + 
  labs(caption = "2020 Season is July 23rd Through August 25th") +
  ylim(0,10) +
  theme_classic() + 
  theme(plot.title = element_text(size=20),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text = element_text(size=11))
  
game_data_hm_so1 <- game_data %>%
  mutate(Tm = HmTm) %>%
  select(year, month, HmTmGNum, Tm, HmK) %>%
  group_by(year, HmTmGNum, Tm) %>%
  summarise(K=sum(HmK), G = n())

game_data_vis_so1 <- game_data %>%
  mutate(Tm = VisTm) %>%
  select(year, month, VisTmGNum, Tm, VisK) %>%
  group_by(year,VisTmGNum, Tm) %>%
  summarise(K=sum(VisK), G = n())


game_data_so_cumsum <- cbind(game_data_hm_so1, game_data_vis_so1)
names(game_data_so_cumsum) <- c("year", "Game_Num", "Tm", "K", "G", "year1", "Game_Num1", "Tm1", "K1", "G1")
game_data_so_cumsum <- game_data_so_cumsum %>%
  filter(year>=2017) %>%
  select(year, Game_Num, K, G, K1, G1) %>%
  mutate(totalK = K+K1, totalG = G+G1) %>%
  group_by(year,Game_Num) %>%
  summarise(sum_k = sum(totalK)) %>%
  group_by(year) %>%
  summarise(cum_sum_so = cumsum(sum_k))
#game_data_so_cumsum <- game_data_so_cumsum[-487,]
#game_data_so_cumsum <- game_data_so_cumsum[-1297,]
game_data_so_cumsum <- game_data_so_cumsum[-325,]
game_data_so_cumsum$Game_Num <- rep(c(1:162), 3)


ggplot(game_data_so_cumsum, aes(x=Game_Num, y=sum_k, group=factor(year), color=factor(year))) + 
  geom_point() +
  geom_smooth(se=F) +
  xlim(0,162) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  xlab("Game Number") +
  ylab("Total Strikeouts") +
  ggtitle("Total League Strikeouts for Every Game per Season")

## HOMERUNS  
hr <- batting %>% 
  filter(yearID > "2010") %>%
  select(yearID, teamID, G, HR) %>%
  mutate(HR_pg=HR/G) %>%
  group_by(yearID) %>%
  summarise(HR_pg=round(mean(HR_pg),2))

hr2020 <- batting2020 %>%
  select(yearID, Tm, G, HR) %>%
  mutate(HR_pg=HR/G) %>% 
  group_by(yearID) %>%
  summarise(HR_pg=round(mean(HR_pg),2)) 

hr <- rbind(hr, hr2020)

#HR per game by year 
ggplot(hr, aes(x=factor(yearID), y=HR_pg))+ 
  geom_bar(stat = "identity", fill = "#D50032", color = "white") + 
  geom_point(shape = 18, size = 2) +
  geom_text(aes(label=HR_pg), vjust = -0.65, color = "#002D72", size = 5) + 
  xlab("Year") +
  ylab("HR per Game") +
  ggtitle("Homeruns per Game by Year") + 
  labs(caption = "2020 Season is July 23rd Through August 25th") +
  ylim(0,1.5) +
  theme_classic() + 
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11))

game_data_hm_hr <- game_data %>%
  filter(month==4 | month==3) %>%
  mutate(Tm = HmTm) %>%
  select(year, month, Tm, HmHR) %>%
  group_by(year,Tm) %>%
  summarise(HR=sum(HmHR), G = n())

game_data_vis_hr <- game_data %>%
  filter(month==4 | month==3) %>%
  mutate(Tm = VisTm) %>%
  select(year, month, Tm, VisHR) %>%
  group_by(year,Tm) %>%
  summarise(HR=sum(VisHR), G = n())

game_data_hr <- cbind(game_data_hm_hr, game_data_vis_hr)
names(game_data_hr) <- c("year", "Tm", "HR", "G", "year1", "Tm1", "HR1", "G1")
game_data_hr <- game_data_hr %>%
  select(year, Tm, HR, G, HR1, G1) %>%
  mutate(totalHR = HR+HR1, totalG = G+G1) %>%
  group_by(year, Tm) %>%
  summarise(HR_pg=totalHR/totalG) %>%
  group_by(year) %>%
  summarise(HR_pg = round(mean(HR_pg),2))

names(game_data_hr) <- c("yearID", "HR_pg")
early_szn_hr <- rbind(game_data_hr, hr2020)

#hr per game by year in first month of season
ggplot(early_szn_hr, aes(x=factor(yearID), y=HR_pg)) + 
  geom_bar(stat = "identity", fill = "#D50032", color = "white") + 
  geom_point(shape = 18, size = 2) +
  geom_text(aes(label=HR_pg), vjust = -0.65, color = "#002D72", size = 5) + 
  xlab("Year") +
  ylab("Strikeouts per Game") +
  ggtitle("Homeruns per Game in March/April") + 
  labs(caption = "2020 Season is July 23rd Through August 25th") +
  ylim(0,1.5) +
  theme_classic() + 
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11))


library(mice)
batting_Reg <- batting %>%
  filter(yearID > 1900) %>%
  select(yearID, lgID, franchID, SO, W, L, HR, R, AB, H, CS, E, DP)
md.pattern((batting_Reg))
imp <- mice(batting_Reg,method = "pmm")
batting_Reg <- complete(imp)


cor(game_data_hr$HR_pg,game_data_so$kpg)
summary(lm(game_data_so$kpg ~ game_data_hr$HR_pg))
plot(early_szn_so$kpg ~ early_szn_hr$HR_pg)


md.pattern((batting_Reg))
imp <- mice(batting_Reg,method = "pmm")
batting_Reg <- complete(imp)

library(corrplot)
corrplot(cor(batting_Reg[,4:12]))

plot(batting_Reg$SO, batting_Reg$HR)
ggplot(batting_Reg, aes(SO)) + geom_histogram(bins = 100)


ggplot(batting_Reg, aes(factor(yearID), SO, fill = HR)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

batting_Reg <- batting_Reg %>%
  filter(yearID > 1960)

ggplot(batting_Reg, aes(SO)) + geom_histogram(bins = 100)

## Ks per Year with HR
batting_Reg %>%
  group_by(yearID) %>% 
  summarise(SO = sum(SO), HR = sum(HR)) %>%
  ggplot(., aes(factor(yearID), SO, fill = HR)) + geom_bar(stat = "identity", color = "white") +
  xlab("Year") +
  ylab("Strikeouts") +
  ggtitle("Strikeouts per Year Colored by Homeruns per Year") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11))

batting_Reg %>%
  group_by(yearID) %>% 
  summarise(SO = sum(SO), HR = sum(HR)) %>%
  ggplot(., aes(HR, SO)) + geom_point(size = 2) +
  geom_smooth() +
  xlab("HRs") +
  ylab("Ks") +
  ggtitle("Strikeouts vs. Homeruns") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11))

  
so_anova_2020 <- batting2020 %>%
  select(yearID, Tm, G, SO, HR)

so_anova_set <- batting_Reg %>%
  filter(yearID > 2010) %>%
  mutate(G=W+L) %>%
  select(yearID, franchID, G, SO, HR)

names(so_anova_set) <- names(so_anova_2020)
so_anova_Set <- rbind(so_anova_set, so_anova_2020)

so_anova_Set <- so_anova_Set %>%
  group_by(yearID, Tm) %>%
  summarise(kpg=SO/G, HR_pg=HR/G)

library(multcompView)
anova <- aov(lm(kpg~factor(yearID), data = so_anova_Set))  
tukey <- TukeyHSD(anova, conf.level = .95)    
plot(tukey, las=2)    
anova    


summary(lm(kpg~factor(yearID), data = so_anova_Set))
plot(so_anova_Set$HR_pg~so_anova_Set$kpg)


ggplot(so_anova_Set, aes(x=factor(yearID), y=kpg, fill = yearID, alpha= 0.5)) + 
  geom_violin(trim=F, size = .2, width = .8) + 
  coord_flip() +
  theme_minimal() + 
  scale_fill_gradient(low="#002D72", high ="#D50032", aesthetics = "fill") +
  scale_fill_gradient(low="#002D72", high ="#D50032", aesthetics = "colour") +
  ylab("Ks per Game") +
  xlab("") +
  ggtitle("Distribution of Ks per Year") +
  theme_classic() + 
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11)) +
  theme(legend.position = "none") 

  

ggplot(so_anova_Set, aes(x=factor(yearID), y=HR_pg, fill = yearID, alpha= 0.5)) + 
  geom_violin(trim=F, size = .2, width = .8) + 
  coord_flip() +
  theme_minimal() + 
  scale_fill_gradient(low="#002D72", high ="#D50032", aesthetics = "fill") +
  scale_fill_gradient(low="#002D72", high ="#D50032", aesthetics = "colour") +
  ylab("HRs per Game") +
  xlab("") +
  ggtitle("Distribution of HRs per Year") +
  theme_classic() + 
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(size=20),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=11)) +
  theme(legend.position = "none") 


so_anova_Set %>%
  group_by(yearID) %>%
  summarise(SD_k=sd(kpg), SD_hr=sd(HR_pg)) %>%
  ggplot(., aes(x=factor(yearID), y=SD_k)) + geom_point(size = 8, shape = 18, color = "#002D72") +
  theme_minimal() +
  ylim(0,1) + 
  ggtitle("Std. Deviation of K Rate") +
  ylab("Std. Deviation") +
  xlab("") +
  theme(axis.text.x = element_text(vjust = 0.5),
          plot.title = element_text(size=20),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text = element_text(size=11)) +
  theme(legend.position = "none") 

so_anova_Set %>%
    group_by(yearID) %>%
    summarise(SD_k=sd(kpg), SD_hr=sd(HR_pg)) %>%
    ggplot(., aes(x=factor(yearID), y=SD_hr)) + geom_point(size = 8, shape = 18, color = "#002D72") +
    theme_minimal() +
    ylim(0,.5) + 
    ggtitle("Std. Deviation of HR Rate") +
    ylab("Std. Deviation") +
    xlab("") +
    theme(axis.text.x = element_text(vjust = 0.5),
          plot.title = element_text(size=20),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text = element_text(size=11)) +
    theme(legend.position = "none") 


####### Teams

so_by_tm <- batting %>% 
  filter(yearID>"2010") %>%
  select(yearID, teamID, G, SO, HR) %>%
  mutate(kpg=SO/G, HR_pg=HR/G) %>%
  group_by(yearID, teamID) %>%
  summarise(kpg=round(mean(kpg),2),HR_pg=round(mean(HR_pg),2)) 

so2020_by_tm <- batting2020 %>%
  select(yearID, Tm, G, SO, HR) %>%
  mutate(kpg=SO/G, HR_pg=HR/G) %>% 
  group_by(yearID, Tm) %>%
  summarise(kpg=round(mean(kpg),2), HR_pg=round(mean(HR_pg),2))

names(so2020_by_tm) <- names(so_by_tm)
so_by_tm <- rbind(so_by_tm, so2020_by_tm)

so_by_tm[so_by_tm$teamID=="CHA",2] <- "CHC"
so_by_tm[so_by_tm$teamID=="CHN",2] <- "CHW"
so_by_tm[so_by_tm$teamID=="KCA",2] <- "KCR"
so_by_tm[so_by_tm$teamID=="LAN",2] <- "LAD"
so_by_tm[so_by_tm$teamID=="NYA",2] <- "NYM"
so_by_tm[so_by_tm$teamID=="NYN",2] <- "NYY"
so_by_tm[so_by_tm$teamID=="SDN",2] <- "SDP"
so_by_tm[so_by_tm$teamID=="SFN",2] <- "SFG"
so_by_tm[so_by_tm$teamID=="SLN",2] <- "STL"
so_by_tm[so_by_tm$teamID=="TBA",2] <- "TBR"
so_by_tm[so_by_tm$teamID=="WSN",2] <- "WAS"


tm_so_pct_change <- so_by_tm %>%
  filter(yearID >= 2019) %>%
  group_by(teamID) %>%
  arrange(yearID) %>%
  summarise(pct_change_so = 100*((kpg-lag(kpg))/lag(kpg)), pct_change_hr = 100*((HR_pg-lag(HR_pg))/lag(HR_pg)))
tm_so_pct_change <- tm_so_pct_change[!is.na(tm_so_pct_change$pct_change_so),]
tm_so_pct_change

ggplot(tm_so_pct_change, aes(x=reorder(factor(teamID),-pct_change_so), y=pct_change_so, fill=pct_change_so)) + 
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position =  'none') + 
  scale_fill_gradient(low="#002D72", high="#D50032") +
  ylim(-25,25) +
  xlab("") +
  ylab("% change in K rate") +
  ggtitle("Percent Change in K Rate from 2019 to 2020")

ggplot(tm_so_pct_change, aes(x=reorder(factor(teamID),-pct_change_hr), y=pct_change_hr, fill=pct_change_hr)) + 
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position =  'none') + 
  scale_fill_gradient(low="#002D72", high="#D50032") +
  ylim(-60,70) +
  xlab("") +
  ylab("% change in HR rate") +
  ggtitle("Percent Change in HR Rate from 2019 to 2020")

max(tm_so_pct_change$pct_change_so)
min(tm_so_pct_change$pct_change_so)

max(tm_so_pct_change$pct_change_hr)
min(tm_so_pct_change$pct_change_hr)

sorted <- unlist(c(tm_so_pct_change[order(tm_so_pct_change$pct_change_so, decreasing = T),1]))
tm_so_pct_change_sorted <- tm_so_pct_change
tm_so_pct_change_sorted$teamID <- factor(tm_so_pct_change_sorted$teamID, levels = sorted)
names(tm_so_pct_change_sorted) <- c("teamID", "Strikeouts", "Homeruns")

ggplot(melt(tm_so_pct_change_sorted, id.vars = "teamID"),aes(x=teamID,y=value,fill=factor(variable))) + 
  geom_bar(stat = 'identity', position='dodge') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5),
        legend.title = element_blank()) + 
  scale_fill_manual(values = c("#002D72", "#D50032")) +
  ylab("Percent Change from 2019") +
  xlab("") +
  ggtitle("Percent Change in K rate and HR Rate Since 2019")


tm_so_pct_change_2018 <- so_by_tm %>%
  filter(yearID >= 2018 & yearID <= 2019) %>%
  group_by(teamID) %>%
  arrange(yearID) %>%
  summarise(pct_change_so = 100*((kpg-lag(kpg))/lag(kpg)), pct_change_hr = 100*((HR_pg-lag(HR_pg))/lag(HR_pg)))
tm_so_pct_change_2018 <- tm_so_pct_change_2018[!is.na(tm_so_pct_change_2018$pct_change_so),]
tm_so_pct_change_2018

ggplot(tm_so_pct_change_2018, aes(x=reorder(factor(teamID),-pct_change_so), y=pct_change_so, fill=pct_change_so)) + 
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position =  'none') + 
  scale_fill_gradient(low="#002D72", high="#D50032") +
  ylim(-25,25) +
  xlab("") +
  ylab("% change in K rate") +
  ggtitle("Percent Change in K Rate from 2018 to 2019")

ggplot(tm_so_pct_change_2018, aes(x=reorder(factor(teamID),-pct_change_hr), y=pct_change_hr, fill=pct_change_hr)) + 
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position =  'none') + 
  scale_fill_gradient(low="#002D72", high="#D50032") +
  ylim(-60,90) +
  xlab("") +
  ylab("% change in HR rate") +
  ggtitle("Percent Change in HR Rate from 2018 to 2019")

max(tm_so_pct_change_2018$pct_change_so)
min(tm_so_pct_change_2018$pct_change_so)

max(tm_so_pct_change_2018$pct_change_hr)
min(tm_so_pct_change_2018$pct_change_hr)

sorted_2018 <- unlist(c(tm_so_pct_change_2018[order(tm_so_pct_change_2018$pct_change_so, decreasing = T),1]))
tm_so_pct_change_sorted_2018 <- tm_so_pct_change_2018
tm_so_pct_change_sorted_2018$teamID <- factor(tm_so_pct_change_sorted_2018$teamID, levels = sorted_2018)
names(tm_so_pct_change_sorted_2018) <- c("teamID", "Strikeouts", "Homeruns")

ggplot(melt(tm_so_pct_change_sorted_2018, id.vars = "teamID"),aes(x=teamID,y=value,fill=factor(variable))) + 
  geom_bar(stat = 'identity', position='dodge') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5),
        legend.title = element_blank()) + 
  scale_fill_manual(values = c("#002D72", "#D50032")) +
  ylab("Percent Change from 2019") +
  xlab("") +
  ggtitle("Percent Change in K rate and HR Rate Since 2019")
