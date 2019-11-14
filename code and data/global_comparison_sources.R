# This is a set of codes that accompanies the article:
# "Comparison of bibliographic data sources: Implications for the robustness of university rankings"
# by Chun-Kai (Karl) Huang, Cameron Neylon, Chloe Brookes-Kenworthy, Richard Hosking, Lucy Montgomery, Katie Wilson, Alkim Ozaygen
# This produces various graphs related to global comparisons across WoS, Scopus and MSA (Section 2 of the above article)
# codes written by Chun-Kai (Karl) Huang
# data accessed via each source's website on 15 Aug 2019

# packages required
#install.packages("rstudioapi")
#install.packages("ggplot2")
#install.packages("dplyr")

##############################
#set path of working directory to same folder as the current file location
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

###############################
# time series plots of number of records in each source from 1970 to 2017
df <- read.csv("total_count_all_sources_20190815.csv",header=T) #read in data
attach(df)
colours <- c("wos_core"="#F8766D","scopus"="#00BA38","msa"="#619CFF","wos_all"="#E6AB02")
library(ggplot2)
ggplot(df, aes(year)) + 
  geom_line(aes(y = scopus, colour = "scopus")) + 
  geom_point(aes(y = scopus, colour = "scopus")) +
  geom_line(aes(y = wos_core, colour = "wos_core")) +
  geom_point(aes(y = wos_core, colour = "wos_core")) +
  geom_line(aes(y = msa, colour = "msa")) +
  geom_point(aes(y = msa, colour = "msa")) +
  geom_line(aes(y = wos_all, colour = "wos_all")) + 
  geom_point(aes(y = wos_all, colour = "wos_all")) +
  scale_x_continuous(breaks = round(seq(min(year), max(year), by = 2),1)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,11000000,by=2000000)) +
  scale_colour_manual(values=colours)+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  labs(colour = NULL)

################################
# pie charts for disciplines within each source

# wos all
# define data
df.wos.all <- data.frame(
  discipline = c("Arts & Humanities","Life Sciences & \nBiomedicine",
                 "Physical Sciences","Technology","Social Sciences"),
  count = c(5557977,41892467,22007714,23542290,14024125))
# calculate required angles for pie chart
library(dplyr)
df.wos.all <- df.wos.all %>%
  arrange(desc(discipline)) %>%
  mutate(lab.ypos = cumsum(count/sum(count)*100) - 0.5*count/sum(count)*100)
# set colours
mycols <- c("#0073C2FF", "#E7298A", "#868686FF", "#EFC000FF", "#1B9E77")
# plot pie chart
ggplot(df.wos.all, aes(x = "", y = count/sum(count)*100, fill = discipline)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = round(count/sum(count)*100,1)), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title="WoS All",fill = "Disciplines")+
  theme(plot.title = element_text(hjust = 0.5,vjust=-6))

# wos core
# define data
df.wos.core <- data.frame(
  discipline = c("Arts & Humanities","Life Sciences & \nBiomedicine",
                 "Physical Sciences","Technology","Social Sciences"),
  count = c(4858298,29968501,14853504,15031867,6328583))
# calculate required angles for pie chart
library(dplyr)
df.wos.core <- df.wos.core %>%
  arrange(desc(discipline)) %>%
  mutate(lab.ypos = cumsum(count/sum(count)*100) - 0.5*count/sum(count)*100)
# set colours
mycols <- c("#0073C2FF", "#E7298A", "#868686FF", "#EFC000FF", "#1B9E77")
# plot pie chart
ggplot(df.wos.core, aes(x = "", y = count/sum(count)*100, fill = discipline)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = round(count/sum(count)*100,1)), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title="WoS Core",fill = "Disciplines")+
  theme(plot.title = element_text(hjust = 0.5,vjust=-6))

# Scopus
# define data
df.scopus <- data.frame(
  discipline = c("Health Sciences","Life Sciences","Physical Sciences",
                 "Social Sciences"),
  count = c(25732912,16468331,32563386,9328191
  ))
# calculate required angles for pie chart
library(dplyr)
df.scopus <- df.scopus %>%
  arrange(desc(discipline)) %>%
  mutate(lab.ypos = cumsum(count/sum(count)*100) - 0.5*count/sum(count)*100)
# set colours
mycols <- c("#D95F02","#CD534CFF", "#868686FF", "#EFC000FF")
# plot pie chart
ggplot(df.scopus, aes(x = "", y = count/sum(count)*100, fill = discipline)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = round(count/sum(count)*100,1)), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title="Scopus",fill = "Disciplines")+
  theme(plot.title = element_text(hjust = 0.5,vjust=-6))

# MSA
# define data
df.msa <- data.frame(
  discipline = c("Health Sciences","Physical Sciences","Life Sciences",
                 "Social Sciences","Arts & Humanities","Other"),
  count = c(24307458,66765218,17684621,25595304,13655212,25772908))
# calculate required angles for pie chart
library(dplyr)
df.msa <- df.msa %>%
  arrange(desc(discipline)) %>%
  mutate(lab.ypos = cumsum(count/sum(count)*100) - 0.5*count/sum(count)*100)
# set colours
mycols <- c("#0073C2FF","#D95F02","#CD534CFF", "#7570B3", "#868686FF", "#EFC000FF")
# plot pie chart
ggplot(df.msa, aes(x = "", y = count/sum(count)*100, fill = discipline)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = round(count/sum(count)*100,1)), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title="MSA",fill = "Disciplines")+
  theme(plot.title = element_text(hjust = 0.5,vjust=-6))

