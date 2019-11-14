# This is a set of codes that accompanies the article:
# "Comparison of bibliographic data sources: Implications for the robustness of university rankings"
# by Chun-Kai (Karl) Huang, Cameron Neylon, Chloe Brookes-Kenworthy, Richard Hosking, Lucy Montgomery, Katie Wilson, Alkim Ozaygen
# This set of codes analyses citation counts and open access status for the primary sample of 15 universities
# and for the set of 155 universities
# and produce various graphs to show the results
# codes written by Chun-Kai (Karl) Huang

# packages required
# install.packages("rstudioapi")
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("directlabels")

###############################################
# set path of working directory to same folder as the current file location
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# load data file
# this data is only for 2016, as per publication date of data source
load("analysis_on_oa_and_citations.RData")

###############################################
# define sets of grid ids as needed
# get all grid ids in the data dump
grid.ids <- levels(factor(c(data.bigquery.wos$grid_id,data.bigquery.scopus$grid_id,data.bigquery.msa$grid_id)))
# grid ids for the 15 universities for cross-validation
grids_of_interest <- c(
  "grid.7776.1",
  "grid.1032.0",
  "grid.30055.33",
  "grid.34980.36",
  "grid.434933.a",
  "grid.6571.5",
  "grid.116068.8",
  "grid.14476.30",
  "grid.9486.3",
  "grid.83440.3b",
  "grid.7836.a",
  "grid.8664.c",
  "grid.11899.38",
  "grid.26999.3d",
  "grid.254444.7")
#grid ids for those institutions with known problems, 
#e.g., missing or incorrect source ids/search terms, mixed up campuses
grid.ids.problems <- c("grid.462044.0",
                       "grid.449625.8",
                       "grid.431470.5",
                       "grid.431331.7",
                       "grid.13063.37",
                       "grid.16488.33",
                       "grid.1038.a",
                       "grid.29980.3a",
                       "grid.448660.8",
                       "grid.5801.c")

###############################################
# calculate OA levels for each institution and place results in two tables (15 universities and 155 universities)
# calculate oa levels as per source, for all grid ids in data
data.oa <- matrix(,nrow=0,ncol=4)
colnames(data.oa) <- c("grid_id","oa_wos","oa_scopus","oa_msa")
for (g in 1:length(grid.ids)) {
  grid.count <- grid.ids[g]
  Temp = data.bigquery.wos[data.bigquery.wos$grid_id==grid.count,]
  oa_wos = length(unique(Temp[Temp$is_oa==TRUE,]$f0_))/length(Temp$f0_)*100
  Temp = data.bigquery.scopus[data.bigquery.scopus$grid_id==grid.count,]
  oa_scopus = length(unique(Temp[Temp$is_oa==TRUE,]$f0_))/length(Temp$f0_)*100
  Temp = data.bigquery.msa[data.bigquery.msa$grid_id==grid.count,]
  oa_msa = length(unique(Temp[Temp$is_oa==TRUE,]$f0_))/length(Temp$f0_)*100
  data.oa <- rbind(data.oa,
                   cbind(grid_id=grid.count,oa_wos,oa_scopus,oa_msa))
}
# filter down to (1) 15 unis only, (2) 155 unis -removing problem ones
data.oa.15 <- data.oa[data.oa[,1]%in%grids_of_interest,]
data.oa.155 <- data.oa[!(data.oa[,1]%in%grid.ids.problems),]
# calculate rank and add short names for the set of 15 unis
data.oa.15 <- cbind(data.oa.15,wos_rank=rank(-as.numeric(data.oa.15[,2])),
                    scopus_rank=rank(-as.numeric(data.oa.15[,3])),
                    msa_rank=rank(-as.numeric(data.oa.15[,4])),
                    name=c("Curtin","MIT","USP","MSU","WSU","Tokyo","DUT","IISC",
                           "ITB","LU","Cairo","UCT","UCL","Giessen","UNAM"))

###############################################
# output the table of 155 to .csv file for further analysis
# write.csv(data.oa[!(data.oa[,1]%in%grid.ids.problems),], file = "oa.155.csv", row.names=FALSE) #155 unis after removing problematic ones
# not run here
# resulting final .csv files is named "oa.155.analysis.csv" which is used below

###############################################
# plotting the 15 universities by OA rank and OA %, as per source (WoS, Scopus and MSA)
library(reshape2)
library(ggplot2)
library(directlabels)
#plot oa ranks by source and uni
data.oa.15.melt <- melt(as.data.frame(data.oa.15[,5:8]),id='name')
data.oa.15.melt$name <- as.factor(data.oa.15.melt$name)
data.oa.15.melt$variable <- as.factor(data.oa.15.melt$variable)
data.oa.15.melt$value <- as.numeric(as.character(data.oa.15.melt$value))
ggplot(data.oa.15.melt, aes(x = variable, y = value, group=name, colour = name)) + 
  geom_line(size=1) +
  geom_point(size=2) +
  scale_y_continuous(trans = "reverse", breaks = unique(data.oa.15.melt$value)) +
  scale_x_discrete(limits=c("scopus_rank","wos_rank","msa_rank")) +
  scale_colour_discrete(guide = 'none') +
  theme(panel.grid.minor = element_blank())+
  xlab(NULL)+
  ylab("rank")+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + 0.5), "last.points", cex = 0.8)) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - 0.5), "first.points", cex = 0.8)) +
  labs(title="OA ranks") +
  theme(plot.title = element_text(hjust = 0.5))
#plot oa %s by source and uni
data.oa.15.melt <- melt(as.data.frame(data.oa.15[,c(2:4,8)]),id='name')
data.oa.15.melt$name <- as.factor(data.oa.15.melt$name)
data.oa.15.melt$variable <- as.factor(data.oa.15.melt$variable)
data.oa.15.melt$value <- as.numeric(as.character(data.oa.15.melt$value))
ggplot(data.oa.15.melt, aes(x = variable, y = value, group=name, colour = name)) + 
  geom_line(size=1) +
  geom_point(size=2) +
  scale_x_discrete(limits=c("oa_scopus","oa_wos","oa_msa")) +
  scale_colour_discrete(guide = 'none') +
  theme(panel.grid.minor = element_blank())+
  xlab(NULL)+
  ylab("OA%")+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + 0.5), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - 0.5), "first.bumpup", cex = 0.8)) +
  labs(title="OA percentages") +
  theme(plot.title = element_text(hjust = 0.5))

###############################################
# construct various plots for the set of 155 universities
# read in data after some analysis in Excel
data.oa.processed <- read.csv("oa.155.analysis.csv")
#now plot 155 uni ranks as per source, using ggplot
data.oa.155.melt <- melt(as.data.frame(data.oa.processed[,c("grid_id","oa_wos_rank",
                         "oa_scopus_rank","oa_msa_rank","colour")]),id=c('grid_id','colour'))
ggplot() + 
  geom_line(data=data.oa.155.melt[data.oa.155.melt$colour=='grey',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=0.8) +
  geom_point(data=data.oa.155.melt[data.oa.155.melt$colour=='grey',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=1.2) +
  geom_line(data=data.oa.155.melt[data.oa.155.melt$colour=='red',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.oa.155.melt[data.oa.155.melt$colour=='red',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  geom_line(data=data.oa.155.melt[data.oa.155.melt$colour=='orange',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.oa.155.melt[data.oa.155.melt$colour=='orange',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  scale_y_continuous(trans = "reverse", breaks = c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)) +
  scale_x_discrete(limits=c("oa_scopus_rank","oa_wos_rank","oa_msa_rank")) +
  scale_colour_manual('Legend',labels=c('shift<20','shift>=20 & non-Eng','shift>=20 & Eng'),
                      values=c('grey','orange','#F8766D')) +
  labs(title="OA ranks")+
  xlab(NULL)+
  ylab('rank')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=20))
#now plot boxplots of changes in OA% and changes in OA rank when shifting between sources
ggplot(melt(data.oa.processed[,c('wos.scopus','wos.msa','scopus.msa')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in OA%") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Change to OA%")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))
ggplot(melt(data.oa.processed[,c('wos.scopus.rank','wos.msa.rank','scopus.msa.rank')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in rank ") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Shift in OA rank")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))

###############################################
# analysis on citations for 2016 using opencitations
# calculate citation counts and average citations for each university, as per source
data.citation <- matrix(,nrow=0,ncol=7)
colnames(data.citation) <- c("grid_id","cit_wos","cit_scopus","cit_msa","cit_ave_wos","cit_ave_scopus","cit_ave_msa")
for (g in 1:length(grid.ids)) {
  grid.count <- grid.ids[g]
  #filter all WoS data to grid and add citations for these DOIs from opencitations
  Temp = unique(data.bigquery.wos[data.bigquery.wos$grid_id==grid.count,]$f0_)
  cit_wos = sum(data.opencitations[data.opencitations$doi%in%Temp,]$citations_total)
  cit_ave_wos = cit_wos/length(Temp)
  #filter all Scopus data to grid and add citations for these DOIs from opencitations
  Temp = unique(data.bigquery.scopus[data.bigquery.scopus$grid_id==grid.count,]$f0_)
  cit_scopus = sum(data.opencitations[data.opencitations$doi%in%Temp,]$citations_total)
  cit_ave_scopus = cit_scopus/length(Temp)
  #filter all MSA data to grid and add citations for these DOIs from opencitations
  Temp = unique(data.bigquery.msa[data.bigquery.msa$grid_id==grid.count,]$f0_)
  cit_msa = sum(data.opencitations[data.opencitations$doi%in%Temp,]$citations_total)
  cit_ave_msa = cit_msa/length(Temp)
  #add result for grid to table
  data.citation <- rbind(data.citation,
                   cbind(grid_id=grid.count,cit_wos,cit_scopus,cit_msa,cit_ave_wos,cit_ave_scopus,cit_ave_msa))
}  
# create tables (one for the 15 and another for the 155) of citation counts as per source, also removing problem cases grid ids
data.citation.15 <- data.citation[data.citation[,1]%in%grids_of_interest,]
data.citation.155 <- data.citation[!(data.citation[,1]%in%grid.ids.problems),]
# calculate rank and add short names for the set of 15 unis
data.citation.15 <- cbind(data.citation.15,wos_rank=rank(-as.numeric(data.citation.15[,5])),
                    scopus_rank=rank(-as.numeric(data.citation.15[,6])),
                    msa_rank=rank(-as.numeric(data.citation.15[,7])),
                    name=c("Curtin","MIT","USP","MSU","WSU","Tokyo","DUT","IISC",
                           "ITB","LU","Cairo","UCT","UCL","Giessen","UNAM"))

###############################################
# plots of 15 universities in terms of total citation counts
# and ranks in average citation count, as per source
library(reshape2)
library(ggplot2)
library(directlabels)
#plot ranks in ave citation by source and uni
data.citation.15.melt <- melt(as.data.frame(data.citation.15[,8:11]),id='name')
data.citation.15.melt$name <- as.factor(data.citation.15.melt$name)
data.citation.15.melt$variable <- as.factor(data.citation.15.melt$variable)
data.citation.15.melt$value <- as.numeric(as.character(data.citation.15.melt$value))
ggplot(data.citation.15.melt, aes(x = variable, y = value, group=name, colour = name)) + 
  geom_line(size=1) +
  geom_point(size=2) +
  scale_y_continuous(trans = "reverse", breaks = unique(data.oa.15.melt$value)) +
  scale_x_discrete(limits=c("scopus_rank","wos_rank","msa_rank")) +
  scale_colour_discrete(guide = 'none') +
  theme(panel.grid.minor = element_blank())+
  xlab(NULL)+
  ylab("rank")+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + 0.5), "last.points", cex = 0.8)) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - 0.5), "first.points", cex = 0.8)) +
  labs(title="Rank by average citation") +
  theme(plot.title = element_text(hjust = 0.5))
#plot total citations by source and uni
data.citation.15.melt <- melt(as.data.frame(data.citation.15[,c(2:4,11)]),id='name')
data.citation.15.melt$name <- as.factor(data.citation.15.melt$name)
data.citation.15.melt$variable <- as.factor(data.citation.15.melt$variable)
data.citation.15.melt$value <- as.numeric(as.character(data.citation.15.melt$value))
ggplot(data.citation.15.melt, aes(x = variable, y = value, group=name, colour = name)) + 
  geom_line(size=1) +
  geom_point(size=2) +
  scale_x_discrete(limits=c("cit_scopus","cit_wos","cit_msa")) +
  scale_colour_discrete(guide = 'none') +
  theme(panel.grid.minor = element_blank())+
  xlab(NULL)+
  ylab("citation count")+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + 0.5), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - 0.5), "first.bumpup", cex = 0.8)) +
  labs(title="Total citations") +
  theme(plot.title = element_text(hjust = 0.5))

###############################################
# output the table of 155 to .csv file for further analysis
# write.csv(data.citation[!(data.citation[,1]%in%grid.ids.problems),], file = "citation.155.csv", row.names=FALSE) #155 unis after removing problematic ones
# not run here
# resulting final .csv files is named "citation.155.analysis.csv" which is used below

###############################################
# construct various plots for the set of 155 universities
# read in data after some analysis in Excel
data.citation.processed <- read.csv("citation.155.analysis.csv")
# Plot citation ranks of 155 universities using ggplot
data.citation.155.melt <- melt(as.data.frame(data.citation.processed[,c("grid_id",
                               "cit_wos_rank","cit_scopus_rank","cit_msa_rank","colour")]),
                               id=c('grid_id','colour'))
ggplot() + 
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='grey',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=0.8) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='grey',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=1.2) +
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='red',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='red',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='orange',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='orange',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  scale_y_continuous(trans = "reverse", breaks = c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)) +
  scale_x_discrete(limits=c("cit_scopus_rank","cit_wos_rank","cit_msa_rank")) +
  scale_colour_manual('Legend',labels=c('shift<20','shift>=20 & non-Eng','shift>=20 & Eng'),
                      values=c('grey','orange','#F8766D')) +
  labs(title="Ranks by average citation")+
  xlab(NULL)+
  ylab('rank')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=20))
#now plot boxplots of changes in ave cit and changes in cit rank when shifting between sources
ggplot(melt(data.citation.processed[,c('wos.scopus','wos.msa','scopus.msa')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in ave. citation") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Change to ave. citation")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))
ggplot(melt(data.citation.processed[,c('wos.scopus.rank','wos.msa.rank','scopus.msa.rank')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in citation rank ") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Shift in rank by average citations")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))

###############################################


##############################################
##############################################
# Do the same for data collected from each source directly and for all output (not just DOIs)
# read in data after some analysis in Excel
data.citation.processed <- read.csv("citation.155.from.sources.directly.csv")
#plot ranks for all 155 uni using ggplot
data.citation.155.melt <- melt(as.data.frame(data.citation.processed[,c("grid_id",
                                                                        "cit_wos_rank","cit_scopus_rank","cit_msa_rank","colour")]),
                               id=c('grid_id','colour'))
ggplot() + 
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='grey',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=0.8) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='grey',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=1.2) +
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='red',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='red',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  geom_line(data=data.citation.155.melt[data.citation.155.melt$colour=='orange',],
            aes(x = variable, y = value, group=grid_id, colour = colour),size=1) +
  geom_point(data=data.citation.155.melt[data.citation.155.melt$colour=='orange',],
             aes(x = variable, y = value, group=grid_id, colour = colour),size=2) +
  scale_y_continuous(trans = "reverse", breaks = c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)) +
  scale_x_discrete(limits=c("cit_scopus_rank","cit_wos_rank","cit_msa_rank")) +
  scale_colour_manual('Legend',labels=c('shift<20','shift>=20 & non-Eng','shift>=20 & Eng'),
                      values=c('grey','orange','#F8766D')) +
  labs(title="Ranks by average citation")+
  xlab(NULL)+
  ylab('rank')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=20))

#now plot boxplots of changes in ave cit and changes in cit rank when shifting between sources
ggplot(melt(data.citation.processed[,c('wos.scopus','wos.msa','scopus.msa')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in ave. citation") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Change to ave. citation")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))

ggplot(melt(data.citation.processed[,c('wos.scopus.rank','wos.msa.rank','scopus.msa.rank')]),aes(x=variable,y=value))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  ylab("difference in citation rank ") + xlab(NULL) + theme(axis.text = element_text(size = 12))+
  labs(title="Shift in rank by average citations")+
  theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=15))

  
  








