# This is a set of codes that accompanies the article:
# "Comparison of bibliographic data sources: Implications for the robustness of university rankings"
# by Chun-Kai (Karl) Huang, Cameron Neylon, Chloe Brookes-Kenworthy, Richard Hosking, Lucy Montgomery, Katie Wilson, Alkim Ozaygen
# This produces the following for our primary sample of 15 universities:
# 1. A table summarising coverage of DOIs by sources and by Unpaywall
# 2. Venn diagrams relating to these coverages
# 3. Comparison of sources in publication years recorded
# 4. Comparison of sources in coverage of different document types
# data collected via the Curtin Open Knowledge Initiative Data Infrastrcture
# codes written by Chun-Kai (Karl) Huang

# packages required
# install.packages("rstudioapi")
# install.packages("VennDiagram")
# install.packages("eulerr")
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("gtools")

###############################################
# set path of working directory to same folder as the current file location
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# load data file
load("cross_validation.RData")

###############################################
# reads in basic information for the primary sample of 15 universities
UniIDs = read.csv('A sample of universities for bibliographic cross-validation.csv',sep=",")
# grid ids for the sample of 15 universities
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
  "grid.254444.7"
)

###############################################
# Defining and filtering the data in preparation for further analysis
# filter data down to each source for all years for all 15 universities
Data.wos.allyear.doi <- Data.all[sapply(Data.all, function(x) "wos_month"%in%names(x))]
Data.wos.allyear.doi <- unique(unlist(sapply(Data.wos.allyear.doi, function(x) unlist(x["f0_"]))))
Data.scopus.allyear.doi <- Data.all[sapply(Data.all, function(x) "scopus_month"%in%names(x))]
Data.scopus.allyear.doi <- unique(unlist(sapply(Data.scopus.allyear.doi, function(x) unlist(x["f0_"]))))
Data.msa.allyear.doi <- Data.all[sapply(Data.all, function(x) "msa_month"%in%names(x))]
Data.msa.allyear.doi <- unique(unlist(sapply(Data.msa.allyear.doi, function(x) unlist(x["f0_"]))))
# filter data down to each source for 2016 for all 15 universities
Data.wos <- Data.all[sapply(Data.all, function(x) "wos_month"%in%names(x))]
Data.wos <- Data.wos[sapply(Data.wos, function(x) grepl('2016',x["wos_month"],fixed=TRUE))]
Data.wos.doi <- unique(unlist(sapply(Data.wos, function(x) unlist(x["f0_"]))))
Data.scopus <- Data.all[sapply(Data.all, function(x) "scopus_month"%in%names(x))]
Data.scopus <- Data.scopus[sapply(Data.scopus, function(x) grepl('2016',x["scopus_month"],fixed=TRUE))]
Data.scopus.doi <- unique(unlist(sapply(Data.scopus, function(x) unlist(x["f0_"]))))
Data.msa <- Data.all[sapply(Data.all, function(x) "msa_month"%in%names(x))]
Data.msa <- Data.msa[sapply(Data.msa, function(x) grepl('2016',x["msa_month"],fixed=TRUE))]
Data.msa.doi <- unique(unlist(sapply(Data.msa, function(x) unlist(x["f0_"]))))
# get all dois indexed by unpaywall for all years, using the "year" field as indicator
Data.upw.doi <- Data.all[sapply(Data.all, function(x) "year"%in%names(x))]
Data.upw.doi <- unlist(sapply(Data.upw.doi, function(x) unlist(x["f0_"])))

###############################################
# calc number of DOIs from each source and how many are in unpaywall, for each university
count_grid_all <-vector()
for (grid.count in grids_of_interest) {
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  grid.comb.doi <- unique(c(grid.wos.doi,grid.scopus.doi,grid.msa.doi))
  count_grid_all <- rbind(count_grid_all,c(grid.id=grid.count,
                                           wos=length(grid.wos),
                                           wos.unpaywall=length(grid.wos.doi[grid.wos.doi%in%Data.upw.doi]),
                                           scopus=length(grid.scopus),
                                           scopus.unpaywall=length(grid.scopus.doi[grid.scopus.doi%in%Data.upw.doi]),
                                           msa=length(grid.msa),
                                           msa.unpaywall=length(grid.msa.doi[grid.msa.doi%in%Data.upw.doi]),
                                           combined=length(grid.comb.doi),
                                           comb.unpaywall=length(grid.comb.doi[grid.comb.doi%in%Data.upw.doi])
  ))
}
#same as above but for all 15 unis combined
library(VennDiagram)
Count = get.venn.partitions(list(t(Data.wos.doi),t(Data.scopus.doi),t(Data.msa.doi)))
sum(Count$..count..)
sum(Count[Count$X1==TRUE,]$..count..)
sum(Count[Count$X2==TRUE,]$..count..)
sum(Count[Count$X3==TRUE,]$..count..)
length(Data.wos.doi[Data.wos.doi%in%Data.upw.doi])
length(Data.scopus.doi[Data.scopus.doi%in%Data.upw.doi])
length(Data.msa.doi[Data.msa.doi%in%Data.upw.doi])
length(paste(unlist(Count$..values..))[paste(unlist(Count$..values..))%in%Data.upw.doi])

###############################################
# Venn diag of DOIs across all three sources for all years and all universities combined
library(VennDiagram)
venn.diagram(list(t(Data.wos.allyear.doi),t(Data.scopus.allyear.doi),t(Data.msa.allyear.doi)), 
             filename="Venn_all_years.tiff", 
             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
Count.allyear = get.venn.partitions(list(t(Data.wos.allyear.doi),t(Data.scopus.allyear.doi),t(Data.msa.allyear.doi)))
sum(Count.allyear$..count..)
# Alternative Venn diagram using proportional-areas and percentages
# adapted from codes provided by Alberto Martin-Martin, University of Granada
library(eulerr)
venn_data <- c('MSA' = Count.allyear$..count..[4],
               'Scopus' = Count.allyear$..count..[6],
               'WoS' = Count.allyear$..count..[7],
               'MSA&Scopus' = Count.allyear$..count..[2],
               'MSA&WoS' = Count.allyear$..count..[3],
               'Scopus&WoS' = Count.allyear$..count..[5],
               'MSA&Scopus&WoS' = Count.allyear$..count..[1]
)
total_venn <- sum(venn_data)
v <- euler(venn_data)
jpeg(file="Venn_all_years_new.jpg")
plot(v,
     fills = c('#99ccFF','#a6f5ad','#ff9999','#66cccc','#cc99ff','#ffcc66','#cccc99'),
     quantities = round(venn_data/total_venn*100 , digits = 1),
     labels = F, # change to TRUE if you prefer the label to be displayed in the diagram
     legend = T, # change to FALSE if you prefer to hide the legend
     main = paste("Total no. of DOIs = ",total_venn,sep="")
)
dev.off()

###################################################
# Venn diag of DOIs across all three sources for 2016 for all universities combined
library(VennDiagram)
venn.diagram(list(t(Data.wos.doi),t(Data.scopus.doi),t(Data.msa.doi)), 
             filename="Venn_all_2016.tiff", 
             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
Count = get.venn.partitions(list(t(Data.wos.doi),t(Data.scopus.doi),t(Data.msa.doi)))
sum(Count$..count..)
#Alternative Venn diagram using proportional-areas and percentages
#(adapted from codes provided by Alberto Martin-Martin, University of Granada
library(eulerr)
venn_data <- c('MSA' = Count$..count..[4],
               'Scopus' = Count$..count..[6],
               'WoS' = Count$..count..[7],
               'MSA&Scopus' = Count$..count..[2],
               'MSA&WoS' = Count$..count..[3],
               'Scopus&WoS' = Count$..count..[5],
               'MSA&Scopus&WoS' = Count$..count..[1]
)
total_venn <- sum(venn_data)
v <- euler(venn_data)
jpeg(file="Venn_all_2016_new.jpg")
plot(v,
     fills = c('#99ccFF','#a6f5ad','#ff9999','#66cccc','#cc99ff','#ffcc66','#cccc99'),
     quantities = round(venn_data/total_venn*100 , digits = 1),
     labels = F, # change to TRUE if you prefer the label to be displayed in the diagram
     legend = T, # change to FALSE if you prefer to hide the legend
     main = paste("Total no. of DOIs = ",total_venn,sep="")
)
dev.off()

###############################################
#Venn diagram of all DOIs, for each institution
library(VennDiagram)
library(eulerr)
for (grid.count in grids_of_interest) {
  # filter 2016 data to the current grid id
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  # extract DOIs
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  Count = get.venn.partitions(list(t(grid.wos.doi),t(grid.scopus.doi),t(grid.msa.doi)))
  venn_data <- c('MSA' = Count$..count..[4],
                 'Scopus' = Count$..count..[6],
                 'WoS' = Count$..count..[7],
                 'MSA&Scopus' = Count$..count..[2],
                 'MSA&WoS' = Count$..count..[3],
                 'Scopus&WoS' = Count$..count..[5],
                 'MSA&Scopus&WoS' = Count$..count..[1]
  )
  total_venn <- sum(venn_data)
  v <- euler(venn_data)
  jpeg(file=paste("Venn_",grid.count,"_2016_allDOI_new.jpg",sep=""))
  print(plot(v,
             fills = c('#99ccFF','#a6f5ad','#ff9999','#66cccc','#cc99ff','#ffcc66','#cccc99'),
             quantities = round(venn_data/total_venn*100 , digits = 1),
             labels = F, # change to TRUE if you prefer the label to be displayed in the diagram
             legend = T, # change to FALSE if you prefer to hide the legend
             main = paste(UniIDs[UniIDs$grid_id==grid.count,"name"],"\nTotal no. of DOIs = ",total_venn,sep="")
  ))
  dev.off()
}

###############################################
#Venn diagram of DOIs also recorded by Unpaywall, for each institution
library(VennDiagram)
library(eulerr)
for (grid.count in grids_of_interest) {
  # filter 2016 data to the current grid id
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  # filter down to those with type (unpaywall) recorded
  grid.wos <- grid.wos[sapply(grid.wos, function(x) "genre"%in%names(x))]
  grid.scopus <- grid.scopus[sapply(grid.scopus, function(x) "genre"%in%names(x))]
  grid.msa <- grid.msa[sapply(grid.msa, function(x) "genre"%in%names(x))]
  # extract DOIs
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  Count = get.venn.partitions(list(t(grid.wos.doi),t(grid.scopus.doi),t(grid.msa.doi)))
  venn_data <- c('MSA' = Count$..count..[4],
                 'Scopus' = Count$..count..[6],
                 'WoS' = Count$..count..[7],
                 'MSA&Scopus' = Count$..count..[2],
                 'MSA&WoS' = Count$..count..[3],
                 'Scopus&WoS' = Count$..count..[5],
                 'MSA&Scopus&WoS' = Count$..count..[1]
  )
  total_venn <- sum(venn_data)
  v <- euler(venn_data)
  jpeg(file=paste("Venn_",grid.count,"_2016_new.jpg",sep=""))
  print(plot(v,
             fills = c('#99ccFF','#a6f5ad','#ff9999','#66cccc','#cc99ff','#ffcc66','#cccc99'),
             quantities = round(venn_data/total_venn*100 , digits = 1),
             labels = F, # change to TRUE if you prefer the label to be displayed in the diagram
             legend = T, # change to FALSE if you prefer to hide the legend
             main = paste(UniIDs[UniIDs$grid_id==grid.count,"name"],"\nTotal no. of DOIs = ",total_venn,sep="")
  ))
  dev.off()
}

###############################################
# Calc percentages of 2016 DOIs exclusive to one source lying in a different source for a different year (all 15 universities combined)
Data.wos.doi.byyear <- list()
Data.scopus.doi.byyear <- list()
Data.msa.doi.byyear <- list()
year <- 2000:2018
# Gets DOIs from each source split into per year sets
Temp <- Data.all[sapply(Data.all, function(x) "wos_month"%in%names(x))]
for (k in 1:19) {
  Temp1 <- Temp[sapply(Temp, function(x) grepl(year[k],x["wos_month"],fixed=TRUE))]
  Data.wos.doi.byyear[k] <- list(unique(unlist(sapply(Temp1, function(x) unlist(x["f0_"])))))
}
Temp <- Data.all[sapply(Data.all, function(x) "scopus_month"%in%names(x))]
for (k in 1:19) {
  Temp1 <- Temp[sapply(Temp, function(x) grepl(year[k],x["scopus_month"],fixed=TRUE))]
  Data.scopus.doi.byyear[k] <- list(unique(unlist(sapply(Temp1, function(x) unlist(x["f0_"])))))
}
Temp <- Data.all[sapply(Data.all, function(x) "msa_month"%in%names(x))]
for (k in 1:19) {
  Temp1 <- Temp[sapply(Temp, function(x) grepl(year[k],x["msa_month"],fixed=TRUE))]
  Data.msa.doi.byyear[k] <- list(unique(unlist(sapply(Temp1, function(x) unlist(x["f0_"])))))
}
# Calculate counts of DOIs per year per source
counts_per_year <- cbind(as.data.frame(lengths(Data.wos.doi.byyear)),as.data.frame(lengths(Data.scopus.doi.byyear)),
                         as.data.frame(lengths(Data.msa.doi.byyear)))
rm(Temp)
# Get Venn diagram DOI counts for all universities for 2016
Count = get.venn.partitions(list(t(Data.wos.doi),t(Data.scopus.doi),t(Data.msa.doi)))
# Check how many DOIs exclusively from one source in 2016 falls in another source for a different year
wos2016_in_others <- cbind(lengths(sapply(Data.scopus.doi.byyear, function(x) x[x%in%unlist(Count[7,]$..values..)] )),
                    lengths(sapply(Data.msa.doi.byyear, function(x) x[x%in%unlist(Count[7,]$..values..)] )))
scopus2016_in_others <- cbind(lengths(sapply(Data.wos.doi.byyear, function(x) x[x%in%unlist(Count[6,]$..values..)] )),
                           lengths(sapply(Data.msa.doi.byyear, function(x) x[x%in%unlist(Count[6,]$..values..)] )))
msa2016_in_others <- cbind(lengths(sapply(Data.wos.doi.byyear, function(x) x[x%in%unlist(Count[4,]$..values..)] )),
                           lengths(sapply(Data.scopus.doi.byyear, function(x) x[x%in%unlist(Count[4,]$..values..)] )))
# Rearrange the results and combine some rows for ease of plotting
Temp <- cbind(W_in_S=wos2016_in_others[,1],
              W_in_M=wos2016_in_others[,2],
              S_in_W=scopus2016_in_others[,1],
              S_in_M=scopus2016_in_others[,2],
              M_in_W=msa2016_in_others[,1],
              M_in_S=msa2016_in_others[,2])
Temp <- rbind(colSums(Temp[1:5,]),
              colSums(Temp[6:9,]),
              colSums(Temp[10:13,]),
              Temp[14:19,])
Temp <- cbind(Year=c('2000-\n2004','2005-\n2008','2009-\n2012',2013:2018),Temp)
# Split results as per source
# WoS
Temp.W <- as.data.frame(Temp[,1:3])
Temp.W$Year <- as.factor(Temp.W$Year)
Temp.W$W_in_S <- as.numeric(as.character(Temp.W$W_in_S))
Temp.W$W_in_M <- as.numeric(as.character(Temp.W$W_in_M))
colnames(Temp.W)[2:3]<-c('Scopus','MSA')
Temp.W[Temp.W==0]<-NA
# Scopus
Temp.S <- as.data.frame(Temp[,c(1,4,5)])
Temp.S$Year <- as.factor(Temp.S$Year)
Temp.S$S_in_W <- as.numeric(as.character(Temp.S$S_in_W))
Temp.S$S_in_M <- as.numeric(as.character(Temp.S$S_in_M))
colnames(Temp.S)[2:3]<-c('WoS','MSA')
Temp.S[Temp.S==0]<-NA
# MSA
Temp.M <- as.data.frame(Temp[,c(1,6,7)])
Temp.M$Year <- as.factor(Temp.M$Year)
Temp.M$M_in_W <- as.numeric(as.character(Temp.M$M_in_W))
Temp.M$M_in_S <- as.numeric(as.character(Temp.M$M_in_S))
colnames(Temp.M)[2:3]<-c('WoS','Scopus')
Temp.M[Temp.M==0]<-NA
# Melt each result in preparation for plotting
library(reshape2)
Temp.W <- melt(Temp.W,id.vars='Year')
Temp.S <- melt(Temp.S,id.vars='Year')
Temp.M <- melt(Temp.M,id.vars='Year')
# define colours
colour <- c("WoS"="#F8766D","Scopus"="#00BA38","MSA"="#619CFF")
library(ggplot2)
library(gridExtra)
library(grid)
# Plot results for each source on the same grid, using log scale on y-axis
grid.arrange(
ggplot(Temp.W,aes(x=Year,y=value,fill = variable))+
  geom_bar(position = "dodge", stat="identity")+
  scale_y_log10(limit=c(1,1500))+
  ylab("Count in log scale")+
  xlab(NULL)+
  scale_fill_manual(values=colour, drop = FALSE)+
  labs(title="Distribution of WoS DOIs",fill="Target sources")+
  theme(plot.title = element_text(hjust = 0.5)),
ggplot(Temp.S,aes(x=Year,y=value,fill = variable))+
  geom_bar(position = "dodge", stat="identity")+
  scale_y_log10(limit=c(1,1500))+
  ylab("Count in log scale")+
  xlab(NULL)+
  scale_fill_manual(values=colour, drop = FALSE)+
  labs(title="Distribution of Scopus DOIs",fill="Target sources")+
  theme(plot.title = element_text(hjust = 0.5)),
ggplot(Temp.M,aes(x=Year,y=value,fill = variable))+
  geom_bar(position = "dodge", stat="identity")+
  scale_y_log10(limit=c(1,1500))+
  ylab("Count in log scale")+
  xlab(NULL)+
  scale_fill_manual(values=colour, drop = FALSE)+
  labs(title="Distribution of MSA DOIs",fill="Target sources")+
  theme(plot.title = element_text(hjust = 0.5))
)

###############################################
# Calc % of agreements on publication year between sources (for all 15 universities and all years)
#unlist(Data.wos.doi.byyear)[duplicated(unlist(Data.wos.doi.byyear))] # 1 duplicated DOI that falls in different years in WoS
#unlist(Data.scopus.doi.byyear)[duplicated(unlist(Data.scopus.doi.byyear))] # 2 duplicated DOIs fall in diff years in scopus
#unlist(Data.msa.doi.byyear)[duplicated(unlist(Data.msa.doi.byyear))] # 43 duplicated DOI that fall in diff years in msa
length(unlist(Data.wos.doi.byyear)[unlist(Data.wos.doi.byyear)%in%unlist(Data.scopus.doi.byyear)])
length(unlist(Data.wos.doi.byyear)[unlist(Data.wos.doi.byyear)%in%unlist(Data.msa.doi.byyear)])
length(unlist(Data.scopus.doi.byyear)[unlist(Data.scopus.doi.byyear)%in%unlist(Data.msa.doi.byyear)])
length(unlist(Data.scopus.doi.byyear)[(unlist(Data.scopus.doi.byyear)%in%unlist(Data.msa.doi.byyear))&
                                        (unlist(Data.scopus.doi.byyear)%in%unlist(Data.wos.doi.byyear))])
agreement_byyear <- vector()
for (k in 1:length(year)) {
  agreement_byyear <- rbind(agreement_byyear,c(
    year       = year[k],
    wos.scopus = length(unlist(Data.wos.doi.byyear[k])[unlist(Data.wos.doi.byyear[k])%in%unlist(Data.scopus.doi.byyear[k])]),
    wos.msa    = length(unlist(Data.wos.doi.byyear[k])[unlist(Data.wos.doi.byyear[k])%in%unlist(Data.msa.doi.byyear[k])]),
    scopus.msa = length(unlist(Data.scopus.doi.byyear[k])[unlist(Data.scopus.doi.byyear[k])%in%unlist(Data.msa.doi.byyear[k])]),
    all        = length(unlist(Data.scopus.doi.byyear[k])[(unlist(Data.scopus.doi.byyear[k])%in%unlist(Data.msa.doi.byyear[k]))&
                                                            (unlist(Data.scopus.doi.byyear[k])%in%unlist(Data.wos.doi.byyear[k]))])
  ))
}
colSums(agreement_byyear)

###############################################
# Calc number/% of DOIs from each source that lies in a different year for a different source (for each inst for 2016)
Table.another_yr_source <- vector()
for (grid.count in grids_of_interest) {
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  Table.another_yr_source <- rbind(Table.another_yr_source,c(
    grid.id   = grid.count,
    wos.1y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(16,18)],
                                                                  Data.msa.doi.byyear[c(16,18)]))])/length(grid.wos.doi)*100,
    wos.2y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(15,19)],
                                                                  Data.msa.doi.byyear[c(15,19)]))])/length(grid.wos.doi)*100,
    scopus.1y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(16,18)],
                                                                        Data.msa.doi.byyear[c(16,18)]))])/length(grid.scopus.doi)*100,
    scopus.2y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(15,19)],
                                                                        Data.msa.doi.byyear[c(15,19)]))])/length(grid.scopus.doi)*100,
    msa.1y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(16,18)],
                                                                  Data.scopus.doi.byyear[c(16,18)]))])/length(grid.msa.doi)*100,
    msa.2y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(15,19)],
                                                                  Data.scopus.doi.byyear[c(15,19)]))])/length(grid.msa.doi)*100
  ))
}

###############################################
# Calc number/% of DOIs from each source that lies in a different year for a different source (for each inst for 2016)
# this time with directions of difference in year picked out for all 15 uni
Table.another_yr_source2 <- vector()
for (grid.count in grids_of_interest) {
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  Table.another_yr_source2 <- rbind(Table.another_yr_source2,c(
    grid.id   = grid.count,
    wos._2y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(15)],
                                                                   Data.msa.doi.byyear[c(15)]))])/length(grid.wos.doi)*100,
    wos._1y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(16)],
                                                                   Data.msa.doi.byyear[c(16)]))])/length(grid.wos.doi)*100,
    wos.1y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(18)],
                                                                  Data.msa.doi.byyear[c(18)]))])/length(grid.wos.doi)*100,
    wos.2y    = length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(19)],
                                                                  Data.msa.doi.byyear[c(19)]))])/length(grid.wos.doi)*100,
    scopus._2y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(15)],
                                                                         Data.msa.doi.byyear[c(15)]))])/length(grid.scopus.doi)*100,
    scopus._1y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(16)],
                                                                         Data.msa.doi.byyear[c(16)]))])/length(grid.scopus.doi)*100,
    scopus.1y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(18)],
                                                                        Data.msa.doi.byyear[c(18)]))])/length(grid.scopus.doi)*100,
    scopus.2y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(19)],
                                                                        Data.msa.doi.byyear[c(19)]))])/length(grid.scopus.doi)*100,
    msa._2y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(15)],
                                                                   Data.scopus.doi.byyear[c(15)]))])/length(grid.msa.doi)*100,
    msa._1y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(16)],
                                                                   Data.scopus.doi.byyear[c(16)]))])/length(grid.msa.doi)*100,
    msa.1y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(18)],
                                                                  Data.scopus.doi.byyear[c(18)]))])/length(grid.msa.doi)*100,
    msa.2y    = length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(19)],
                                                                  Data.scopus.doi.byyear[c(19)]))])/length(grid.msa.doi)*100
  ))
}

###############################################
# Calc number/% of DOIs from each source that lies in a different year for a different source (for 2016)
# this time with directions of difference in year picked out for Cairo, IISC and ITB
# only for Scopus and WoS
# This is recorded in Supplementary Material 7
Table.another_yr_source3 <- vector()
for (grid.count in c("grid.7776.1","grid.34980.36","grid.434933.a")) {
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  Table.another_yr_source3 <- rbind(Table.another_yr_source3,c(
    grid.id   = grid.count,
    scopus._2y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(Data.msa.doi.byyear[c(15)])])/length(grid.scopus.doi)*100,
    scopus._1y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(Data.msa.doi.byyear[c(16)])])/length(grid.scopus.doi)*100,
    scopus.1y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(Data.msa.doi.byyear[c(18)])])/length(grid.scopus.doi)*100,
    scopus.2y = length(grid.scopus.doi[grid.scopus.doi%in%unlist(Data.msa.doi.byyear[c(19)])])/length(grid.scopus.doi)*100,
    msa._2y    = length(grid.msa.doi[grid.msa.doi%in%unlist(Data.scopus.doi.byyear[c(15)])])/length(grid.msa.doi)*100,
    msa._1y    = length(grid.msa.doi[grid.msa.doi%in%unlist(Data.scopus.doi.byyear[c(16)])])/length(grid.msa.doi)*100,
    msa.1y    = length(grid.msa.doi[grid.msa.doi%in%unlist(Data.scopus.doi.byyear[c(18)])])/length(grid.msa.doi)*100,
    msa.2y    = length(grid.msa.doi[grid.msa.doi%in%unlist(Data.scopus.doi.byyear[c(19)])])/length(grid.msa.doi)*100
  ))
}

###############################################
# Calc number/% of DOIs exclusively from each source that lies in a different year for a different source (for each inst for 2016)
Table.excl.another_yr_source <- vector()
for (grid.count in grids_of_interest) {
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  grid.venn = get.venn.partitions(list(t(grid.wos.doi),t(grid.scopus.doi),t(grid.msa.doi)))
  grid.wos.doi <- unlist(grid.venn[7,]$..values..)
  grid.scopus.doi <- unlist(grid.venn[6,]$..values..)
  grid.msa.doi <- unlist(grid.venn[4,]$..values..)
  wos.bf    = length(grid.wos.doi)
  wos.1y    = length(grid.wos.doi)-
    length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(16,18)],
                                                      Data.msa.doi.byyear[c(16,18)]))])
  wos.2y    = wos.1y-
    length(grid.wos.doi[grid.wos.doi%in%unlist(append(Data.scopus.doi.byyear[c(15,19)],
                                                      Data.msa.doi.byyear[c(15,19)]))])
  scopus.bf = length(grid.scopus.doi)
  scopus.1y = length(grid.scopus.doi)-
    length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(16,18)],
                                                            Data.msa.doi.byyear[c(16,18)]))])
  scopus.2y = scopus.1y-
    length(grid.scopus.doi[grid.scopus.doi%in%unlist(append(Data.wos.doi.byyear[c(15,19)],
                                                            Data.msa.doi.byyear[c(15,19)]))])
  msa.bf    = length(grid.msa.doi)
  msa.1y    = length(grid.msa.doi)-
    length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(16,18)],
                                                      Data.scopus.doi.byyear[c(16,18)]))])
  msa.2y    = msa.1y-
    length(grid.msa.doi[grid.msa.doi%in%unlist(append(Data.wos.doi.byyear[c(15,19)],
                                                      Data.scopus.doi.byyear[c(15,19)]))])
  Table.excl.another_yr_source <- rbind(Table.excl.another_yr_source,
                                        c(grid.id       =grid.count,
                                          wos.before    =wos.bf,
                                          wos.1year     =wos.1y,
                                          wos.2year     =wos.2y,
                                          scopus.before =scopus.bf,
                                          scopus.1year  =scopus.1y,
                                          scopus.2year  =scopus.2y,
                                          msa.before    =msa.bf,
                                          msa.1year     =msa.1y,
                                          msa.2year     =msa.2y))
}

###############################################
# determine doc type within each part of the Venn diagram for all DOIs combined for all 15 universities and all 19 years
doctype_overall<-data.frame()
library(gtools)
for (k in 1:length(Count.allyear[,1])) {
  Temp <- Data.all[sapply(Data.all, function(x) "genre"%in%names(x))]
  if (Count.allyear[k,1]==TRUE) {Temp <- Temp[sapply(Temp, function(x) "wos_month"%in%names(x))]
  } else {Temp <- Temp[sapply(Temp, function(x) !("wos_month"%in%names(x)))]}
  if (Count.allyear[k,2]==TRUE) {Temp <- Temp[sapply(Temp, function(x) "scopus_month"%in%names(x))]
  } else {Temp <- Temp[sapply(Temp, function(x) !("scopus_month"%in%names(x)))]}
  if (Count.allyear[k,3]==TRUE) {Temp <- Temp[sapply(Temp, function(x) "msa_month"%in%names(x))]
  } else {Temp <- Temp[sapply(Temp, function(x) !("msa_month"%in%names(x)))]}
  Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
  doctype_overall<- smartbind(doctype_overall,
                              as.data.frame(rbind(table(sapply(Temp, function(x) unlist(x['genre']))))))
}
doctype_overall <- cbind(Count.allyear[,1:3],doctype_overall)

###############################################
# determine doc types as above but for each institution and for 2016 only
doctype_each_inst<-list()
library(gtools)
library(VennDiagram)
for (grid.count in grids_of_interest) {
  # filter 2016 data to the current grid id
  grid.wos <- Data.wos[sapply(Data.wos, function(x) x["grid_id"]==grid.count)]
  grid.scopus <- Data.scopus[sapply(Data.scopus, function(x) x["grid_id"]==grid.count)]
  grid.msa <- Data.msa[sapply(Data.msa, function(x) x["grid_id"]==grid.count)]
  # filter down to those with type (unpaywall) recorded
  grid.wos <- grid.wos[sapply(grid.wos, function(x) "genre"%in%names(x))]
  grid.scopus <- grid.scopus[sapply(grid.scopus, function(x) "genre"%in%names(x))]
  grid.msa <- grid.msa[sapply(grid.msa, function(x) "genre"%in%names(x))]
  # extract DOIs
  grid.wos.doi <- unique(unlist(sapply(grid.wos, function(x) unlist(x["f0_"]))))
  grid.scopus.doi <- unique(unlist(sapply(grid.scopus, function(x) unlist(x["f0_"]))))
  grid.msa.doi <- unique(unlist(sapply(grid.msa, function(x) unlist(x["f0_"]))))
  # A Venn diagram of DOIs with doc type (i.e., those in unpaywall with doc type recorded)
  venn.diagram(list(t(grid.wos.doi),t(grid.scopus.doi),t(grid.msa.doi)), 
               filename=paste("Venn_",grid.count,"_2016.tiff",sep=""), 
               category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent",
               main=UniIDs[UniIDs$grid_id==grid.count,"name"])
  grid.venn <- get.venn.partitions(list(t(grid.wos.doi),t(grid.scopus.doi),t(grid.msa.doi)))
  grid.all <- append(grid.wos,grid.scopus)
  grid.all <- append(grid.all,grid.msa)
  grid.doctype <- data.frame()
  # Get doc type for each section of the Venn diagram
  for (k in 1:length(grid.venn[,1])) {
    Temp <- grid.all[sapply(grid.all, function(x) "genre"%in%names(x))]
    Temp1 <- unlist(grid.venn[k,]$..values..)
    Temp <- Temp[sapply(Temp, function(x) unlist(x['f0_'])%in%Temp1)]
    Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
    grid.doctype<- smartbind(grid.doctype,
                             as.data.frame(rbind(table(sapply(Temp, function(x) unlist(x['genre']))))))
  }
  grid.doctype <- cbind(grid.venn[,1:3],grid.doctype)
  doctype_each_inst <- append(doctype_each_inst,list(grid.doctype))
}
for (k in 1:length(grids_of_interest)){
  write.csv(doctype_each_inst[[k]],file = paste("doc_type_",grids_of_interest[k],".csv",sep=""))
}

###############################################
# calculate overall OA for each source for all years (dates by each source)
oa_overall <- vector()
Temp <- Data.all[sapply(Data.all, function(x) "wos_month"%in%names(x))]
Temp <- Temp[sapply(Temp, function(x) "is_oa"%in%names(x))]
Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
oa_overall <- cbind(oa_overall,wos_total=length(Temp))
oa_overall <- cbind(oa_overall,wos_oa=length(Temp[sapply(Temp, function(x) x['is_oa']==TRUE)]))
Temp <- Data.all[sapply(Data.all, function(x) "scopus_month"%in%names(x))]
Temp <- Temp[sapply(Temp, function(x) "is_oa"%in%names(x))]
Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
oa_overall <- cbind(oa_overall,scopus_total=length(Temp))
oa_overall <- cbind(oa_overall,scopus_oa=length(Temp[sapply(Temp, function(x) x['is_oa']==TRUE)]))
Temp <- Data.all[sapply(Data.all, function(x) "msa_month"%in%names(x))]
Temp <- Temp[sapply(Temp, function(x) "is_oa"%in%names(x))]
Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
oa_overall <- cbind(oa_overall,msa_total=length(Temp))
oa_overall <- cbind(oa_overall,msa_oa=length(Temp[sapply(Temp, function(x) x['is_oa']==TRUE)]))
Temp <- Data.all[sapply(Data.all, function(x) "is_oa"%in%names(x))]
Temp <- Temp[!duplicated(sapply(Temp, function(x) unlist(x['f0_'])))]
oa_overall <- cbind(oa_overall,all_total=length(Temp))
oa_overall <- cbind(oa_overall,all_oa=length(Temp[sapply(Temp, function(x) x['is_oa']==TRUE)]))
rm(Temp)

###############################################
# calculate citation numbers for all years combined for each source using opencitations
#create a list of unpaywall dois
Data.upw.doi <- Data.all[sapply(Data.all, function(x) "is_oa"%in%names(x))]
Data.upw.doi <- unlist(sapply(Data.upw.doi, function(x) unlist(x["f0_"])))
#calculate to citations for all 15 unis and 2000 to 2018 combined,
#using dois indexed in unpaywall
sum(data.opencitations[data.opencitations$doi%in%
                         Data.wos.allyear.doi[Data.wos.allyear.doi%in%Data.upw.doi],]$citations_total)
sum(data.opencitations[data.opencitations$doi%in%
                         Data.scopus.allyear.doi[Data.scopus.allyear.doi%in%Data.upw.doi],]$citations_total)
sum(data.opencitations[data.opencitations$doi%in%
                         Data.msa.allyear.doi[Data.msa.allyear.doi%in%Data.upw.doi],]$citations_total)
sum(data.opencitations[data.opencitations$doi%in%
                         unique(c(Data.wos.allyear.doi[Data.wos.allyear.doi%in%Data.upw.doi],
                                  Data.scopus.allyear.doi[Data.scopus.allyear.doi%in%Data.upw.doi],
                                  Data.msa.allyear.doi[Data.msa.allyear.doi%in%Data.upw.doi])),
                       ]$citations_total)
#using the full sets of dois, i.e., including non-Unpaywall dois
#sum(data.opencitations[data.opencitations$doi%in%Data.wos.allyear.doi,]$citations_total)
#sum(data.opencitations[data.opencitations$doi%in%Data.scopus.allyear.doi,]$citations_total)
#sum(data.opencitations[data.opencitations$doi%in%Data.msa.allyear.doi,]$citations_total)
#sum(data.opencitations[data.opencitations$doi%in%
#            unique(c(Data.wos.allyear.doi,Data.scopus.allyear.doi,Data.msa.allyear.doi)),
#            ]$citations_total)
# count total number of DOIs indexed by Unpaywall for each source and for combined set 
length(Data.wos.allyear.doi[Data.wos.allyear.doi%in%Data.upw.doi])
length(Data.scopus.allyear.doi[Data.scopus.allyear.doi%in%Data.upw.doi])
length(Data.msa.allyear.doi[Data.msa.allyear.doi%in%Data.upw.doi])
length(unique(c(Data.wos.allyear.doi[Data.wos.allyear.doi%in%Data.upw.doi],
                Data.scopus.allyear.doi[Data.scopus.allyear.doi%in%Data.upw.doi],
                Data.msa.allyear.doi[Data.msa.allyear.doi%in%Data.upw.doi])))





