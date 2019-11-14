# This is a set of codes that accompanies the article:
# "Comparison of bibliographic data sources: Implications for the robustness of university rankings"
# by Chun-Kai (Karl) Huang, Cameron Neylon, Chloe Brookes-Kenworthy, Richard Hosking, Lucy Montgomery, Katie Wilson, Alkim Ozaygen
# This set of codes deals withfurther analyses of venn diagrams of coverages of DOIs (for 2016) by WoS, Scopus and MSA
# of the primary sample of 15 universities
# and the larger set of 155 unis,
# and produces related graphs. 
# data collected via the Curtin Open Knowledge Initiative Data Infrastrcture
# codes written by Chun-Kai (Karl) Huang

# packages required
# install.packages("rstudioapi")
# install.packages("VennDiagram")
# install.packages("RVAideMemoire")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("fGarch")
# install.packages("fMultivar")
# install.packages("sn")
# install.packages("dendextend")
# install.packages("cluster")
# install.packages("randomcoloR")
# install.packages("data.table")

###############################################
# set path of working directory to same folder as the current file location
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# load data file
load("analysis_on_venn_diag.RData") #these are data for 2016 only

###############################################
# listing the relevant sets of grid ids
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
  "grid.254444.7"
)
# grid ids for those institutions with known problems, 
# e.g., missing or incorrect source ids/search terms, mixed up campuses
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
# get the counts and proportions of each part of the venn diagram for each grid id
# and make a table for counts and a table for proportions
library(VennDiagram)
data.venn.count <- matrix(,nrow=0,ncol=8)
colnames(data.venn.count) <- c("grid_id",1:7)
data.venn.prop <- matrix(,nrow=0,ncol=8)
colnames(data.venn.prop) <- c("grid_id",1:7)
for (g in 1:length(grid.ids)) {
  grid.count <- grid.ids[g]
  grid.count.venn <- get.venn.partitions(list(t(unique(data.bigquery.wos[data.bigquery.wos$grid_id==grid.count,]$f0_)),
                                           t(unique(data.bigquery.scopus[data.bigquery.scopus$grid_id==grid.count,]$f0_)),
                                           t(unique(data.bigquery.msa[data.bigquery.msa$grid_id==grid.count,]$f0_))))
  data.venn.count <- rbind(data.venn.count,cbind(grid.ids[g],t(grid.count.venn$..count..)))
  data.venn.prop <- rbind(data.venn.prop,cbind(grid.ids[g],t(grid.count.venn$..count..)/sum(grid.count.venn$..count..)))
}
#remove results for the problem grid ids from the tables of venn diagram counts and proportions
data.venn.count <- data.venn.count[!(data.venn.count[,1]%in%grid.ids.problems),]
data.venn.prop <- data.venn.prop[!(data.venn.prop[,1]%in%grid.ids.problems),]
data.venn.count.only <- data.venn.count[,2:8]
mode(data.venn.count.only) <- "numeric"
data.venn.prop.only <- data.venn.prop[,2:8]
mode(data.venn.prop.only) <- "numeric"

###############################################
# Box plots of proportions of DOIs in each section of the venn diagram across 155 universities
temp <- as.data.frame(rbind(cbind(prop=data.venn.prop.only[,1],label=rep('WSM',155)),
                            cbind(prop=data.venn.prop.only[,2],label=rep('SM',155)),
                            cbind(prop=data.venn.prop.only[,3],label=rep('WM',155)),
                            cbind(prop=data.venn.prop.only[,4],label=rep('M',155)),
                            cbind(prop=data.venn.prop.only[,5],label=rep('WS',155)),
                            cbind(prop=data.venn.prop.only[,6],label=rep('S',155)),
                            cbind(prop=data.venn.prop.only[,7],label=rep('W',155))
))
temp$label<-factor(temp$label,levels = rev(c('WSM','WS','WM','SM','W','S','M')),ordered = TRUE)
temp$prop <- as.character(temp$prop)
temp$prop <- as.numeric(temp$prop)
library(ggplot2)
ggplot(temp,aes(x=label,y=prop))+
  geom_boxplot(# custom boxes
    color="blue",
    fill="blue",
    alpha=0.3,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=2
  ) + 
  coord_flip() + 
  ylab("Proportions") + xlab(NULL) + theme(axis.text = element_text(size = 12))

###############################################
# define variables of total counts and proportions for each source
# and produce some simple plots
p_msa = data.venn.prop.only[,1]+data.venn.prop.only[,2]+data.venn.prop.only[,3]+data.venn.prop.only[,4]
p_scopus = data.venn.prop.only[,1]+data.venn.prop.only[,2]+data.venn.prop.only[,5]+data.venn.prop.only[,6]
p_wos = data.venn.prop.only[,1]+data.venn.prop.only[,3]+data.venn.prop.only[,5]+data.venn.prop.only[,7]
c_msa = data.venn.count.only[,1]+data.venn.count.only[,2]+data.venn.count.only[,3]+data.venn.count.only[,4]
c_scopus = data.venn.count.only[,1]+data.venn.count.only[,2]+data.venn.count.only[,5]+data.venn.count.only[,6]
c_wos = data.venn.count.only[,1]+data.venn.count.only[,3]+data.venn.count.only[,5]+data.venn.count.only[,7]
c_all = rowSums(data.venn.count.only)
hist(c_all) #distribution of total DOIs (all sources combined) for each institution
# distribution of proportions of DOIs in each source for all 155 universities
par(mfrow=c(1,3))
hist(p_wos,breaks=seq(0,1,by=0.05),ylim=c(0,60))
hist(p_scopus,breaks=seq(0,1,by=0.05),ylim=c(0,60))
hist(p_msa,breaks=seq(0,1,by=0.05),ylim=c(0,60))
# calculate correlations of DOI counts and proportions across the 3 sources
View(cor(cbind(p_wos,p_scopus,p_msa)))
View(cor(cbind(p_wos,p_scopus,p_msa),method="spearman"))
View(cor(cbind(c_wos,c_scopus,c_msa)))
View(cor(cbind(c_wos,c_scopus,c_msa),method="spearman"))
# excess kurtosis of proportions of DOI coverage by sources
kurtosis(p_wos)
kurtosis(p_scopus)
kurtosis(p_msa)
# construct pairwise scatterplots to check whether correlations are driven by size of total output numbers.
# dots coloured by output sizes of each institution
library(dplyr)
c_groups <- case_when(c_all>=15000 ~ "1",
                      c_all<15000&c_all>=10000 ~ "2",
                      c_all<10000&c_all>=5000 ~ "3",
                      c_all<5000&c_all>=2500 ~ "4",
                      c_all<2500 ~ "5"
)
par(mfrow=c(1,4))
plot(p_wos,p_scopus,col=c("palegreen1","palegreen2","palegreen3","palegreen4","darkgreen")[factor(c_groups)], pch=16)
plot(p_wos,p_msa,col=c("palegreen1","palegreen2","palegreen3","palegreen4","darkgreen")[factor(c_groups)], pch=16)
plot(p_scopus,p_msa,col=c("palegreen1","palegreen2","palegreen3","palegreen4","darkgreen")[factor(c_groups)], pch=16)
plot.new()
legend(x="topleft", legend = c(">15000","10000-15000","5000-10000","2500-5000","<2500"), col=c("palegreen1","palegreen2","palegreen3","palegreen4","darkgreen"), pch=16)

###############################################
# Test of homogeneity of venn diagrams across the sample of 15 universities
# and for all 155.
# Views each pair (doi,grid.id) as an object and each row is a sample from the population of DOIs for that grid.id
# test of homogeneity for only the 15 unis
data15.venn.count <- data.venn.count[data.venn.count[,1]%in%grids_of_interest,]
data15.venn.prop <- data.venn.prop[data.venn.prop[,1]%in%grids_of_interest,]
data15.venn.count.only <- data15.venn.count[,2:8]
data15.venn.prop.only <- data15.venn.prop[,2:8]
mode(data15.venn.count.only) <- "numeric"
mode(data15.venn.prop.only) <- "numeric"
temp <- chisq.test(data15.venn.count.only)
temp
sum(temp$expected==0) #0 cells with 0 expected count
sum(temp$expected<5) #0 cells have less than 5 expected count
sum(temp$expected>=5) #105 cells have 5 or more expected count, i.e., 100% > 80%
sum(temp$expected<10) #0 cells have less than 10 expected count
sum(temp$expected>=10) #105 cells have 10 or more expected count, i.e., 100% > 80%
chisq.test(data15.venn.count.only)
chisq.test(data15.venn.count.only,simulate.p.value=TRUE,B=5000) #with simulated p-value using 5000 replicates
library(RVAideMemoire)
G.test(data.venn.count.only) # G-Test
# test of homogeneity of rows/grid ids for all 155 unis. 
temp <- chisq.test(data.venn.count.only)
temp
sum(temp$expected==0) #0 cells with 0 expected count
sum(temp$expected<5) #0 cells have less than 5 expected count
sum(temp$expected>=5) #1085 cells have 5 or more expected count, i.e., 100% > 80%
sum(temp$expected<10) #0 cells have less than 10 expected count
sum(temp$expected>=10) #1085 cells have 10 or more expected count, i.e., 100% > 80%
chisq.test(data.venn.count.only)
chisq.test(data.venn.count.only,simulate.p.value=TRUE,B=5000) #with simulated p-value using 5000 replicates
library(RVAideMemoire)
G.test(data.venn.count.only) # G-Test
# codes for chi-square tests of bootstrapped samples of sizes from 10 to 155, in increments of 5.
# temp.bootstrapped <- vector()
# for (n in 1:30) {
#   temp.bootstrapped <- rbind(temp.bootstrapped,
#                         c(sample.n=nrow(data.venn.count.only)-150+n*5,
#                         chisq.p=chisq.test(data.venn.count.only[sample(nrow(data.venn.count.only),
#                                            size=nrow(data.venn.count.only)-150+n*5,replace=TRUE),],
#                                            simulate.p.value=1000)$p.value
#                         ))
# }

###############################################
# check for symmetry of the venn diagrams using differences across sections of the venn diagrams (define d)
# see detailed definitions of d1 (d.all), d2 (d.inner) and d3 (d.outer) in main article
d.all <- (abs(data.venn.prop.only[,2]-data.venn.prop.only[,3])
         + abs(data.venn.prop.only[,3]-data.venn.prop.only[,5])
         + abs(data.venn.prop.only[,5]-data.venn.prop.only[,2])
         + abs(data.venn.prop.only[,4]-data.venn.prop.only[,7])
         + abs(data.venn.prop.only[,7]-data.venn.prop.only[,6])
         + abs(data.venn.prop.only[,6]-data.venn.prop.only[,4]))/6
d.inner <- (abs(data.venn.prop.only[,2]-data.venn.prop.only[,3])
         + abs(data.venn.prop.only[,3]-data.venn.prop.only[,5])
         + abs(data.venn.prop.only[,5]-data.venn.prop.only[,2]))/3
d.outer <- (abs(data.venn.prop.only[,4]-data.venn.prop.only[,7])
         + abs(data.venn.prop.only[,7]-data.venn.prop.only[,6])
         + abs(data.venn.prop.only[,6]-data.venn.prop.only[,4]))/3
# plot d.all, d.inner and d.outer
plot.d.all <- hist(d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.d.all$counts <- plot.d.all$counts/sum(plot.d.all$counts)
plot(plot.d.all,freq=TRUE,ylab="Relative frequency", ylim=c(0,1), xlim=c(0,0.6))
plot.d.inner <- hist(d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.d.inner$counts <- plot.d.inner$counts/sum(plot.d.inner$counts)
plot(plot.d.inner,freq=TRUE,ylab="Relative frequency", ylim=c(0,1), xlim=c(0,0.6))
plot.d.outer <- hist(d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.d.outer$counts <- plot.d.outer$counts/sum(plot.d.outer$counts)
plot(plot.d.outer,freq=TRUE,ylab="Relative frequency", ylim=c(0,1), xlim=c(0,0.6))

###############################################
#simulate values for d using randomly generated symmetric venn diagrams
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p=runif(1,min=1/3,max=1)
  n=1:sample(200:35000,1) #generate a sequence of numbers of random size (size bounded by min and max sizes in our data)
  #assign these numbers to wos, scopus and msa using the same probability of inclusion 
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p,1-p))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p,1-p))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p,1-p))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
# plot the generated distributions of d, againt the ones from our data
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
#simulate values for d using randomly generated venn diagrams using indep uniform dists (not restricted to symmetric ones)
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p1=runif(1,min=1/3,max=1) #generate probabilities of inclusion independently
  p2=runif(1,min=1/3,max=1)
  p3=runif(1,min=1/3,max=1)
  n=1:sample(200:35000,1) #similar to above
  #assign these numbers to wos, scopus and msa using the independently generated uniform probablities of inclusion
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p1,1-p1))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p2,1-p2))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p3,1-p3))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
# plot the generated distributions of d, againt the ones from our data
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
#simulate values for d using randomly generated venn diagrams using norm dist (not restricted to symmetric ones) mu and sd from data
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p1=rnorm(1,mean=mean(p_wos),sd=sd(p_wos))
  p2=rnorm(1,mean=mean(p_scopus),sd=sd(p_scopus))
  p3=rnorm(1,mean=mean(p_msa),sd=sd(p_msa))
  if (p1<1/3) {p1=1/3}
  if (p2<1/3) {p2=1/3}
  if (p3<1/3) {p3=1/3}
  if (p1>1) {p1=1}
  if (p2>1) {p2=1}
  if (p3>1) {p3=1}
  n=1:sample(200:35000,1)
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p1,1-p1))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p2,1-p2))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p3,1-p3))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  #venn.diagram(list(sample.wos,sample.scopus,sample.msa), 
  #             filename="Venn_sample.tiff", 
  #             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
#simulate values for d using randomly generated venn diagrams using skew norm dist (not restricted to symmetric ones)
#parameters estimated from data
library(fGarch)
p_wos_skew_par <- snormFit(p_wos)$par #approximate paramters for the skew normal from data
p_scopus_skew_par <- snormFit(p_scopus)$par
p_msa_skew_par <- snormFit(p_msa)$par
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p1=rsnorm(1,mean=p_wos_skew_par[1],sd=p_wos_skew_par[2],xi=p_wos_skew_par[3])
  p2=rsnorm(1,mean=p_scopus_skew_par[1],sd=p_scopus_skew_par[2],xi=p_scopus_skew_par[3])
  p3=rsnorm(1,mean=p_msa_skew_par[1],sd=p_msa_skew_par[2],xi=p_msa_skew_par[3])
  if (p1<1/3) {p1=1/3}
  if (p2<1/3) {p2=1/3}
  if (p3<1/3) {p3=1/3}
  if (p1>1) {p1=1}
  if (p2>1) {p2=1}
  if (p3>1) {p3=1}
  n=1:sample(200:35000,1)
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p1,1-p1))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p2,1-p2))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p3,1-p3))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  #venn.diagram(list(sample.wos,sample.scopus,sample.msa), 
  #             filename="Venn_sample.tiff", 
  #             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
#simulate values for d using randomly generated venn diagrams using multivariate skew cauchy dist (or multi skew normal or multi skew t)
#parameters estimated from data
library(fMultivar)
library(sn)
p_all_mscfit <- mscFit(cbind(p_wos,p_scopus,p_msa)) #approximate paramters for the skew cauchy from data
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p_rmsc <- rmsc(n=1, xi=as.vector(p_all_mscfit@fit$estimated$beta), Omega=as.matrix(p_all_mscfit@fit$estimated$Omega), 
                 alpha=as.vector(p_all_mscfit@fit$estimated$alpha))
  p1=p_rmsc[1]
  p2=p_rmsc[2]
  p3=p_rmsc[3]
  if (p1<0) {p1=1/3}
  if (p2<0) {p2=1/3}
  if (p3<0) {p3=1/3}
  if (p1>1) {p1=1}
  if (p2>1) {p2=1}
  if (p3>1) {p3=1}
  n=1:sample(200:35000,1)
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p1,1-p1))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p2,1-p2))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p3,1-p3))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  #venn.diagram(list(sample.wos,sample.scopus,sample.msa), 
  #             filename="Venn_sample.tiff", 
  #             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
#simulate values for d using randomly generated venn diagrams using multivariate skew normal dist (or multi skew normal or multi skew t)
#parameters estimated from data
library(fMultivar)
library(sn)
p_all_mscfit <- msnFit(cbind(p_wos,p_scopus,p_msa)) #approximate paramters for the multivariate skew normal from data
sample.d.all <- vector()
sample.d.inner <- vector()
sample.d.outer <- vector()
for (m in 1:1000) {
  p_rmsc <- rmsn(n=1, xi=as.vector(p_all_mscfit@fit$estimated$beta), Omega=as.matrix(p_all_mscfit@fit$estimated$Omega), 
                 alpha=as.vector(p_all_mscfit@fit$estimated$alpha))
  p1=p_rmsc[1]
  p2=p_rmsc[2]
  p3=p_rmsc[3]
  if (p1<0) {p1=1/3}
  if (p2<0) {p2=1/3}
  if (p3<0) {p3=1/3}
  if (p1>1) {p1=1}
  if (p2>1) {p2=1}
  if (p3>1) {p3=1}
  n=1:sample(200:35000,1)
  sample.wos <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p1,1-p1))]
  sample.scopus <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p2,1-p2))]
  sample.msa <- n[sample(c(TRUE,FALSE),length(n),replace=TRUE,prob=c(p3,1-p3))]
  sample.venn <- get.venn.partitions(list(sample.wos,sample.scopus,sample.msa))
  #venn.diagram(list(sample.wos,sample.scopus,sample.msa), 
  #             filename="Venn_sample.tiff", 
  #             category.names = c("WoS","Scopus","MSA"),fill = c("red", "green", "blue"),col = "transparent")
  sample.d.all <- rbind(sample.d.all,
                        (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                         + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                         + abs(sample.venn$..count..[5]-sample.venn$..count..[2])
                         + abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                         + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                         + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/6)
  sample.d.inner <- rbind(sample.d.inner,
                          (abs(sample.venn$..count..[2]-sample.venn$..count..[3])
                           + abs(sample.venn$..count..[3]-sample.venn$..count..[5])
                           + abs(sample.venn$..count..[5]-sample.venn$..count..[2]))/sum(sample.venn$..count..)/3)
  sample.d.outer <- rbind(sample.d.outer,
                          (abs(sample.venn$..count..[4]-sample.venn$..count..[7])
                           + abs(sample.venn$..count..[7]-sample.venn$..count..[6])
                           + abs(sample.venn$..count..[6]-sample.venn$..count..[4]))/sum(sample.venn$..count..)/3)
}
par(mfrow=c(1,3))
plot.sample.d.all <- hist(sample.d.all,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.all$counts <- plot.sample.d.all$counts/sum(plot.sample.d.all$counts)
plot(plot.sample.d.all,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.all,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.inner <- hist(sample.d.inner,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.inner$counts <- plot.sample.d.inner$counts/sum(plot.sample.d.inner$counts)
plot(plot.sample.d.inner,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.inner,freq=TRUE,col=rgb(1,0,0,1/4),add=T)
plot.sample.d.outer <- hist(sample.d.outer,breaks=seq(0,0.6,by=0.02),plot=FALSE)
plot.sample.d.outer$counts <- plot.sample.d.outer$counts/sum(plot.sample.d.outer$counts)
plot(plot.sample.d.outer,freq=TRUE,ylab="Relative frequency",col=rgb(0,0,1,1/4), ylim=c(0,1), xlim=c(0,0.6))
plot(plot.d.outer,freq=TRUE,col=rgb(1,0,0,1/4),add=T)

###############################################
# clustering of uiversities using proportions in the venn diagrams
# produce a dendrogram of clustering of universities by venn digram proportions
library(dendextend)
library(cluster)
grid.ids.names <- read.csv('inst_id_lookup_table_20190308.csv',row.names=1)
#clustering all 156 universities
data.cluster <- data.venn.prop.only
rownames(data.cluster) <- grid.ids.names[data.venn.prop[,1],]$name
cluster <- as.dendrogram(hclust(daisy(data.cluster,metric="gower"),method="complete"))
#specifications for plot, colour by region
cluster <- cluster     %>% 
  set("labels_cex",0.7)
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions")
#read in groupings for unis, e.g., region, subregion, etc.
grid.ids.types <- read.csv('groupings_table.csv')
rownames(grid.ids.types) <- grid.ids.types$grid

###############################################
# as above but with colour by region
library(randomcoloR)
colour_lookup = data.frame(region=levels(grid.ids.types[data.venn.prop[,1],]$region)[-1],
                           type.colour=distinctColorPalette(length(levels(grid.ids.types[data.venn.prop[,1],]$region)[-1]),altCol=T))
colour_lookup_merge = grid.ids.types[,c("grid","uni_name","region")]
colour_lookup_merge = merge(colour_lookup_merge, colour_lookup, by="region")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data.venn.prop[,1],]
cluster <- cluster     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster)]))
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions (colour by region)")
legend("topright",legend=colour_lookup$region,
       fill=as.character(colour_lookup$type.colour),cex=0.7, ncol=2,bty='n')

###############################################
# as above but colour by subregion
colour_lookup = data.frame(subregion=levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1],
                           type.colour=distinctColorPalette(length(levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1]),altCol=T))
colour_lookup_merge = grid.ids.types[,c("grid","uni_name","subregion")]
colour_lookup_merge = merge(colour_lookup_merge, colour_lookup, by="subregion")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data.venn.prop[,1],]
cluster <- cluster     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster)]))
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions")
legend("topright",legend=colour_lookup$subregion,
       fill=as.character(colour_lookup$type.colour),cex=0.7, ncol=2,bty='n')

###############################################
# as above bbut only showing Europe in colour
colour_lookup = data.frame(subregion=levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1],
                           type.colour=distinctColorPalette(length(levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1]),altCol=T))
colour_lookup[!(colour_lookup$subregion%in%c('Central Europe','Eastern Europe','Northern Europe',
                                             'Southern Europe','Western Europe')),]$type.colour <- NA
colour_lookup_merge = grid.ids.types[,c("grid","uni_name","subregion")]
colour_lookup_merge = merge(colour_lookup_merge, colour_lookup, by="subregion")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data.venn.prop[,1],]
cluster <- cluster     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster)]))
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions")
legend("topright",legend=colour_lookup$subregion,
       fill=as.character(colour_lookup$type.colour),cex=0.7, ncol=2,bty='n')

###############################################
#showing only americas in colour
colour_lookup = data.frame(subregion=levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1],
                           type.colour=distinctColorPalette(length(levels(grid.ids.types[data.venn.prop[,1],]$subregion)[-1]),altCol=T))
colour_lookup[!(colour_lookup$subregion%in%c('Central America','Northern America','South America')),]$type.colour <- NA
colour_lookup_merge = grid.ids.types[,c("grid","uni_name","subregion")]
colour_lookup_merge = merge(colour_lookup_merge, colour_lookup, by="subregion")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data.venn.prop[,1],]
cluster <- cluster     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster)]))
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions")
legend("topright",legend=colour_lookup$subregion,
       fill=as.character(colour_lookup$type.colour),cex=0.7, ncol=2,bty='n')

###############################################
# as above but colour by THE ranking
library(data.table)
grid.ids.ranking <- read.csv('THERanking2019__grid.csv')
rownames(grid.ids.ranking) <- grid.ids.ranking$grid
colour_lookup = data.frame(rank_group=levels(factor(grid.ids.ranking[data.venn.prop[,1],]$rank_group)),
                           type.colour=c("red","orange","gold","yellow2","lightgreen","green4","deepskyblue","blue"))
colour_lookup_merge = data.table(grid.ids.ranking[,c("grid","rank_group")])
colour_lookup_merge = merge(colour_lookup, colour_lookup_merge, by.x="rank_group",by.y="rank_group")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data.venn.prop[,1],]
detach("package:data.table", unload=TRUE)
cluster <- cluster     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster)]))
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster, main="Cluster by Venn diagram proportions (colours by THE ranking)")
legend("topright",legend=c("1-20","21-50","51-100","101-200","201-500","501-800","801-1000","1001-","unknown"),
       fill=c(as.character(colour_lookup$type.colour),"black"),cex=0.7, ncol=2,bty='n')

###############################################
# clustering only 15 unis, no colours (default)
library(dendextend)
library(cluster)
data15.cluster <- data15.venn.prop.only
rownames(data15.cluster) <- grid.ids.names[data15.venn.prop[,1],]$name
cluster15 <- as.dendrogram(hclust(daisy(data15.cluster,metric="gower"),method="complete"))
cluster15 <- cluster15     %>% 
  set("labels_cex", value= 0.8)
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster15, main="Cluter by Venn diagram proportions (15 universities only)")

###############################################
# clustering 15 unis, colour by THE rankings
grid.ids.ranking <- read.csv('THERanking2019__grid.csv')
rownames(grid.ids.ranking) <- grid.ids.ranking$grid
colour_lookup = data.frame(rank_group=levels(factor(grid.ids.ranking[data15.venn.prop[,1],]$rank_group)),
                           type.colour=c("red","orange","yellow2","lightgreen","green4","deepskyblue"))
library(data.table)
colour_lookup_merge = data.table(grid.ids.ranking[,c("grid","rank_group")])
colour_lookup_merge = merge(colour_lookup, colour_lookup_merge, by.x="rank_group",by.y="rank_group")
rownames(colour_lookup_merge) <- colour_lookup_merge$grid
colour_lookup_merge = colour_lookup_merge[data15.venn.prop[,1],]
detach("package:data.table", unload=TRUE)
cluster15 <- cluster15     %>% 
  set("labels_colors", value= as.character(colour_lookup_merge$type.colour[order.dendrogram(cluster15)])) %>%
  set("labels_cex", value= 0.8)
x11(width=80, height=70)
par(mfrow=c(1,1))
par(mar=c(12,2,1,0))
plot(cluster15, main="Cluster by Venn diagram proportions (15 universities only, colours by THE ranking)")
legend("topright",legend=c("1-20","21-50","101-200","201-500","500-800","800-1000","unknown"),
       fill=c(as.character(colour_lookup$type.colour),"black"),cex=0.8, ncol=2,bty='n')























