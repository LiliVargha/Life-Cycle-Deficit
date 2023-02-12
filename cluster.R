# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### Visualizing Labour Income Age Profiles
####### Towards a typology of intergenerational reallocation patterns: Clustering of countries based on NTA (and NTTA) age profiles

####### Lili Vargha, Tanja Istenic, Timothy Miller
####### Work in Progress

####### Contact: Lili Vargha (lili.vargha@hu-berlin.de or vargha@demografia.hu)

####### YL CLUSTERS, LCD CLUSTERS
####### gender specific clusters are found in different R files: YL_byGender.R and LCD_byGender.R

####### Original data source:
####### 1. Global NTA results (Lee and Mason 2011): https://www.ntaccounts.org/web/nta/show/Browse%20database
####### 2. European AGENTA Project (Istenic et al. 2019): http://dataexplorer.wittgensteincentre.org/nta/


####### Data downloaded from the NTA website: July 2020 (There might have been updates, so please check)
####### Data downloaded from AGENTA: November 2022
####### Data downloaded from CWW: November 2022


####### Please note, check-ups and normalization of global NTA results were done in Stata.
####### Updates will be done in the future for fully replicable plots using long databases downloaded from the AGENTA and the NTA website.

####### Last update: 9 February 2023

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd ("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/NTA/2020")

####### Loading packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(plyr)
library(cowplot)
library(data.table)
library(reshape)
library(patchwork)
library(viridisLite)
library(viridis)
library(readstata13)
#library(tricolore)
library(shiny)
library(sf)
library(devtools)
library(qlcMatrix)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First set of clusters: YL age profiles for 77 countries

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


####Data with normalized age profiles for non-EU countries

setwd ("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/NTA/2020")

nta2 <- read.dta13("nta.dta", nonint.factors= TRUE)

####European normalized NTA age profiles

ntadatamatrixeu <- t(as.matrix(read.DIF("nta2010_wide_Norm2.dif", dec = ".", transpose=TRUE, row.names = 1)))
str(ntadatamatrixeu)

mode(ntadatamatrixeu) <- 'numeric' 
ntaeu <- ntadatamatrixeu [1:86,]
mode(ntaeu) <- 'numeric'
dimnames(ntaeu)[[1]] <- character(0)
ntaeu <- as.data.frame(ntaeu)

###Linking together normalized non-EU countries (nta2) and normalized EU countries (ntaeu)

NTA <- cbind (nta2, ntaeu)
class(NTA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


####YL: LABOUR INCOME CLUSTERS 77 countries


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### selecting only YL variables

YLvar <- c("YL_AT", "YL_BE", "YL_BG", "YL_CY", "YL_CZ", "YL_DK", "YL_EE", "YL_FI", "YL_FR",
           "YL_DE", "YL_GR", "YL_HU", "YL_IE", "YL_IT", "YL_LV", "YL_LT", "YL_LU", "YL_PL",
           "YL_PT", "YL_RO", "YL_SK", "YL_SI", "YL_ES", "YL_SE", "YL_UK", "YL_AR", "YL_AU",
           "YL_BJ", "YL_BW", "YL_BR", "YL_BF", "YL_KH", "YL_CA", "YL_CF", "YL_TD", "YL_CN",
           "YL_CO", "YL_CR", "YL_SV", "YL_ET", "YL_GA", "YL_GM", "YL_GH", "YL_GN", "YL_GW",
           "YL_IN", "YL_ID", "YL_JM", "YL_JP", "YL_KE", "YL_MV", "YL_ML", "YL_MR", "YL_MX",
           "YL_MD", "YL_MN", "YL_MZ", "YL_NA", "YL_NE", "YL_NG", "YL_PE", "YL_PH", "YL_RU",
           "YL_ST", "YL_SN", "YL_SL", "YL_SG", "YL_ZA", "YL_KR", "YL_SZ", "YL_TW", "YL_TH",
           "YL_TL", "YL_TR", "YL_UY", "YL_US", "YL_VN", "age")

NTAYL <- NTA[YLvar]

colnames(NTAYL)

countries <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
               "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
               "Poland", "Portugalia", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom", "Argentina",
               "Australia", "Benin", "Botswana", "Brazil", "Burkina Faso", "Cameroon", "Canada", "Central Africa",
               "Chad", "China", "Colombia", "Costa Rica", "El Salvador", "Ethiopia", "Gabon", "Gambia", "Ghana",
               "Guinea", "Guinea-Bissau", "India", "Indonesia", "Jamaica", "Japan", "Kenya", "Maldives", "Mali",
               "Mauritania", "Mexico", "Moldova", "Mongolia", "Mozambique", "Namibia", "Niger", "Nigeria", "Peru",
               "Philippines", "Russia", "Sao Tome", "Senegal", "Sierra Leone", "Singapore", "South Africa", "South Korea",
               "Swaziland", "Taiwan", "Thailand", "Timor-Leste", "Turkey", "Uruguay", "United States", "Vietnam")


names(NTAYL)

NTAYLc <- rename(NTAYL, c("YL_AT"="Austria 2010", "YL_BE"="Belgium 2010","YL_BG"="Bulgaria 2010", "YL_CY"="Cyprus 2010", "YL_CZ"="Czech Republic 2010",
                          "YL_DK"="Denmark 2010", "YL_EE"="Estonia 2010", "YL_FI"="Finland 2010", "YL_FR"="France 2010", "YL_DE"="Germany 2010", "YL_GR"="Greece 2010",
                          "YL_HU"="Hungary 2010","YL_IE"= "Ireland 2010", "YL_IT"="Italy 2010", "YL_LV"="Latvia 2010", "YL_LT"="Lithuania 2010", "YL_LU"="Luxembourg 2010",
                          "YL_PL"="Poland 2010","YL_PT"="Portugalia 2010", "YL_RO"="Romania 2010", "YL_SK"="Slovakia 2010", "YL_SI"="Slovenia 2010", "YL_ES"="Spain 2010",
                          "YL_SE"="Sweden 2010", "YL_UK"="United Kingdom 2010", "YL_AR"= "Argentina 2016","YL_AU"="Australia 2010", "YL_BJ"="Benin 2007", "YL_BW"="Botswana 2010",
                          "YL_BR"="Brazil 2002", "YL_BF"= "Burkina Faso 2014", "YL_KH"="Cameroon 2014", "YL_CA"="Canada 2011", "YL_CF"="Central Africa 2008","YL_TD"="Chad 2011",
                          "YL_CN"="China 2002", "YL_CO"="Colombia 2014", "YL_CR"="Costa Rica 2013", "YL_SV"="El Salvador 2010", "YL_ET"="Ethiopia 2005", "YL_GA"="Gabon 2005",
                          "YL_GM"="Gambia 2015", "YL_GH"="Ghana 2005","YL_GN"="Guinea 2012", "YL_GW"="Guinea-Bissau 2010", "YL_IN"="India 2004", "YL_ID"="Indonesia 2005",
                          "YL_JM"="Jamaica 2002", "YL_JP"="Japan 2004", "YL_KE"="Kenya 2005","YL_MV"="Maldives 2010", "YL_ML"="Mali 2005", "YL_MR"="Mauritania 2014", "YL_MX"="Mexico 2014",
                          "YL_MD"="Moldova 2014", "YL_MN"="Mongolia 2014", "YL_MZ"="Mozambique 2008","YL_NA"="Namibia 2012", "YL_NE"="Niger 2014", "YL_NG"="Nigeria 2016",
                          "YL_PE"="Peru 2014","YL_PH"="Philippines 2011","YL_RU"="Russia 2013", "YL_ST"="Sao Tome 2012","YL_SN"="Senegal 2011", "YL_SL"="Sierra Leone 2011",
                          "YL_SG"="Singapore 2013", "YL_ZA"="South Africa 2005", "YL_KR"="South Korea 2012","YL_SZ"="Swaziland 2011", "YL_TW"="Taiwan 2015", "YL_TH"="Thailand 2011",
                          "YL_TL"="Timor-Leste 2011", "YL_TR"="Turkey 2006", "YL_UY"="Uruguay 2013", "YL_US"="United States 2011", "YL_VN"="Vietnam 2008"))

rownames(NTAYLc)
rownames(NTAYLc) <- NTAYLc$age

?heatmap


NTAYLh <- t(as.matrix(NTAYLc))

colnames(NTAYLh)

YLheatmap <- heatmap(NTAYLh, Rowv=NA, Colv=NA)
?heatmap

?cm.colors

#deleting row age 
NTAYLh <- NTAYLh[-c(78),]

## sort the data by the maximum place
max <- as.numeric(max.col(NTAYLh))

## sort the data by the maximum of YL

max <- as.numeric(rowMax(NTAYLh))

NTAYLhs <- cbind(NTAYLh, max)


NTAYLhso <- NTAYLhs[order(-NTAYLhs[,87]),]

#making a list of countries in order

NTAYLhsor <- as.data.frame(NTAYLhso)
Tborder <- setDT(NTAYLhsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder

#deleting the sorting coloumn
NTAYLhso <- NTAYLhso[,-c(87)]

YLheatmap <- heatmap(NTAYLhso, Rowv=NA, Colv=NA)

####GGPLOT

#restructuring the data
YLdf <- as.data.frame(NTAYLhso)

YLd <- setDT(YLdf, keep.rownames = TRUE)[]
names(YLd)[1] <- "country"

#Reshape Data for ggplot2 plot
YLmelt <- melt(YLd)

YLmelt
str(YLmelt)

save(YLmelt, file="YLmelt.Rdata")

setwd ("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/NTA/2020")
load("YLmelt.Rdata")

#reorder the country names 

YLmelt$country <- factor(x = YLmelt$country,
                         levels = corder, 
                         ordered = TRUE)

YLtiles <- ggplot(YLmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  labs(title="",
       caption="")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

YLtiles <- ggplot(YLmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_viridis()+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  theme(axis.text.y = element_text(size = 10)) +
  labs(title="Labour income by age",
       caption="Data from Lee and Mason 2011, Istenic et al. 2019 | Plot by Lili Vargha")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")


YLtiles <- ggplot(YLmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.text.y = element_text(size = 15)) +
  labs(title="Labour income by age in 77 countries (2002-2016)",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Labour-Income_YL")+
  labs(fill = "Normalized value") +
  theme(axis.line.y=element_blank(), plot.title=element_text(size=rel(2)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles

tiff("Outputs/YLnewViz.tiff", units="in", width=14, height=16, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/GitHub/YLViz.jpg", units="in", width=14, height=16, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#######
####### Looking for clusters of countries
#######


####### First calculating dissimilarity matrix

####### Using different cluster methods

library(cluster)
library(WeightedCluster)
library(TraMineR)

library(factoextra)
library(ggplot2)
library(heatmaply)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### Dissmilarity/distance matrix, NTAYLhso

ntamatrix <- as.matrix(NTAYLhso)

str(ntamatrix)

####### Standardise the age specific variables

ntamatrix <- scale(ntamatrix)

####### Euclidean (default) and the sum of absolute distances (manhattan)

YLdismatrix <- as.matrix(daisy(ntamatrix))
YLdismatrix2 <- as.matrix(daisy(ntamatrix, metric = "manhattan"))

str(YLdismatrix)

YLdismatrix[1:5,1:5]

####### Hierarchical clustering 

cluster1 <- hclust(as.dist(YLdismatrix), 
                   method = "ward.D")

cluster2 <- hclust(as.dist(YLdismatrix2), 
                   method = "ward.D")
?hclust

#plot the dendrogram

plot(cluster1, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")

plot(cluster2, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")


wardtest1 <- as.clustrange(cluster1,
                           diss=YLdismatrix,
                           ncluster=9)

wardtest2 <- as.clustrange(cluster2,
                           diss=YLdismatrix,
                           ncluster=10)
wardtest1
wardtest2

plot(wardtest1, norm="zscore", lwd=4)


#elbow method

#fviz_nbclust(YLdismatrix, FUN = hcut, method = "wss",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")

#fviz_nbclust(YLdismatrix, FUN = hcut, method = "silhouette",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")

#fviz_nbclust(YLdismatrix2, FUN = hcut, method = "silhouette",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")


#choosing 4 clusters

cluster1.4 <- cutree(cluster1, 
                     k = 4)

cluster1.4

summary(silh.ward <- silhouette(cluster1.4, dmatrix = YLdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 3 clusters

cluster1.3 <- cutree(cluster1, 
                     k = 3)

cluster1.3

summary(silh.ward <- silhouette(cluster1.3, dmatrix = YLdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 2 clusters

cluster1.2 <- cutree(cluster1, 
                     k = 2)

cluster1.2

summary(silh.ward <- silhouette(cluster1.2, dmatrix = YLdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 6 clusters

cluster1.6 <- cutree(cluster1, 
                     k = 6)

cluster1.6

summary(silh.ward <- silhouette(cluster1.6, dmatrix = YLdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)



# 2 clusters seem the best solution for YLdismatrix


#test different cluster solutions

pam <- wcKMedRange(YLdismatrix, 
                   kvals = 2:15)

#print the quality test for different cluster solutions

pam 

#apply the PAM-clustering algorithm

pam2 <- wcKMedoids(YLdismatrix, 
                   k = 2)

pam2 <- pam2$clustering 

table(pam2)

pam2


# checking different clustering techniques: ward method seems the best
proxmat <- dist(YLdismatrix, method = 'euclidean')

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(YLdismatrix, method = x)$ac
}

map_dbl(m, ac)

#2 clusters seem the best solution for YLdismatrix: PLOT 2 clusters

cluster1.2
str(cluster1.2)
str(YLd)

str(YLmelt)


YLd$cluster2 <-  as.factor(cluster1.2)


ord <- cbind(corder,cluster1.2)
ord
str(ord)
?order
ord <-ord[order(ord[,2], decreasing = FALSE),]

corder2<-ord[,1]


YLmelt$country <- factor(x = YLmelt$country,
                         levels = corder2,
                         ordered = TRUE)

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")


YLtiles <- ggplot(YLmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.text.y = element_text(size = 15)) +
  labs(title="Labour income by age and by 2 clusters in 77 countries (2002-2016)",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Labour-Income_YL")+
  labs(fill = "Normalized value") +
  theme(axis.line.y=element_blank(), plot.title=element_text(size=rel(2)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles

tiff("Outputs/cluster/YLclusterVizv2.tiff", units="in", width=14, height=16, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()


jpeg("Outputs/GitHub/YLclusterViz.jpg", units="in", width=14, height=16, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()

#Select countries in first cluster

YLd1 <- YLd[YLd$cluster2 == "1", ] 
YLmelt1 <- melt(YLd1)

YLmelt1$country <- factor(x = YLmelt1$country,
                           levels = corder, 
                           ordered = TRUE)
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")


YLtiles <- ggplot(YLmelt1, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.text.y = element_text(size = 14)) +
  labs(title="Labour income by age in 77 countries (2002-2016)",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Labour-Income_YL")+
  labs(fill = "Normalized value") +
  theme(axis.line.y=element_blank(), plot.title=element_text(size=rel(2)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles

tiff("Outputs/YLnewVizCluster.tiff", units="in", width=10, height=6, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/YLnewVizCluster.jpg", units="in", width=10, height=6, res=500)
plot(YLtiles, align="h", rel_widths=c(1,0.2))
dev.off()


#Select countries in second cluster

YLd2 <- YLd[YLd$cluster2 == "2", ] 
YLmelt2 <- melt(YLd2)

YLmelt2$country <- factor(x = YLmelt2$country,
                          levels = corder, 
                          ordered = TRUE)
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")


YLtiles2 <- ggplot(YLmelt2, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.text.y = element_text(size = 10)) +
  labs(title="",
       subtitle=paste0(""),
       caption="")+
  theme(axis.line.y=element_blank(), plot.title=element_text(size=rel(2)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
YLtiles2

jpeg("Outputs/cluster/VizYlcluster2.png", units="in", width=14, height=16, res=500)
plot_grid(YLtiles2, align="h", rel_widths=c(1,0.2))
dev.off()

YLmelt <- melt(YLd)
YLmelt

ggplot(data=YLmelt, aes(x=variable, y=value, group=country))+
  geom_line()


# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   
# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   

age<- as.numeric(c("0", "10", "20","30", "40", "50", "60", "70", "80"))            

YLmelt$variable <- as.numeric(YLmelt$variable)
fig <- ggplot(data=YLmelt, aes(x=variable, y=value, group=country, color=cluster2)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("red", "#00AFBB"), name  ="2 Clusters",
                       breaks=c("1", "2"),
                       labels=c("YL max at later ages, high peak, higher old age (N=19)", "YL max at younger ages, longer plateau type (N=58)")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age)) +
  labs(title="Labour Income Age Profile Clusters (N=77)",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Labour-Income_YL") +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 2)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(name="Normalized Value", limits=c(0,1.5) )

fig 

jpeg("Outputs/GitHub/ClusterYL.jpg", units="in", width=10, height=6, res=500)
plot(fig, align="h", rel_widths=c(1,0.2))
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


####
#### Calculating average age profiles and calculating country differences to this age profile
#### Idea proposed by Timothy Miller (UN)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# YL dif

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colMeans(NTAYLh)
AVG <- colMeans(NTAYLh)
NTAYLdif <- sweep(NTAYLh,2,AVG)

YLdifdf <- as.data.frame(NTAYLdif)

YLdifdf2 <- setDT(YLdifdf, keep.rownames = TRUE)[]
names(YLdifdf2)[1] <- "country"

##maximum and minimum values in the matrix
maxYLdif <- max(NTAYLdif)
minYLdif <- min(NTAYLdif)

maxYLdif
minYLdif

#Reshape Data for ggplot2 plot
YLdifmelt <- melt(YLdifdf2)

YLdifmelt
str(YLdifmelt)

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

YLdiftiles <- ggplot(YLdifmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  labs(title="YL difference to the mean",
       caption="Data from Lee and Mason 2011, Istenic et al. 2019")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))  
YLdiftiles

NTAYLdifs <- cbind(NTAYLdif, max)

NTAYLdifso <- NTAYLdifs[order(-NTAYLdifs[,87]),]

#making a list of countries in order

NTAYLdifsor <- as.data.frame(NTAYLdifso)
Tborder <- setDT(NTAYLdifsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder

#deleting the sorting coloumn
NTAYLdifso <- NTAYLdifso[,-c(87)]

YLdifeatmap <- heatmap(NTAYLdifso, Rowv=NA, Colv=NA)

####GGPLOT

#restructuring the data
YLdf <- as.data.frame(NTAYLdifso)

YLd <- setDT(YLdf, keep.rownames = TRUE)[]
names(YLd)[1] <- "country"


#Reshape Data for ggplot2 plot
YLdifmelt_o <- melt(YLd)

#reorder the country names 

YLdifmelt_o$country <- factor(x = YLdifmelt_o$country,
                              levels = corder2, 
                              ordered = TRUE)


YLdiftiles_o <- ggplot(YLdifmelt_o, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  labs(title="Country specific differences to the average age specific labour income ordered by clusters",
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Labour-Income_YL")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))  
YLdiftiles_o


jpeg("Outputs/GitHub/YLdiftiles_o.jpg", units="in", width=14, height=16, res=500)
plot_grid(YLdiftiles_o, align="h", rel_widths=c(1,0.2))
dev.off()  


# LCD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NTA <- cbind (nta2, ntaeu)
class(NTA)


### selecting only LCD variables

LCDvar <- c("LCD_AT", "LCD_BE", "LCD_BG", "LCD_CY", "LCD_CZ", "LCD_DK", "LCD_EE", "LCD_FI", "LCD_FR",
            "LCD_DE", "LCD_GR", "LCD_HU", "LCD_IE", "LCD_IT", "LCD_LV", "LCD_LT", "LCD_LU", "LCD_PL",
            "LCD_PT", "LCD_RO", "LCD_SK", "LCD_SI", "LCD_ES", "LCD_SE", "LCD_UK", "LCD_AR", "LCD_AU",
            "LCD_BJ", "LCD_BW", "LCD_BR", "LCD_BF", "LCD_KH", "LCD_CA", "LCD_CF", "LCD_TD", "LCD_CN",
            "LCD_CO", "LCD_CR", "LCD_SV", "LCD_ET", "LCD_GA", "LCD_GM", "LCD_GH", "LCD_GN", "LCD_GW",
            "LCD_IN", "LCD_ID", "LCD_JM", "LCD_JP", "LCD_KE", "LCD_MV", "LCD_ML", "LCD_MR", "LCD_MX",
            "LCD_MD", "LCD_MN", "LCD_MZ", "LCD_NA", "LCD_NE", "LCD_NG", "LCD_PE", "LCD_PH", "LCD_RU",
            "LCD_ST", "LCD_SN", "LCD_SL", "LCD_SG", "LCD_ZA", "LCD_KR", "LCD_SZ", "LCD_TW", "LCD_TH",
            "LCD_TL", "LCD_TR", "LCD_UY", "LCD_US", "LCD_VN", "age")

NTALCD <- NTA[LCDvar]

colnames(NTALCD)

countries <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
               "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
               "Poland", "Portugalia", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom", "Argentina",
               "Australia", "Benin", "Botswana", "Brazil", "Burkina Faso", "Cameroon", "Canada", "Central Africa",
               "Chad", "China", "Colombia", "Costa Rica", "El Salvador", "Ethiopia", "Gabon", "Gambia", "Ghana",
               "Guinea", "Guinea-Bissau", "India", "Indonesia", "Jamaica", "Japan", "Kenya", "Maldives", "Mali",
               "Mauritania", "Mexico", "Moldova", "Mongolia", "Mozambique", "Namibia", "Niger", "Nigeria", "Peru",
               "Philippines", "Russia", "Sao Tome", "Senegal", "Sierra Leone", "Singapore", "South Africa", "South Korea",
               "Swaziland", "Taiwan", "Thailand", "Timor-Leste", "Turkey", "Uruguay", "United States", "Vietnam")

names(NTALCD)

NTALCDc <- rename(NTALCD, c("LCD_AT"="Austria 2010", "LCD_BE"="Belgium 2010","LCD_BG"="Bulgaria 2010", "LCD_CY"="Cyprus 2010", "LCD_CZ"="Czech Republic 2010",
                            "LCD_DK"="Denmark 2010", "LCD_EE"="Estonia 2010", "LCD_FI"="Finland 2010", "LCD_FR"="France 2010", "LCD_DE"="Germany 2010", "LCD_GR"="Greece 2010",
                            "LCD_HU"="Hungary 2010","LCD_IE"= "Ireland 2010", "LCD_IT"="Italy 2010", "LCD_LV"="Latvia 2010", "LCD_LT"="Lithuania 2010", "LCD_LU"="Luxembourg 2010",
                            "LCD_PL"="Poland 2010","LCD_PT"="Portugalia 2010", "LCD_RO"="Romania 2010", "LCD_SK"="Slovakia 2010", "LCD_SI"="Slovenia 2010", "LCD_ES"="Spain 2010",
                            "LCD_SE"="Sweden 2010", "LCD_UK"="United Kingdom 2010", "LCD_AR"= "Argentina 2016","LCD_AU"="Australia 2010", "LCD_BJ"="Benin 2007", "LCD_BW"="Botswana 2010",
                            "LCD_BR"="Brazil 2002", "LCD_BF"= "Burkina Faso 2014", "LCD_KH"="Cameroon 2014", "LCD_CA"="Canada 2011", "LCD_CF"="Central Africa 2008","LCD_TD"="Chad 2011",
                            "LCD_CN"="China 2002", "LCD_CO"="Colombia 2014", "LCD_CR"="Costa Rica 2013", "LCD_SV"="El Salvador 2010", "LCD_ET"="Ethiopia 2005", "LCD_GA"="Gabon 2005",
                            "LCD_GM"="Gambia 2015", "LCD_GH"="Ghana 2005","LCD_GN"="Guinea 2012", "LCD_GW"="Guinea-Bissau 2010", "LCD_IN"="India 2004", "LCD_ID"="Indonesia 2005",
                            "LCD_JM"="Jamaica 2002", "LCD_JP"="Japan 2004", "LCD_KE"="Kenya 2005","LCD_MV"="Maldives 2010", "LCD_ML"="Mali 2015", "LCD_MR"="Mauritania 2014", "LCD_MX"="Mexico 2014",
                            "LCD_MD"="Moldova 2014", "LCD_MN"="Mongolia 2014", "LCD_MZ"="Mozambique 2008","LCD_NA"="Namibia 2012", "LCD_NE"="Niger 2014", "LCD_NG"="Nigeria 2016",
                            "LCD_PE"="Peru 2014","LCD_PH"="Philippines 2011","LCD_RU"="Russia 2013", "LCD_ST"="Sao Tome 2012","LCD_SN"="Senegal 2011", "LCD_SL"="Sierra Leone 2011",
                            "LCD_SG"="Singapore 2013", "LCD_ZA"="South Africa 2005", "LCD_KR"="South Korea 2012","LCD_SZ"="Swaziland 2011", "LCD_TW"="Taiwan 2015", "LCD_TH"="Thailand 2011",
                            "LCD_TL"="Timor-Leste 2011", "LCD_TR"="Turkey 2006", "LCD_UY"="Uruguay 2013", "LCD_US"="United States 2011", "LCD_VN"="Vietnam 2008"))

rownames(NTALCDc)
rownames(NTALCDc) <- NTALCDc$age

NTALCDh <- t(as.matrix(NTALCDc))

colnames(NTALCDh)

LCDheatmap <- heatmap(NTALCDh, Rowv=NA, Colv=NA)

#deleting row age 
NTALCDh <- NTALCDh[-c(78),]


##maximum and minimum values in the matrix
maxLCD <- max(NTALCDh)
minLCD <- min(NTALCDh)

maxLCD
minLCD
####Sorting: three different versions

#1. Sort the data by the maximum place
#max <- as.numeric(max.col(NTALCDh))
#NTALCDhs <- cbind(NTALCDh, max)
#NTALCDhso <- NTALCDhs[order(-NTALCDhs[,87]),]


#2. sort the data according to when values turn negative
neg <- as.numeric(max.col(NTALCDh < 0,ties.method = "first"))
neg

#replace value of El Salvador
neg <- replace(neg, neg==1, 80)
neg

#3. sort the data according to how many ages have negative values
#neg <- as.numeric(rowMeans(NTALCDh < 0))
#neg

NTALCDhs <- cbind(NTALCDh, neg)

NTALCDhso <- NTALCDhs[order(-NTALCDhs[,87]),]

#making a list of countries in order

NTALCDhsor <- as.data.frame(NTALCDhso)
Tborder <- setDT(NTALCDhsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder



#deleting the sorting coloumn
NTALCDhso <- NTALCDhso[,-c(87)]

LCDheatmap <- heatmap(NTALCDhso, Rowv=NA, Colv=NA)


#restructuring the data
LCDdf <- as.data.frame(NTALCDhso)

LCDd <- setDT(LCDdf, keep.rownames = TRUE)[]
names(LCDd)[1] <- "country"

####### Dissmilarity/distance matrix, NTALCDhso

ntamatrix <- as.matrix(NTALCDhso)

str(ntamatrix)

####### Standardise the age specific variables

ntamatrix <- scale(ntamatrix)

####### Euclidean (default) and the sum of absolute distances (manhattan)

LCDdismatrix <- as.matrix(daisy(ntamatrix))
LCDdismatrix2 <- as.matrix(daisy(ntamatrix, metric = "manhattan"))

str(LCDdismatrix)

LCDdismatrix[1:5,1:5]

####### Hierarchical clustering 

cluster1 <- hclust(as.dist(LCDdismatrix), 
                   method = "ward.D")

cluster2 <- hclust(as.dist(LCDdismatrix2), 
                   method = "ward.D")
?hclust

#plot the dendrogram

plot(cluster1, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")

plot(cluster2, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")


wardtest1 <- as.clustrange(cluster1,
                           diss=LCDdismatrix,
                           ncluster=9)

wardtest2 <- as.clustrange(cluster2,
                           diss=LCDdismatrix,
                           ncluster=10)
wardtest1
wardtest2

plot(wardtest1, norm="zscore", lwd=4)



#choosing 4 clusters

cluster1.4 <- cutree(cluster1, 
                     k = 4)

cluster1.4

summary(silh.ward <- silhouette(cluster1.4, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 3 clusters

cluster1.3 <- cutree(cluster1, 
                     k = 3)

cluster1.3

summary(silh.ward <- silhouette(cluster1.3, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 2 clusters

cluster1.2 <- cutree(cluster1, 
                     k = 2)

cluster1.2

summary(silh.ward <- silhouette(cluster1.2, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 8 clusters

cluster1.8 <- cutree(cluster1, 
                     k = 8)

cluster1.8

summary(silh.ward <- silhouette(cluster1.8, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)


#4 clusters seem the best solution for LCDdismatrix


#test different cluster solutions

pam <- wcKMedRange(LCDdismatrix, 
                   kvals = 2:15)

#print the quality test for different cluster solutions

pam 

#apply the PAM-clustering algorithm

pam2 <- wcKMedoids(LCDdismatrix, 
                   k = 2)

pam2 <- pam2$clustering 

table(pam2)

pam2


# checking different clustering techniques: ward method seems the best
proxmat <- dist(LCDdismatrix, method = 'euclidean')

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(LCDdismatrix, method = x)$ac
}

map_dbl(m, ac)

#4 clusters seem the best solution for LCDdismatrix: PLOT 4 clusters

cluster1.4
str(cluster1.4)
str(LCDd)

LCDd$cluster4 <-  as.factor(cluster1.4)

LCDd$cluster2 <-  as.factor(cluster1.2)

#Select countries in first cluster

LCDd1 <- LCDd[LCDd$cluster4 == "1", ] 
LCDmelt1 <- melt(LCDd1)

LCDmelt1$country <- factor(x = LCDmelt1$country,
                           levels = corder, 
                           ordered = TRUE)

bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

LCDtiles1 <- ggplot(LCDmelt1, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.29, 1.29)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalised value") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCDtiles1 

#Select countries in second cluster

LCDd2 <- LCDd[LCDd$cluster4 == "2", ] 
LCDmelt2 <- melt(LCDd2)

LCDmelt2$country <- factor(x = LCDmelt2$country,
                           levels = corder, 
                           ordered = TRUE)



LCDtiles2 <- ggplot(LCDmelt2, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.29, 1.29)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalised value") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCDtiles2 

#Select countries in third cluster

LCDd3 <- LCDd[LCDd$cluster4 == "3", ] 
LCDmelt3 <- melt(LCDd3)

LCDmelt3$country <- factor(x = LCDmelt3$country,
                           levels = corder, 
                           ordered = TRUE)

LCDtiles3 <- ggplot(LCDmelt3, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.29, 1.29)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalised value") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCDtiles3

#Select countries in fourth cluster

LCDd4 <- LCDd[LCDd$cluster4 == "4", ] 
LCDmelt4 <- melt(LCDd4)

LCDmelt4$country <- factor(x = LCDmelt4$country,
                           levels = corder, 
                           ordered = TRUE)

LCDtiles4 <- ggplot(LCDmelt4, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.29, 1.29)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalised value") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCDtiles4

#### Plotting colourful spaghetti figure



LCDmelt <- melt(LCDd)
LCDmelt

ggplot(data=LCDmelt, aes(x=variable, y=value, group=country))+
  geom_line()

LCDmelt$variable <- as.numeric(LCDmelt$variable)

age<- as.numeric(c("0", "10", "20","30", "40", "50", "60", "70", "80"))           

fig <- ggplot(data=LCDmelt, aes(x=variable, y=value, group=country, color=cluster4)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("#E69F00", "red", "#00AFBB", "blue"), name ="4 Clusters",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("Highest deficit in young and old age, lowest surplus at working age (N=8)", "Medium deficit in young and old age, medium surplus at working age (N=15)", "Higher deficit in young and old age,  medium surplus at working age (N=40)", "Lowest deficit in young and old age, and highest, longest surplus at working age (N=14)")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age)) +
  
  labs(title="Life Cycle Deficit Age Profile Clusters (N=77)",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Life-Cycle-Deficit_LCD")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 4)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(name="Normalized Value")
  #geom_hline(yintercept=0, colour="black")

fig 

jpeg("Outputs/GitHub/ClusterLCD.jpg", units="in", width=8.2, height=6, res=500)
plot(fig, align="h", rel_widths=c(1,0.2))
dev.off()


cluster1.4

ord <- cbind(corder,cluster1.4)
ord
str(ord)
?order
ord <-ord[order(ord[,2], decreasing = FALSE),]

corder2<-ord[,1]

LCDmelt <- melt(LCDd)

LCDmelt$country <- factor(x = LCDmelt$country,
                         levels = corder2,
                         ordered = TRUE)


bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

LCDtiles <- ggplot(LCDmelt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.29, 1.29)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 85, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalised value") +
  labs(title="Life Cycle Deficit by age in 77 countries",
       subtitle=paste0(""),
       caption="Data from Lee and Mason 2011, Istenic et al. 2019
       Replication files & details: https://github.com/LiliVargha/Life-Cycle-Deficit_LCD")+
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCDtiles 


jpeg("Outputs/GitHub/VizLcd.jpg", units="in", width=14, height=16, res=500)
plot_grid(LCDtiles, align="h", rel_widths=c(1,0.2))
dev.off()


