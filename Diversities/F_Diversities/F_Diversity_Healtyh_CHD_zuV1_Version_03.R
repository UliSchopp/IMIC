library("dplyr")
library(readxl)
library(vegan)
library(ggplot2)


options(scipen = 10)

setwd("Y:/Auswertung_Datenanalyse/DatenAnalyseMitR")
Genus_Data <- read.csv("A_Daten/20_OutPut_OutTables/Genus_Data.csv", sep=";")
head(Genus_Data)
Genus_Data[,1]

# Normalisieren
# [normalize data]
Genus_Data_norm      <- Genus_Data
Genus_Data_norm[,-1] <- Genus_Data_norm[,-1]/rowSums(Genus_Data_norm[,-1])

# Entfernen der MOC-Daten
# [Get rid of MOC data]
tmp <- grep("MOC",Genus_Data_norm$ID)
if(length(tmp) > 0){
  Genus_Data_norm <- Genus_Data_norm[-tmp,]
}
# Selektion der Daten zu V1 => nur Patienten!
# [Select patients data at first visit]
tmp <- grep("V1",Genus_Data_norm$ID)
if(length(tmp) > 0){
  P_Data_norm <- Genus_Data_norm[tmp,]
}
P_Data_norm
# Selektion der Daten zu Kontrollen!
# [Select data of control group.]
tmp <- grep("C",Genus_Data_norm$ID)
if(length(tmp) > 0){
  C_Data_norm <- Genus_Data_norm[tmp,]
}


diversity_results <- 
  data.frame(
    group = c(rep("Controls",dim(C_Data_norm)[1]),
               rep("Patients",dim(P_Data_norm)[1])),
    id    = c(C_Data_norm$ID,P_Data_norm$ID),
    shannon = c(vegan::diversity(C_Data_norm[,-1], index = "shannon"),
                vegan::diversity(P_Data_norm[,-1], index = "shannon")),
    simpson = c(vegan::diversity(C_Data_norm[,-1], index = "simpson"),
                vegan::diversity(P_Data_norm[,-1], index = "simpson")),
    invsimpson = c(vegan::diversity(C_Data_norm[,-1], index = "invsimpson"),
                vegan::diversity(P_Data_norm[,-1], index = "invsimpson"))
    )
head(diversity_results)

rsh_diversity_results <- reshape2::melt(diversity_results)

head(rsh_diversity_results)
# Design 2
ggplot(data = rsh_diversity_results, aes(x=group)) +
  geom_boxplot(aes(y=value)) +
  ylim(c(0,5)) +
  facet_wrap(~variable, scales = "fixed") +
  ggtitle(
    label = "Diversities for Patients and Controls. All genera.")

ggsave("Y:\\Auswertung_Datenanalyse\\DatenAnalyseMitR\\F_Diversities\\SupplememntFigure_A_Diversities_ContrPat_AllGenera_limitedYaxis.jpeg")

