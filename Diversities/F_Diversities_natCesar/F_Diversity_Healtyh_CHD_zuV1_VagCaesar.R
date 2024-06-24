library("dplyr")
library(readxl)
library(vegan)
library(ggplot2)
library(Hmisc)

options(scipen = 10)

# [Set the working directory]
setwd("Q:/NAS/IMiC/Auswertung_Datenanalyse/DatenAnalyseMitR")

# [Load genus abundance data]
Genus_Data <- read.csv("A_Daten/20_OutPut_OutTables/Genus_Data.csv", sep=";")
head(Genus_Data)
Genus_Data[,1]

# Normalisieren [Nomalize Data]
Genus_Data_norm      <- Genus_Data
Genus_Data_norm[,-1] <- Genus_Data_norm[,-1]/rowSums(Genus_Data_norm[,-1])

# Entfernen der MOC-Daten [Get rid of MOC data]
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
C_Data_norm

# Cluster-Bestandteile werden mit dem Geburtsweg gejoint und erhalten 
# UEBER DIE REIHENFOLGE den Geburtsweg
# [Join data with birth way info]
cesarean.info <- 
  read.table("A_Daten\\20_OutPut_OutTables\\KaiserSchnitt.csv", sep = ";", header = TRUE)
colnames(cesarean.info)[1] <- "SHORTID"
head(cesarean.info)
tail(cesarean.info)

short_names <- function(x){unlist(strsplit(x,"_"))[1]}
C_Data_norm$SHORTID <- sapply(C_Data_norm$ID, short_names)
P_Data_norm$SHORTID <- sapply(P_Data_norm$ID, short_names)

# Check
tmp <- which(C_Data_norm$SHORTID %nin% cesarean.info$SHORTID)
if(length(tmp) > 0){
  print("Achtung, Info zum Geburtsweg fehlt!")
  C_Data_norm$SHORTID[tmp]
}
tmp <- which(P_Data_norm$SHORTID %nin% cesarean.info$SHORTID)
if(length(tmp) > 0){
  print("Achtung, Info zum Geburtsweg fehlt!")
  P_Data_norm$SHORTID[tmp]
}

# mergen der Daten
# [merge data]
dim(C_Data_norm)
C_Data_norm_merged <- merge(C_Data_norm,cesarean.info)
dim(C_Data_norm_merged)

dim(P_Data_norm)
P_Data_norm_merged <- merge(P_Data_norm,cesarean.info)
dim(P_Data_norm_merged)
colnames(P_Data_norm_merged)
nonFeatures <- which(colnames(P_Data_norm_merged) %in% c("SHORTID","ID","Cesarean"))

# [calculate diversities]
diversity_results <- 
  data.frame(
    group = c(rep("Controls",dim(C_Data_norm_merged)[1]),
               rep("Patients",dim(P_Data_norm_merged)[1])),
    typeOfBirth = c(C_Data_norm_merged$Cesarean,
                    P_Data_norm_merged$Cesarean),
    id    = c(C_Data_norm_merged$ID,
              P_Data_norm_merged$ID),
    shannon = c(vegan::diversity(C_Data_norm_merged[,-nonFeatures], index = "shannon"),
                vegan::diversity(P_Data_norm_merged[,-nonFeatures], index = "shannon")),
    simpson = c(vegan::diversity(C_Data_norm_merged[,-nonFeatures], index = "simpson"),
                vegan::diversity(P_Data_norm_merged[,-nonFeatures], index = "simpson")),
    invsimpson = c(vegan::diversity(C_Data_norm_merged[,-nonFeatures], index = "invsimpson"),
                vegan::diversity(P_Data_norm_merged[,-nonFeatures], index = "invsimpson"))
    )
head(diversity_results)
diversity_results$typeOfBirth <- ifelse(diversity_results$typeOfBirth == 0,"natural","cesarean")

rsh_diversity_results <- 
  reshape2::melt(diversity_results,
                 id.vars = c("id","group","typeOfBirth"),
                 measure.vars = c("shannon",    "simpson", "invsimpson"))
head(rsh_diversity_results)

# Design 2
ggplot(data = rsh_diversity_results, aes(x=group)) +
  geom_boxplot(aes(y=value)) +
  facet_wrap(~variable+typeOfBirth, scales = "fixed", nrow = 3) +
  ylim(c(0,5))
  ggtitle(
    label = "Diversities for Patients and Controls by type of birth. All genera.")

ggsave("Y:\\Auswertung_Datenanalyse\\DatenAnalyseMitR\\F_Diversities_natCesar\\SupplememntFigure_C_Diversities_ContrPat_WayOfBirth_AllGenera_limitedYaxis.jpeg",
       dpi=600)

ggplot(data = rsh_diversity_results, aes(x=typeOfBirth)) +
  geom_boxplot(aes(y=value)) +
  facet_wrap(~variable, scales = "fixed") +
  ylim(c(0,5)) +
  ggtitle(
    label = "Diversities by type of birth. All genera.") +
  xlab("Type of Birth")
ggsave("Y:\\Auswertung_Datenanalyse\\DatenAnalyseMitR\\F_Diversities_natCesar\\SupplememntFigure_B_Diversities_WayOfBirth_AllGenera_limitedYaxix.jpeg",
       dpi=800)

# Daten in Datei schreiben
# [write results to file]
out <- cbind(aggregate(value ~ group + typeOfBirth + variable, data = rsh_diversity_results, FUN = median),
             aggregate(value ~ group + typeOfBirth + variable, data = rsh_diversity_results, FUN = IQR))
out <- out[,c(1,2,3,4,8)]
colnames(out)[c(4,5)] <- c("median","IQR")
library(writexl)
write_xlsx(x = out,
           "Q:\\NAS\\IMiC\\Auswertung_Datenanalyse\\DatenAnalyseMitR\\F_Diversities_natCesar\\Data_To_SupplememntFigure_B_Diversities_WayOfBirth_AllGenera.xlsx")

# [session info]
packageVersion("dplyr")
packageVersion("readxl")
packageVersion("vegan")
packageVersion("ggplot2")
packageVersion("Hmisc")
packageVersion("writexl")

citation()
citation("dplyr")
citation("readxl")
citation("vegan")
citation("ggplot2")
citation("Hmisc")
citation("writexl")
