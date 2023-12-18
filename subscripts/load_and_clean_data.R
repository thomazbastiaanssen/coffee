#Statistical tools        Primarily PERMANOVA, alpha diversity and the CLR transformation.
library(vegan)            #install.packages("vegan")
library(iNEXT)            #install.packages("iNEXT")
library(Tjazi)            #devtools::install_github("thomazbastiaanssen/Tjazi")
library(lme4)
library(lmerTest)

#Data Wrangling
library(tidyverse)        #install.packages("tidyverse")
library(knitr)            #install.packages("knitr")
library(waldo)            #install.packages("waldo")

#Plotting
library(ggplot2)          #install.packages("ggplot2")
library(ggforce)          #install.packages("ggforce")
library(patchwork)        #install.packages("patchwork")
library(ggbeeswarm)       #install.packages("ggbeeswarm")
library(metafolio)        #install.packages("metafolio")
library(scales)


#Disable strings automatically being read in as factors to avoid unintuitive behaviour.
options(stringsAsFactors = F)

#Set a seed for the purposes of reproducibility in this document.
set.seed(1)

library(broom)
library(broom.mixed)

#Set contrasts for type-3 sum of squares 
options(contrasts = c("contr.treatment","contr.poly"))

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

std <- function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x)))
#Load in the species level count table and the metadata file. 
counts   <- read.delim("raw/microbiome/species.tsv", sep = "\t", row.names = 1, header = T)
row.names(counts) <- counts$Name

metabs  <- read.delim("raw/metabolome/metabs.csv",  sep = ",", row.names = 1, header = T)
GBMs <- read.delim("raw/microbiome/GBMs_coffee.csv", sep = ",", row.names = 3)[,-c(1:2)]
GMMs <- read.delim("raw/microbiome/GMMs_coffee.csv", sep = ",", row.names = 3)[,-c(1:2)]

alpha_div <- read.delim("raw/microbiome/alpha_div.csv", sep = ",", row.names = 1)



metadata <- read.delim("raw/keys/metadata_coffee_metab.csv", sep = ",")

metadata = metadata %>% 
  group_by(participant_ID, visit) %>% 
  add_tally() %>% 
  ungroup() %>% 
  filter(n == 1 | n == 2 & batch == "Batch_2") %>% 
  dplyr::select(!n) 

alpha_div = alpha_div[metadata$R_ID,] %>% 
  rownames_to_column("rownames") %>% 
  left_join(.,y = metadata, by = c("rownames"="R_ID")) %>% 
  column_to_rownames("rownames")


counts = counts[,metadata$R_ID]

GBMs <- GBMs[,metadata$R_ID]
GMMs <- GMMs[,metadata$R_ID]

GBMs.exp <- clr_c(GBMs)
GMMs.exp <- clr_c(GMMs)

metabs = t(metabs[metadata$Metab_ID[metadata$Metab_ID != ""],])
metabs.exp <- clr_c(metabs)

metab_trans <- read.delim("raw/keys/metab_translation.csv", sep = ",", header = T)

#Fork off your count data so that you always have an untouched version handy.
species   <- counts

#make sure our count data is all numbers
species   <- apply(species,c(1,2),function(x) as.numeric(as.character(x)))

#Remove features with prevalence < 10% in two steps:
#First, determine how often every feature is absent in a sample
n_zeroes <- rowSums(species == 0)

#Then, remove features that are absent in more than your threshold (90% in this case).
species    <- species[n_zeroes <= round(ncol(species) * 0.60),]

#CLR transform
species.exp <- deleuze::getTableMeans(species)

