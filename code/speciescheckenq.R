require('ggplot2')
require('broom')
require('tidyverse')
require('dplyr')
require('data.table')

rm(list=ls())
graphics.off()
setwd("/home/primuser/Documents/RVK/code/")


milesdata <- read.csv('../data/Nesbit.csv')


vecdata <- read.csv('../data/Vtraits.csv')

enqvist <- read.csv('../data/enqvist.csv')
enqvist2 <- read.csv('../data/enqvist2.csv')

y2k <- read.csv('../../mhasoba-biotraitsdb-49600802636e/data/GlobalDataset_v0.5.csv')
y2k2 <- as.data.frame(paste(y2k$interactor1genus, y2k$interactor2species))

nesspecies <- read.csv('../data/5Total_processed_Log_RVK_3_models.csv')


nosmithhdata <- dplyr::filter(milesdata, Citation != 'Smith_et_al')

#species
numberofstrainsVEC <- unique(y2k$interactor1)
numberofstrainsNES <- unique(milesdata$Species)
numberofstrainsENQ <- unique(enqvist$organism)
numberofstrainsNES2 <- unique(nesspecies$spp_species)
numberofstrainsENQ2 <- unique(enqvist2$organism)


#do I need labels? Yes.
ENQ <- as.data.frame(numberofstrainsENQ2)
ENQ <- ENQ %>% dplyr::rename(species = numberofstrainsENQ2)
NES <- as.data.frame(numberofstrainsNES2)
NES <- NES %>% dplyr::rename(species = numberofstrainsNES2)
VEC <- as.data.frame(numberofstrainsVEC)
VEC <- VEC %>% dplyr::rename(species = numberofstrainsVEC)


#clean out weird parenthesis in vec and clean out clones/strains
VEC <- gsub("\\s*\\([^\\)]+\\)","",as.character(VEC$species))
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)
VEC <- gsub("strain", "@", as.character(VEC$species))
VEC <- sub('([^@]+\\@).*', '\\1', VEC)
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)
VEC <- gsub("clone", "@", as.character(VEC$species))
VEC <- sub('([^@]+\\@).*', '\\1', VEC)
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)
VEC <- gsub("cult", "@", as.character(VEC$species))
VEC <- sub('([^@]+\\@).*', '\\1', VEC)
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)
VEC<-gsub("_", " ", VEC$species, fixed=TRUE)
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)
VEC<-gsub("@", " ", VEC$species, fixed=TRUE)
VEC <- unique(VEC)
VEC <- as.data.frame(VEC)
VEC <- VEC %>% dplyr::rename(species = VEC)

ENQ2 <- ENQ

str(ENQ2)
ENQ2<- data.frame(lapply(ENQ2, as.character), stringsAsFactors=FALSE)

VECNES <- rbind(NES, VEC)
str(VECNES)
VECNES2 <- data.frame(lapply(VECNES, as.character), stringsAsFactors=FALSE)

#control for all variables
f <- apply(ENQ2,2,toupper)
f <- gsub(" ", "", f, fixed = TRUE)
f <- as.data.frame(f)
e <- apply(VECNES2,2,toupper)
e <- gsub(" ", "", e, fixed = TRUE)
e <- as.data.frame(e)

#score the similarity
data3 <- f %>%
  mutate(score = if_else(f$species %in% e$species , 1, 0))
sum(data3$score)

#try it in genus
#genus
genusNES2 <- unique(nesspecies$spp_genus)
genusENQ2 <- unique(enqvist2$genus)
genusVEC <- unique(y2k$interactor1genus)

#still need labels
gENQ <- as.data.frame(genusENQ2)
gENQ <- gENQ %>% dplyr::rename(genus = genusENQ2)
gNES <- as.data.frame(genusNES2)
gNES <- gNES %>% dplyr::rename(genus = genusNES2)
gVEC <- as.data.frame(genusVEC)
gVEC <- gVEC %>% dplyr::rename(genus = genusVEC)

gENQ2 <- gENQ

str(gENQ2)
gENQ2<- data.frame(lapply(gENQ2, as.character), stringsAsFactors=FALSE)

gVECNES <- rbind(gNES, gVEC)
str(gVECNES)
gVECNES2 <- data.frame(lapply(gVECNES, as.character), stringsAsFactors=FALSE)

#control for all variables
gf <- apply(gENQ2,2,toupper)
gf <- gsub(" ", "", gf, fixed = TRUE)
gf <- as.data.frame(gf)
ge <- apply(gVECNES2,2,toupper)
ge <- gsub(" ", "", ge, fixed = TRUE)
ge <- as.data.frame(ge)


#score the similarity
gdata3 <- gf %>%
  mutate(score = if_else(gf$genus %in% ge$genus , 1, 0))

sum(gdata3$score)

