#trying to process text-mined data

#set your working directory here
setwd("~/Desktop")

practice_species <- read.csv("test_species.csv", header=FALSE)

ant_list <- read.csv("worldants_specieslist.csv", header=TRUE)
ant_list <- as.data.frame(ant_list$current.valid.name)
colnames(ant_list) <- "ants"
levels(ant_list$ants) <- tolower(levels(ant_list$ants))

angiosperms_list <- as.data.frame(read.csv("angiosperms.csv", header = TRUE))
colnames(angiosperms_list) <- "plants"
levels(angiosperms_list$plants) <- tolower(levels(angiosperms_list$plants))

practice_species2 <- practice_species[practice_species$V1 %in% practice_species$V1[duplicated(practice_species$V1)],]

library(dplyr)
practice_species2 %>%
  filter(V2%in% ant_list$ants)

practice_species2 %>%
  filter(V2%in% angiosperms_list$plants)
