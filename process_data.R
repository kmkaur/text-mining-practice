#trying to process text-mined data

#set your working directory here
setwd("~/Desktop")

practice_species <- read.csv("test_species.csv", header=FALSE)

ant_list <- read.csv("worldants_specieslist.csv", header=TRUE)
ant_list <- as.data.frame(ant_list$current.valid.name)
colnames(ant_list) <- "ants"

angiosperms_list <- read.csv("angiosperms.csv", header = TRUE)

