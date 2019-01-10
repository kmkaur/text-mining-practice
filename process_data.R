#trying to process text-mined data

#set your working directory here
setwd("~/Documents/UBC PhD/Networks")

practice_species <- read.csv("test_species2.csv", header=FALSE)

ant_list <- read.csv("worldants_specieslist.csv", header=TRUE)
ant_list <- as.data.frame(ant_list$current.valid.name)
colnames(ant_list) <- "ants"
levels(ant_list$ants) <- tolower(levels(ant_list$ants))

angiosperms_list <- as.data.frame(read.csv("all_angiosperms.csv", header = TRUE, stringsAsFactors = FALSE))
colnames(angiosperms_list) <- "plants"

practice_species2 <- practice_species[practice_species$V1 %in% practice_species$V1[duplicated(practice_species$V1)],]

library(dplyr)

ants_tm <- practice_species2 %>% filter(V2 %in% ant_list$ants)
ants_tm <- remove.factors(ants_tm)
plants_tm <- practice_species2 %>% filter(V2%in% angiosperms_list$plants)
plants_tm <- remove.factors(plants_tm)

together <- merge(ants_tm, plants_tm, by="V1")
colnames(together) <- c("abs", "ants", "plants")
together$abs <- NULL
final_table <- table(together)
final_table <- (final_table > 0) + 0
                           
#mat <- matrix(NA, nrow = 8, ncol = 4, dimnames = list(c(ants_tm$V2), c(plants_tm$V2)))
