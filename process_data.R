#trying to process text-mined data

#load packages
library(dplyr)
library(taRifx)

#set working directory
setwd("~/Desktop")

#####load list of ants and plants to use in sorting the sd, efn, dom lists#####
#ants
ant_list <- read.csv("worldants_specieslist.csv", header=TRUE)
ant_list <- as.data.frame(ant_list$current.valid.name)
colnames(ant_list) <- "ants"
levels(ant_list$ants) <- tolower(levels(ant_list$ants))

#plants
angiosperms_list <- as.data.frame(read.csv("all_angiosperms.csv", header = TRUE, stringsAsFactors = FALSE))
colnames(angiosperms_list) <- "plants"

#####test file - it worked with this#####
practice_species <- read.csv("test_species2.csv", header=FALSE)

#remove abstracts that had only one species appear
practice_species2 <- practice_species[practice_species$V1 %in% practice_species$V1[duplicated(practice_species$V1)],]

#sort into two lists, one with the ants that appeared and one witht the plants
ants_prac <- practice_species2 %>% filter(V2 %in% ant_list$ants)
ants_prac <- remove.factors(ants_tm)
plants_prac <- practice_species2 %>% filter(V2%in% angiosperms_list$plants)
plants_prac <- remove.factors(plants_tm)

#merge into one table, with the columns abstracts, ants, plants
together_prac <- merge(ants_prac, plants_prac, by="V1")
colnames(together_prac) <- c("abs", "ants", "plants")

#no longer need abstract ID
together_prac$abs <- NULL

#this turns the table with two columns into a 0 and 1 matrix
final_table_prac <- table(together_prac)
final_table_prac <- (final_table_prac > 0) + 0
                           
#did not use this code
#mat <- matrix(NA, nrow = 8, ncol = 4, dimnames = list(c(ants_tm$V2), c(plants_tm$V2)))

#####Seed dispersal#####
sd_network <- read.csv("sd_network_tm_processed.csv", header=TRUE)
sd_network2 <- sd_network[sd_network$abs %in% sd_network$abs[duplicated(sd_network$abs)],]
sd_network2$X <- NULL

ants_sd <- sd_network2 %>% filter(name %in% ant_list$ants)
ants_sd <- remove.factors(ants_sd)
plants_sd <- sd_network2 %>% filter(name %in% angiosperms_list$plants)
plants_sd <- remove.factors(plants_sd)

together_sd <- merge(ants_sd, plants_sd, by="abs")
colnames(together_sd) <- c("abs", "ants", "plants")

together_sd$abs <- NULL

final_table_sd <- table(together_sd)
final_table_sd <- (final_table_sd > 0) + 0
write.csv(final_table_sd, file = "sd_01_network.csv")

#####Extrafloral nectar#####
efn_network <- read.csv("efn_network_tm_processed.csv", header=TRUE)
efn_network2 <- efn_network[efn_network$abs %in% efn_network$abs[duplicated(efn_network$abs)],]
efn_network2$X <- NULL

ants_efn <- efn_network2 %>% filter(name %in% ant_list$ants)
ants_efn <- remove.factors(ants_efn)
plants_efn <- efn_network2 %>% filter(name %in% angiosperms_list$plants)
plants_efn <- remove.factors(plants_efn)

together_efn <- merge(ants_efn, plants_efn, by="abs")
colnames(together_efn) <- c("abs", "ants", "plants")

together_efn$abs <- NULL

final_table_efn <- table(together_efn)
final_table_efn <- (final_table_efn > 0) + 0
write.csv(final_table_efn, file = "efn_01_network.csv")

#####Domatia#####
dom_network <- read.csv("dom_network_tm_processed.csv", header=TRUE)
dom_network2 <- dom_network[dom_network$abs %in% dom_network$abs[duplicated(dom_network$abs)],]
dom_network2$X <- NULL

ants_dom <- dom_network2 %>% filter(name %in% ant_list$ants)
ants_dom <- remove.factors(ants_dom)
plants_dom <- dom_network2 %>% filter(name %in% angiosperms_list$plants)
plants_dom <- remove.factors(plants_dom)

together_dom <- merge(ants_dom, plants_dom, by="abs")
colnames(together_dom) <- c("abs", "ants", "plants")

together_dom$abs <- NULL

final_table_dom <- table(together_dom)
final_table_dom <- (final_table_dom > 0) + 0
write.csv(final_table_dom, file = "dom_01_network.csv")

