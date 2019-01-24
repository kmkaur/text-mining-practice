#pre-process data from the text-mined form to use in process_data.R

#load packages
library(tidyr)
library(diplyr)
library(zoo)

#set working directory
setwd("~/Desktop")

#####test file - it worked with this#####
tab <- read.csv(file = "test_cols.csv", header = TRUE)

#separate columns for abstract number and genus and species name
#leave NA on the left side if the abstract number is missing
#will fill it in after
tidy_table <- 
  tab %>%
  separate(abs.name, into = c("abs", "genus", "species"), fill = "left")

#create new column with full binomial name
#then remove the genus and species columns
tidy_table$name <- paste(tidy_table$genus, tidy_table$species)
tidy_table$genus <- NULL
tidy_table$species <- NULL

#fill in the NA with the abstracts above
tidy_table2 <- tidy_table %>% 
  do(na.locf(.))

#did not use this code
#tidy_table$species[is.na(tidy_table$species)] <- as.character(tidy_table$genus[is.na(tidy_table$species)])

#done! ready for process_data.R file to make 0 and 1 matrix

#####Seed dispersal#####
sd_raw <- read.csv(file = "sd_network_tm_raw_2.csv", header = TRUE)

sd_tidy_table <- 
      sd_raw %>%
        separate(abs.name, into = c("abs", "genus", "species"), fill = "left")

sd_tidy_table$name <- paste(sd_tidy_table$genus, sd_tidy_table$species)
sd_tidy_table$genus <- NULL
sd_tidy_table$species <- NULL

sd_tidy_table2 <- sd_tidy_table %>% 
                  do(na.locf(.))
write.csv(sd_tidy_table2, file = "sd_network_tm_processed_2.csv")

#####Extrafloral nectar#####
efn_raw <- read.csv(file = "efn_network_tm_raw_2.csv", header = TRUE)

efn_tidy_table <- 
  efn_raw %>%
  separate(abs.name, into = c("abs", "genus", "species"), fill = "left")

efn_tidy_table$name <- paste(efn_tidy_table$genus, efn_tidy_table$species)
efn_tidy_table$genus <- NULL
efn_tidy_table$species <- NULL

efn_tidy_table2 <- efn_tidy_table %>% 
  do(na.locf(.))
write.csv(efn_tidy_table2, file = "efn_network_tm_processed_2.csv")

#####Domatia#####
dom_raw <- read.csv(file = "dom_network_tm_raw_2.csv", header = TRUE)

dom_tidy_table <- 
  dom_raw %>%
  separate(abs.name, into = c("abs", "genus", "species"), fill = "left")

dom_tidy_table$name <- paste(dom_tidy_table$genus, dom_tidy_table$species)
dom_tidy_table$genus <- NULL
dom_tidy_table$species <- NULL

dom_tidy_table2 <- dom_tidy_table %>% 
  do(na.locf(.))
write.csv(dom_tidy_table2, file = "dom_network_tm_processed_2.csv")


