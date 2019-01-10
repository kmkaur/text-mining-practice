library(tidyr)
library(diplyr)
library(zoo)


setwd("~/Desktop")
tab <- read.csv(file = "test_cols.csv", header = TRUE)


tidy_table <- 
      tab %>%
        separate(abs.name, into = c("abs", "genus", "species"), fill = "left")

tidy_table$name <- paste(tidy_table$genus, tidy_table$species)
tidy_table$genus <- NULL
tidy_table$species <- NULL

tidy_table2 <- tidy_table %>% 
                  do(na.locf(.))
