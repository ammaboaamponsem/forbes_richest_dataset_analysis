cat("\014") # clears console
rm(list = ls()) #clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = True) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(readr)
# Read from a CSV file
billionaire_rank <- read_csv("forbes_2640_billionaires.csv")

# Retrieve full column specification
spec(billionaire_rank)

library(janitor)
# Renaming Columns
billionaire_rank.df <- clean_names(billionaire_rank)
billionaire_rank.df

# Creating and Correcting any data types
str(billionaire_rank.df)

library(dplyr)
#Removing columns in the dataset
billionaire_rank.df_filtered <- billionaire_rank.df %>%
  select(-forbes_id, -age_range, -age_2, -source_of_wealth, -self_made_score, 
         -philanthropy_score, -residence, -marital_status, -children, -bachelor, 
         -master, -doctorate, -drop_out, -self_made)
billionaire_rank.df_filtered

# Reducing the data set to include net worth from 2.5 billion and above 
billionaire_rank_new.df <- billionaire_rank.df_filtered %>%
filter(net_worth >= 2.5)
billionaire_rank_new.df

library(tidyr)
# Removing NAs from the age column
billionaire_rank.df <- drop_na(billionaire_rank_new.df, age)
billionaire_rank.df
