# Impacts of heat stress on soft corals, an overlooked and highly vulnerable component of coral reef ecosystems, at a central equatorial Pacific atoll

# Authors: Dominique G. Maucieri[1], and Julia K. Baum[1]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca

# Script to create Table S1 (Number of quadrats in each year for each site on Kiritimati Island)

##############################

## load packages 
library(rJava)
library(xlsx)
library(dplyr)
library(tidyr)

## Set your working directory
# Make sure that this contains all the file folders
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

load("data/raw_soft_coral.Rdata")

## Summarize number of quadrats by year and site
## Order chosen based on location around atoll
Table_S1 <- raw_soft_coral %>% group_by(FieldSeason, Site) %>% dplyr::summarise(Sample_Size = length(Site)) %>% 
  mutate(Site = factor(Site, levels = c("site17", "site18", "site19", "site39", "site15", "site16", "site20", "site11", "site21", 
                                        "site10", "site23", "site1", "site22", "site2", "site24", "site38", "site3", "site25", "site26", 
                                        "site30", "site27", "site31", "site32", "site40", "site33", "site9", "site34", "site35", "site8", 
                                        "site14", "site6", "site7", "site13", "site12", "site4", "site36", "site5", "site37", "site28", "site29"))) %>% 
  arrange(Site) %>% spread(FieldSeason, Sample_Size) %>% dplyr::select(-Site)

#turn any sites that were not sampled from NA to 0 and from a tibble to a dataframe
Table_S1 <- as.data.frame(Table_S1)
Table_S1[is.na(Table_S1)] <- 0

#rename rows and columns
rownames(Table_S1) <- c("Site 17", "Site 18", "Site 19", "Site 39", "Site 15", "Site 16", "Site 20", "Site 11", "Site 21", "Site 10", "Site 23", 
                                      "Site 1", "Site 22", "Site 2", "Site 24", "Site 38", "Site 3", "Site 25", "Site 26", "Site 30", "Site 27", "Site 31", 
                                      "Site 32", "Site 40", "Site 33", "Site 9", "Site 34", "Site 35", "Site 8", "Site 14", "Site 6", "Site 7", "Site 13", 
                                      "Site 12", "Site 4", "Site 36", "Site 5", "Site 37", "Site 28", "Site 29")
colnames(Table_S1) <- c("2007",  "2009",  "2010",  "2011",  "2013",  "2014",  "2015 - Jan", "2015 - May", 
                                      "2015 - Jul", "2016 - Mar", "2016 - Nov", "2017", "2018",  "2019" )

## Save as Table S1
write.xlsx(Table_S1, "tables_figures/Table_S1.xlsx", row.names = TRUE)


