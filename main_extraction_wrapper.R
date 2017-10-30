## DHIS2 extraction on Bangladesh
## From already extracted org_units and data-_sets
## With these, extract the data_elements and values

## Setup & config:

## Install required packages, if you do not already have these installed, uncomment the lines below
# install.packages("data.table")
# library(data.table)

## Filepathes & directories - update below
rm(list=ls()); 
library(dhisextractr, lib.loc= "/home/j/Project/dhis")
library(plyr)
library(RCurl)
library(XML)
library(data.table)
library(magrittr)
library(dplyr)

if (Sys.info()[1] == 'Windows') {
  username <- "[your username here]"
  root <- "J:/"
  workdir <- paste0(root, "Project/dhis/bangladesh/nrober75_code")
  out_dir <- paste0(root, "Project/dhis/bangladesh/extracted_data/nrober75")
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
  workdir <- paste0(root, "Project/dhis/bangladesh/nrober75_code")
  out_dir <- paste0(root, "Project/dhis/bangladesh/extracted_data/nrober75")
}


# set up dir
data.dir = paste0(j,'Project/dhis/bangladesh/extracted_data/nrober75')

## Need to download in the different prior files
## Extraction is based off of data downloaded by fun: (extract_dhis_content)
org_units_bdsh <- read.csv(paste0(root, "Project/dhis/bangladesh/extracted_data/nrober75/updated_org_units.csv"))
data_sets_bdsh <- read.csv(paste0(root, "Project/dhis/bangladesh/extracted_data/nrober75/updated_datasets.csv"))
data_elements_bdsh <- read.csv(paste0(root, "/Project/dhis/bangladesh/extracted_data/nrober75/updated_data_elements.csv"))
data_categories_bdsh <- read.csv(paste0(root, "/Project/dhis/bangladesh/extracted_data/data_categories.csv"))


##Insert dates that you want, loop over dates/parallelize to make it go faster
# category is age group? Pulls from this a3nDCA6sqPX
data <- extract_all_data("http://103.247.238.82:8080/dhismohfw",
                         data_sets= data_sets_bdsh,
                         org_unit= org_units_bdsh,
                         "2016-01-01", "2017-01-01", "view", "DGHS1234",
                         pace = 10, update_date= "2017-01-01")


# merge to get names of data elements and categories
data_element_merge <- merge(data, data_elements_bdsh, all.x = T, by = "data_element_ID")

#merge to get getegory names
# First change colname to category_ID
colnames(data_categories_bdsh)[2] <- "category_ID"
colnames(data_categories_bdsh)[3] <- "category_name"
colnames(data_element_merge)[6] <- "category_ID"
data_element_merge <- merge(data_element_merge, data_categories_bdsh, all.x = T, by = "category_ID")        

## This results in full dataset with the data element names and the category names         




