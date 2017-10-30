library(data.table)

## Import mega merged dataset with data_elements, categories, everything. 
## From big extraction with merge already done (Takes ~hour to import), should do it on script of extraction

## System setup
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


# subset for just useful datasets
useful_datasets <- descriptions[`Look useful? (0, 1, 99)` != 0]

## Pick a row and subset. Change the name and value of row
daily <- subset(data_element_merge,
                 datasets_name == data_sets_bdsh[8, 3],
                 c("datasets_name", "data_element_name", 
                   "value", "org_unit_ID", "category_name", "period", "last_update") )

value_vars <- c("org_unit_ID", "value")
by_vars <- c("datasets_name", "data_element_name", "org_unit_ID", "category_name", "period", "last_update")

#make org_unit a char
# obviously in hindsight could make this a loop
daily <- daily[, lapply(.SD, as.character), 
               .SDcols = value_vars, 
               by = by_vars]

value_vars <- "value"
by_vars <- c("datasets_name", "data_element_name", "org_unit_ID", "category_name", "period", "last_update")

# make value numeric
daily <- daily[, lapply(.SD, as.numeric), 
               .SDcols = value_vars, 
               by = by_vars]
# Sum for each hospital, category, data element
daily <- daily[, .(total_admissions=sum(value)), 
               by = . (data_element_name, category_name, org_unit_ID)]

# Collapse hospitals
totals <- daily[, .(total=sum(total_admissions)), 
                        by = . (category_name, org_unit_ID)]



