#'Generic xml page parsing function
#'
#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @param xml wheter the url should end with '.xml'. This has been added to add some
#' flexibility in the kind of parsable urls.

setwd('J:/Project/dhis/bangladesh/extracted_data/nrober75')

library(dhisextractr)
library(RCurl)
library(XML)

#INDIVIDUALLY EVERYTHING IS WORKING RIGHT NOW, THE FUNCTION JUST ISN'T PASSING
library(data.table)

parse_page <- function(url, userID, password, xml = F){
  url <- as.character(url)
  nchar_url <- nchar(url)
  #print("parsed from parse_page")
  
  # create username and password
  userpwd <- paste(userID, password , sep = ':')
  print(url)
  print(userpwd)
  response <- getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)
  
  parsed_page <- xmlParse(response)
  
  root <- xmlRoot(parsed_page)
  #print(root)
  # printing here correctly but not saving into the object correctly???
  # I need to return root
  return(root)
}


# when done individually, it works
# when done as a function to return root, doesn't work


#'Generic function to extract relevant nodes in DHIS element
#' \code{extract_info} goes to a specific DHIS2 element url, and extracts attributes
#' name , id and href. It can extract elements that span on multiple pages
#'
#' @param url_page The default url of the elemtns to parse in the DHIS api, as a
#' character string function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param root root of this page, as extracted by \code{\link{parse_page}}
#' @param node_name the name of the name we wish to extract
#' @param out an empty dataframe in which to return the output (there are more elegant ways to do it for sure, see it later).
extract_info <- function(url_page , root , node_name , userID , password , monitor = F){
  print(userID)
  print(password)
  # Turns NPages to 1 if there are no pages
  NPages <- as.numeric(xmlValue(root[['pager' ]][['pageCount']]))
  NPages[is.na(NPages)] <- 1
  out <- data.frame(matrix(ncol = 2, nrow = 0))
  root2 <- root[[2]]
  #root_children <- xmlChildren(root[[2]])
  for (page in 1:NPages){
    #ID <- name <- url <- ''
    
    if(monitor == TRUE){
      print(paste('Parsing page' , page , 'out of' , NPages , sep = ' '))
    }
    print(url_page)
    if(NPages > 1){
      url_read <- paste(url_page , '?page=' , page , sep = '')
    } else{
      url_read <- url_page
    } 
    print(url_read)
    root <- parse_page(url_read , userID , password , xml = FALSE)
    root2 <- root[[2]]
    root_children <- xmlChildren(root2)
    
    url_page <- as.character(url_page)
    ID <- c()
    url_list <- c()
    if (!is.null(root2[[node_name]]) & length(root2[[node_name]]) > 0){
      ID <- xmlSApply(root2 , xmlGetAttr , 'id')
      names <- xmlToDataFrame(root2)
      url_list <- paste0(substr(url_page, 1, nchar(url_page)-4),
                      '/',
                      ID, 
                      '.xml')
        #url <- xmlSApply(root2[[node_name]] , xmlGetAttr , 'href')
        #list <- c()
        #for(i in seq(1,length(root_children))){
          #name <- (print_node(root_children[i], 'dataSet'))
          #name_attr <- xmlGetAttr('displayName')
          #list <- c(list , name[[1]]) # This is returning something weird. 
        #}
        # url <- xmlSApply(root[[node_name]] , xmlGetAttr , 'href')
    }
      #print(ID)
      #print(names)
    loop_out <- data.frame(ID , names, url_list)
      #colnames(out) <- colnames(loop_out)
    out <- rbind(out , loop_out)
    out
  }
  return(out)
}

#print_node <- function(root2 , node_name){
#  unlist(xmlChildren(root_children[i])['displayName'])
#}
#unlist(xmlChildren(root2[['dataSet']])['displayName'])[[1]]
#root2

#out <- extract_dhis_datasets('http://103.247.238.82:8080/dhismohfw/api/dataSets.xml', 'view', 'DGHS1234')
#get_children_name <- function(node_name , info_name){
  #name = xmlChildren(node_name)[[info_name]]
  #name
#}
  
## Now need to put the pieces back into extract_info

#' Extracting the list of datasets in DHIS
#'
#' \code{extract_dhis_datasets} goes to a specific DHIS2 implementation, and extracts
#' its full list of data sets
#'
#' @param url The url of the datasets list in the DHIS web api, as a character string.
#' The function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' unique ID, its name and its url.


extract_dhis_datasets <- function(url , userID , password){
  root <- parse_page(url , userID , password)
  #out <- data.frame(datasets_ID = character() ,
                    #datasets_name = character()
                    #)
                    #datasets_url = character() )
  print(url)
  extract_info(url , root , 'dataSet' , userID , password)
}



#'Extract the list of data elements in a DHIS data set
#'
#' \code{extract_data_elements} extracts the data elements recorded in a given dataset.
#' ## Changed to just get all of the data element names, because extract_all_content does this
#'
#' @param url The url of the dataset page in the DHIS api, from which we want to
#' extract the data elements. The function is made to parse xml pages, so input url
#' should be an xml adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each data element as a line and for each data
#' element, its unique ID, its name and its url.
#' 
#' ## Do you need to do this for datasets? Or just for the whole list of data elements. Wouldn't that be fine?
extract_data_elements <- function(dataset_url, userID, password){
  root <- parse_page(dataset_url , userID , password)
  #out <- data.frame(data_element_ID = character() ,
                    #data_element_name = character()  ,
                    #data_element_url = character() )
  extract_info(dataset_url , root , 'dataElement' , userID , password)
}

#'Extract the list of Organisation Units in the DHIS setting
#'
#' \code{extract_orgunits_list} extracts the list of Organisation Units recorded in a
#' DHIS setting
#'
#' @param url The url of the organisation units page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each organisation unit as a line and for each
#' organisation unit, its unique ID, its name and its url.
extract_orgunits_list <- function(org_unit_page_url, userID, password){
  #out <- data.frame(org_unit_ID = character() ,
                    #org_unit_name = character()  ,
                    #org_unit_url = character() )
  root <- parse_page(org_unit_page_url , userID , password)
  extract_info(org_unit_page_url , root , 'organisationUnit', userID , password , TRUE)
}


#'Extract information about an Orgunit
#'
#' \code{extract_org_unit} extracts all the information about
#'
#' @param url The url of the organisation unit for which we want to extract
#' information. The function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list with three elements :
#' * __Metadata__ For each organization unit, includes its geolocalization and
#' reference to parent unit
#'
#' * __Group __ Groups in which the organization unit is included. This is where the
#' type of organization unit is stored
#'
#' * __Datasets__ Datasets for which the organisation unit should communicate data
#' #Gets hospital/org_unit metadata, not just name and ID


org_units_list <- extract_orgunits_list('http://103.247.238.82:8080/dhismohfw/api/organisationUnits.xml', 'view', 'DGHS1234')

org_unit_meta <- extract_org_unit(org_units_list$url_list, 'view', 'DGHS1234')


extract_org_unit <- function(org_unit_url, userID, password){
  root <- parse_page(org_unit_url , userID , password)
  
  ##Extraction of org units metadata
  parent_id <- parent_name <- parent_url <- NA
  
  id <- xmlAttrs(root)[['id']]
  coordinates <- xmlValue(root[['coordinates']])
  opening_date <- xmlValue(root[['openingDate']])
  name <- xmlValue(root[['displayName']])
  active <- xmlValue(root[['active']])
  if (!is.null(root[['parent']])){
    parent_id <- xmlAttrs(root[['parent']])[['id']]
    #parent_name <- xmlAttrs(root[['parent']])[['name']]
    #parent_url <- xmlAttrs(root[['parent']])[['href']]
  }
  org_unit_metadata <- data.frame(id , coordinates , opening_date , name ,
                                  active , parent_id)
  
  ##Extraction of org units groups
  org_unit_group <- data.frame(group_ID = character() , group_name = character() ,
                               group_url = character())
  if (!is.null(root[['organisationUnitGroups']])){
    Groups <- root[['organisationUnitGroups']]
    group_ID <- xmlSApply(Groups , xmlGetAttr , 'id')
    #group_name <- xmlSApply(Groups , xmlGetAttr , 'name')
    #group_url <- xmlSApply(Groups , xmlGetAttr , 'href')
    org_unit_group <- data.frame(group_ID)
  }
  
  ##Extraction of org units datasets
  org_unit_dataset <- data.frame(dataset_ID = character() ,
                                 dataset_name = character() ,
                                 dataset_url = character())
  if (!is.null(root[['dataSets']])){
    Datasets <- root[['dataSets']]
    dataset_ID <- xmlSApply(Datasets , xmlGetAttr , 'id')
    dataset_name <- xmlSApply(Datasets , xmlGetAttr , 'name')
    dataset_url <- xmlSApply(Datasets , xmlGetAttr , 'href')
    org_unit_dataset <- data.frame(dataset_ID)
  } else{
    org_unit_dataset <- c()
  }
  two_variables <- list(org_unit_group, org_unit_dataset)
  out <- list(org_unit_metadata , org_unit_group , org_unit_dataset)
  return(out)
}

#'Extract the categories for data elements
#'
#' \code{extract_categories} extracts the list of categories that are used for different
#' data elements.
#'
#' @param categories_url The url of the categories page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each category as a line and for each
#' category, its unique ID, its name and its url.
extract_categories <- function(categories_url, userID, password){
  #out <- data.frame(org_unit_ID = character() ,
                    #org_unit_name = character()  ,
                    #org_unit_url = character() )
  root <- parse_page(categories_url , userID , password)
  extract_info(categories_url , root , 'categoryOptionCombo', userID , password)
}


#'Make relevant urls in DHIS web api
#'
#' \code{make_dhis_urls} takes the main adress of a DHIS implementation and returns
#' the relevant adresses in the web api that will be used for extracting data.
#'
#' @param base_url The url of the DHIS implementation
make_dhis_urls <- function(base_url){
  data_sets_url <- paste(base_url , '/api/dataSets.xml' , sep = '')
  data_elements_url <- paste(base_url , '/api/dataElements.xml' , sep = '')
  org_units_url <- paste(base_url , '/api/organisationUnits.xml' , sep = '')
  data_elements_categories <- paste(base_url , '/api/categoryOptionCombos.xml' , sep = '')
  data_sets_url
  data.frame(data_sets_url , data_elements_url , data_elements_categories , org_units_url, stringsAsFactors = FALSE)
}
