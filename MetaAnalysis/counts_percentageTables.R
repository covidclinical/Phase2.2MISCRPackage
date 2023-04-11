library(dplyr)
library(tidyr)
library(stringr)
library(flextable)
rm(list=ls())
setwd("/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")
sites <- list.files("./")

########################
###### FUNCTIONS ######
#######################
combineCategoricalDataFromSites <- function( fileList, table_num, characteristicsList ){
  for( i in 1:length( fileList )){
    print(i)
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    #load the file
    load( paste0("./", site_id, "/replace_earlier/", fileList[i]))
    
    if( table_num == 1){
      inputData <- table1_categorical %>% 
        mutate( site = site_id )
      rm(table1_categorical)
      
      total_Values <- read.delim(paste0("./", site_id, "/replace_earlier/", site_id, "_table1.txt" ))
      
    }else if( table_num == 3){
      inputData <- table3 %>% 
        mutate( site = site_id )
      rm(table3)
      
      total_Values <- read.delim(paste0("./", site_id, "/replace_earlier/", site_id, "_table3.txt" ))
      
    }
    
    #extract total values from column names
    variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
    colnames(variantsN) <- c("variant_misc", "total")
    variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
    
    variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(total_Values)[2], "[.]"), tail, 1))
    variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(total_Values)[3], "[.]"), tail, 1))
    variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(total_Values)[4], "[.]"), tail, 1))
    variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(total_Values)[5], "[.]"), tail, 1))
    variantsN$site <- site_id
    
    # check that all the variables-variants combinations are present
    expectedPairs <- expand.grid(characteristicsList, variantsN$variant_misc) %>%
      mutate(allCombinations = paste0(Var1, '--', Var2))
    
    presentPairs <- paste0(inputData$categories, "--", inputData$variant_misc)
    missingPairs <- expectedPairs[! expectedPairs$allCombinations %in% presentPairs,  ]
    
    # for the missing ones are the row with 0s
    toAdd <-  missingPairs %>%
                mutate( n = 0, 
                        site = site_id) %>%
                select( categories = Var1, variant_misc = Var2, n, site )
    
    inputData <- rbind( inputData, toAdd)
    
    # remove categories that we want to exclude (like history of covid-19)
    inputData <- inputData %>%
      filter( categories %in% expectedPairs$Var1 )
    
      # merge both
      output <- inputData %>%
      left_join( variantsN, by = c("variant_misc", "site"))
    
    # remove tables
    rm(total_Values)
    rm(variantsN)
    
    #remove the original df
    rm(inputData)
    
    if( i ==1 ){
      table_output <- output
    }else{
      table_output <- rbind( table_output, output)
    }
    
  }
  return( table_output )
}

##### combine continuous data from sites
combineContinuousDataFromSites <- function( fileList, time_point){
  
  for( i in 1:length( fileList )){
    
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    print(site_id)
    
    #load the file
    load( paste0("./", site_id, "/replace_earlier/", fileList[i]))
    
    if( time_point =="admission"){
      inputData <- table2_admission
      rm(table2_admission)
    }else if( time_point == "during"){
      inputData <- table2_during
      rm( table2_during)
    }
    
    #add column with the site and with the time, and select the columns of interest
    table_output <- inputData %>%
      mutate( site = site_id,
              time = time_point) %>%
      select( variant_misc, variableName, mean_value, sd_value, n_patients, site, time ) 
    
    
    #remove the original df
    rm(inputData)
    
    if( i ==1 ){
      output <- table_output
    }else{
      output <- rbind( table_output, output)
    }
  }
  return( output )
  
}



##################################
###### TABLE 1: categorical ######
##################################
#remove chop from table 1 and table 3
sites <- sites[ sites != "CHOP"] 

#re-using the folders structure, 1 folder per site and one sub-folder for cut-off dates
#for loop to extract the table1Files for each site
for( i in 1:length(sites)){
  if(i == 1){
    table1_files <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table1")
  }else{
    table1_filesInt <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table1")
    table1_files <- c( table1_files, table1_filesInt)
  }
}

categoricalTable1 <- table1_files[ grepl( "Categorical.RData", table1_files)] 

#extract the list of expected variables 
load( "/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/listOfVariablesForMetaAnalysis.RData")
clinicalCharacteristics <- c(list_to_evaluate$ClinicalCharacteristic_categories, 
                             list_to_evaluate$ClinicalCharacteristic_all)

#apply the combineCategoricalDataFromSites function to read all the files
table1_categorical <- combineCategoricalDataFromSites( fileList = categoricalTable1, 
                                                       table_num = 1, 
                                                       characteristicsList =  clinicalCharacteristics)


#### change obfuscation from 0.5 to 1 and re-format estimating total n, and percentage
table1_output <- table1_categorical %>%
  ungroup() %>%
  mutate( n = ifelse( n == 0.5, 1, n )) %>% 
  group_by(categories, variant_misc) %>%
  summarise(new_n = sum(n),
            new_total = sum(total), 
            perc = round(new_n/new_total*100, 2),
            cell_content = paste0( new_n, " (", perc, "%)"), 
            variant_name = paste0( variant_misc, " (N = ", new_total, ")")
  ) %>%
  ungroup() %>%
  select( - new_n, -perc, -variant_misc, -new_total ) %>%
  unique() %>%
  pivot_wider( names_from =  variant_name, 
               values_from = cell_content)

table1_output %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../../table1_counts_perc.docx")

save(table1_output, file = "../../table1_counts_perc.RData")

###### Add the counts and percentage for sex
sex_counts <- read.delim("/Users/alba/Desktop/sex_counts_replaceEarlier.tsv")
sex_counts$site <- gsub("Pitt CHP", "PittCHP", sex_counts$site )
sex_counts$site <- gsub("H120", "H12O", sex_counts$site )

sex_counts <- sex_counts %>%
  select( categories = sex, variant_misc = variant, n = count, site = site, total = total.n.for.variant)

sex_counts_output <- sex_counts %>%
  ungroup() %>%
  mutate( n = ifelse( n == 0.5, 1, n )) %>% 
  group_by(categories, variant_misc) %>%
  summarise(new_n = sum(n),
            new_total = sum(total), 
            perc = round(new_n/new_total*100, 2),
            cell_content = paste0( new_n, " (", perc, "%)"), 
            variant_name = paste0( variant_misc, " (N = ", new_total, ")")
  ) %>%
  ungroup() %>%
  select( - new_n, -perc, -variant_misc, -new_total ) %>%
  unique() %>%
  pivot_wider( names_from =  variant_name, 
               values_from = cell_content)

sex_counts_output %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../../sex_counts_perc.docx")

##################################
###### TABLE 3: categorical ######
##################################

#re-using the folders structure, 1 folder per site and one sub-folder for cut-off dates
#for loop to extract the table1Files for each site
for( i in 1:length(sites)){
  if(i == 1){
    table3_files <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table3")
  }else{
    table3_filesInt <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table3")
    table3_files <- c( table3_files, table3_filesInt)
  }
}

table3_files <- table3_files[ grepl( ".RData", table3_files)] 

#extract the list of expected variables 
load( "/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/listOfVariablesForMetaAnalysis.RData")
outcomes <- list_to_evaluate$Outcomes_all

#apply the combineCategoricalDataFromSites function to read all the files
table3_all <- combineCategoricalDataFromSites( fileList = table3_files, 
                                               table_num = 3,
                                               characteristicsList =  outcomes)


#### change obfuscation from 0.5 to 1 and re-format estimating total n, and percentage
table3_output <- table3_all %>%
  ungroup() %>%
  mutate( n = ifelse( n == 0.5, 1, n )) %>% 
  group_by(categories, variant_misc) %>%
  summarise(new_n = sum(n),
            new_total = sum(total), 
            perc = round(new_n/new_total*100, 2),
            cell_content = paste0( new_n, " (", perc, "%)"), 
            variant_name = paste0( variant_misc, " (N = ", new_total, ")")
  ) %>%
  ungroup() %>%
  select( - new_n, -perc, -variant_misc, -new_total ) %>%
  unique() %>%
  pivot_wider( names_from =  variant_name, 
               values_from = cell_content)


table3_output %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../../table3_counts_perc.docx")

save(table3_output, file = "../../table3_counts_perc.RData")