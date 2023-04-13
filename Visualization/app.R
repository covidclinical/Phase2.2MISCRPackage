library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(meta)
library(forestploter)


ui <- fluidPage(
    
    #titlePanel("4CE MISC: QC and meta-analysis"),
    theme = shinythemes::shinytheme("cosmo"),
    
    navbarPage("4CE MISC: QC and meta-analysis. Tool for exploratory purposes",
               tabPanel(icon("house"),
                        
                        fluidRow( column(
                            
                            br(),
                            p("Multisystem Inflammatory Syndrome in Children (MIS-C) is a post-infectious vasculitis associated with SARS-CoV-2 infection, and currently represents one of the most important complications of SARS-CoV-2 in pediatric age. Observational studies showed that clinical characteristics and outcomes varies among different SARS-CoV-2 waves. We investigate MIS-C characteristics, including clinical phenotypes, laboratory data, and outcomes in different eras with different SARS-CoV-2 variant predominance (alpha, delta, omicron)."),
                            br(),
                            
                            p("The data used in this application is part of the 4CE Consortium, pediatric working group."),
                            
                            width=8)),
                        
                        hr(),
                        p(em("Developed by"),br("4CE MISC working group"),style="text-align:center; font-family: times")
               ), 
               
               tabPanel("ICD QC",
                        fluidRow(p(
                                "Coding can be different across sites and countries. To review all the different ICD codes associated to MISC patients, we develop a table to compare the percentages of patients per site and code."
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                sliderInput("percentage",
                                            "Percentage of patients:",
                                            min = 0,
                                            max = 100,
                                            value = 10),
                                br(),
                            ),
                            mainPanel(DT::dataTableOutput("table")
                            ))
                        )
               ),
               tabPanel("Labs QC",
                        fluidRow(p(
                                "For the labs, to identify potential units and missingness, we compared the lab values distribution across sites."
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                radioButtons("variant", label = h3("MISC Variant type:"),
                                             choices = list("Total" = "total", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron"), 
                                             selected = "total"),
                                
                                br(),
                                
                                radioButtons("time", "Time point:",
                                             choices = list("At admission (day 0 or 1)" = "admission", "During admission" = "during"), 
                                             selected = "during"),
                                
                            ),
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Lab values comparison", plotOutput("plot1"),  width = "100%"),
                                            tabPanel("% patients by lab value", plotOutput("plot2"),  width = "100%")
                                )
                            ))
                        )
               ),
               tabPanel("Table 1 and 3: comparison across sites",
                        fluidRow(p(
                            "Here we compare the results across sites for categorical variables in tables 1 and 3"
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                radioButtons("variantC", label = h3("MISC Variant type:"),
                                             choices = list("Total" = "total", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron", "All" = "all"), 
                                             selected = "all")
                            ),
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Table 1: percentages", plotOutput("plot1_perc"),  width = "100%"),
                                            tabPanel("Table 3: percentages", plotOutput("plot3_perc"),  width = "100%")
                                )
                            ))
                        )
               ),
               tabPanel("Labs meta-analysis",
                        fluidRow(
                        sidebarLayout(
                            sidebarPanel(
                                
                              radioButtons("variantLabMeta", label = h3("Comparison:"),
                                           choices = list( "delta vs. alpha" = "Delta", "omicron vs. alpha" ="Omicron"), 
                                           selected = "Delta"),

                                br(),
                                
                                radioButtons("timeLabMeta", "Time point:",
                                             choices = list("At admission (day 0 or 1)" = "admission", "During admission" = "during"), 
                                             selected = "during"),
                                
                                br(), 
                                
                                radioButtons("labMeta", label = h3("Chose a laboratory test:"),
                                             choices = list("alanine aminotransferase (ALT)" = "alanine aminotransferase (ALT)",
                                                            "albumin" = "albumin",
                                                            "aspartate aminotransferase (AST)"   = "aspartate aminotransferase (AST)", 
                                                            "CRP"     =      "CRP",                      
                                                            "creatinine"   = "creatinine",                        
                                                            "ferritin"    = "ferritin",                        
                                                            "d_dimer" = "d_dimer",                              
                                                            "troponin normal sensitivity" = "troponin normal sensitivity",    
                                                            "troponin high sensitivity" = "troponin high sensitivity",         
                                                            "white blood cell count (Leukocytes)" = "white blood cell count (Leukocytes)" , 
                                                            "lymphocyte" = "lymphocyte" ,                        
                                                            "neutrophil"   = "neutrophil",                        
                                                            "platelets"  = "platelets" ,                         
                                                            "fibrinogen" = "fibrinogen" ,                         
                                                            "prothrombin time (PT)" = "prothrombin time (PT)",               
                                                            "nlr_ratio" = "nlr_ratio")
                                             
                                )
                                
                            ),
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Forest plot", plotOutput("forestsiteLab"),  width = "100%")
                                )
                            ))
                        )
               ), 
               tabPanel("Clinical Characteristics Meta-analysis",
                        fluidRow(
                        sidebarLayout(
                          sidebarPanel(
                            
                            radioButtons("variantCategoryMeta", label = h3("Comparison:"),
                                         choices = list( "delta vs. alpha" = "Delta", "omicron vs. alpha" ="Omicron"), 
                                         selected = "Delta"),
                            
                            radioButtons("var_category", label = h3("Chose a variable:"),
                                         choices = list("CARDIOVASCULAR SYMPTOMS"  =  "CARDIOVASCULAR SYMPTOMS",                
                                                        "GI SYMPTOMS"  =   "GI SYMPTOMS",                            
                                                        "LIVER DYSFUNCTION" =   "LIVER DYSFUNCTION",                     
                                                        "NEUROLOGIC SYMPTOMS"  = "NEUROLOGIC SYMPTOMS",                      
                                                        "RENAL DYSFUNCTION" =  "RENAL DYSFUNCTION",                
                                                        "RESPIRATORY SYMPTOMS"  = "RESPIRATORY SYMPTOMS", 
                                                        "SHOCK/SIRS"   =  "SHOCK/SIRS" ), selected = "CARDIOVASCULAR SYMPTOMS")
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Forest plot", plotOutput("forestsiteCategories"),  width = "100%")
                            ))
                          ))
                        ), 
               tabPanel("Outcomes Meta-analysis",
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              
                              radioButtons("variantOutcomeMeta", label = h3("Comparison:"),
                                           choices = list( "delta vs. alpha" = "Delta", "omicron vs. alpha" ="Omicron"), 
                                           selected = "Delta"),
                              
                              radioButtons("var_outcomes", label = h3("Chose an outcome:"),
                                           choices = list("Anticoagulation therapy"  =  "Anticoagulation therapy",                
                                                          "Cardiac arrest"  =   "Cardiac arrest",                            
                                                          "Composite adverse cardiovascular outcome" =   "Composite adverse cardiovascular outcome",                     
                                                          "Diuretic therapy"  = "Diuretic therapy",                      
                                                          "ECMO" =  "ECMO",                
                                                          "in_icu"  = "in_icu", 
                                                          "Inotropic support"   =  "Inotropic support", 
                                                          "Invasive monitoring (arterial line)" = "Invasive monitoring (arterial line)" , 
                                                          "Oxygen supplementation" = "Oxygen supplementation", 
                                                          "Sedation or muscle relaxant"  = "Sedation or muscle relaxant" ), selected = "Composite adverse cardiovascular outcome")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Forest plot", plotOutput("forestsiteOutcomes"),  width = "100%")
                              ))
                          ))
               )
    )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output) {
    sites <- list.files("./4CE_MISC_outputs/")
    
    #### ICD code QC
    output$table <- DT::renderDataTable(DT::datatable({
        
        ### our gold standar codes 
        clinicalCodes <- read.delim("./clinicalCharacteristics_paper1.txt", sep = "\t") %>%
            mutate( ICDcode = gsub("[.]", "", concept_code)) %>%
            select( -concept_code )
        
        ### read the total patient file 
        totalN <- read.delim("./sites_totalPatients.txt")
        
        ### read the ICD files from each site
        files <- list.files("./4CE_MISC_outputs/")

        for( i in 1:length( files )){
            site_id <- files[i]
            n_total <- totalN %>% filter( site == site_id )
            load( paste0("./4CE_MISC_outputs/", files[i], "/replace_earlier/QC/", files[i], "_ICDdiagnosisCodes.RData"))
            
            if( i ==1 ){
                site_codes <- diag_sum %>%
                    dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
                    dplyr::mutate( perc = round(n_patients / n_total$total_n *100, 2), 
                                   site = site_id, 
                                   ICDcode = gsub("[.]", "", concept_code)) %>%
                    dplyr::filter( perc >= input$percentage ) %>% 
                    dplyr::select( ICDcode, perc )
                colnames( site_codes)[2] <- paste0( site_id, "_percPatients") 
                rm( diag_sum )
            }else{
                int_site_codes <- diag_sum %>%
                    dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
                    dplyr::mutate( perc = round(n_patients / n_total$total_n *100, 2), 
                                   site = site_id, 
                                   ICDcode = gsub("[.]", "", concept_code)) %>%
                    dplyr::filter( perc >= input$percentage ) %>% 
                    dplyr::select( ICDcode, perc )
                colnames( int_site_codes)[2] <- paste0( site_id, "_percPatients") 
                rm( diag_sum )
                site_codes <- full_join( site_codes, int_site_codes )
            }
            
        }
        
        allCodes <- unique( c( clinicalCodes$ICDcode, site_codes$ICDcode))
        
        ### read the description of the codes
        cdc_codes <- read.delim("./icd10cm-codes-2022.txt", header = FALSE) %>%
            dplyr::mutate( V1 = gsub("\\s+", " ", V1), 
                           ICDcode = sapply( strsplit( V1, " "), '[', 1)) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::group_by( ICDcode ) %>%
            dplyr::mutate( ICDdescription = paste( sapply( strsplit( V1, " "), '[', -1), collapse = " ")) %>%
            dplyr::select( -V1 ) 
        
        
        #phecode_code <- read.csv("Phecode_map_v1_2_icd10cm_beta.csv") %>%
        phecode_code <- read.csv("./phecode_icd10.csv") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", ICD10), 
                           ICDdescription = ICD10.String) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        #codes description missing but found in UMLS
        missingCodesPresentInUMLS <- read.delim("missingCodesFoundInUmls.dsv") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", CODE), 
                           ICDdescription = STR) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        #codes descriptions manually added
        manualCodesDescriptionAdded <- read.delim("manuallyAddedCodeDescription.txt") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", CODE), 
                           ICDdescription = DESCRIPTION) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        totalCode <- rbind( cdc_codes, phecode_code, missingCodesPresentInUMLS, manualCodesDescriptionAdded ) %>%
            unique()
        totalCode <- dplyr::full_join( totalCode, clinicalCodes )
        
        
        ### merge all the tables in one
        compareTable <- dplyr::full_join( totalCode, site_codes ) 
        compareTable <- compareTable[ order( compareTable$ICDcode, decreasing = FALSE), ]
        ### remove CHOP
        compareTable <- compareTable %>% select( - CHOP_percPatients )
        
    }, options = list("pageLength" = 50),  filter = "top", rownames = FALSE))
    
    
    
    
    for( i in 1:length(sites)){
      if(i == 1){
        pvaluesFiles3 <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "3.txt")
        rdataFiles3 <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table3.RData")
      }else{
        pvaluesFiles3Int <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "3.txt")
        rdataFiles3Int <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table3.RData")
        
        pvaluesFiles3 <- c( pvaluesFiles3, pvaluesFiles3Int)
        rdataFiles3 <- c( rdataFiles3, rdataFiles3Int)
        
      }
    }
    
    for( i in 1:length(pvaluesFiles3)){
      print(i)
      siteid <- unlist(strsplit(x = pvaluesFiles3[i], split = "_"))[1]
      p_values <- read.delim(paste0( "./4CE_MISC_outputs/", siteid, "/replace_earlier/",pvaluesFiles3[i]))
      
      variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
      colnames(variantsN) <- c("variant_misc", "total")
      variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
      
      variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(p_values)[2], "[.]"), tail, 1))
      variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(p_values)[3], "[.]"), tail, 1))
      variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(p_values)[4], "[.]"), tail, 1))
      variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(p_values)[5], "[.]"), tail, 1))
      variantsN$site <- siteid
      #colnames(p_values) <- c("categories", "Alpha", "Delta", "Omicron", "total", "p.value")
      p_values <- p_values[, c(1:5)]
      colnames(p_values) <- c("categories", "Alpha", "Delta", "Omicron", "total")
      p_values$site <- siteid
      
      if( i == 1){
        pvalData3 <- p_values 
        variantsData3 <- variantsN
      }else{
        pvalData3 <- rbind(pvalData3, p_values)
        variantsData3 <- rbind(variantsData3, variantsN)
        
      }
    }
    
    pvalData3 <- pvalData3 %>%
      select( categories, site )
    
    rm(variantsN)
    rm(p_values)
    
    for( i in 1:length(rdataFiles3)){
      print(i)
      siteid <- unlist(strsplit(x = rdataFiles3[i], split = "_"))[1]
      load(paste0( "./4CE_MISC_outputs/", siteid, "/replace_earlier/", rdataFiles3[i]))
      table3$site <- siteid
      
      if( i == 1){
        complete_table3 <- table3
      }else{
        complete_table3 <- rbind(complete_table3, table3)
      }
    }
    
    ### table 3 generation
    complete_table3 <- complete_table3 %>%
      left_join( pvalData3, by = c("categories", "site") ) %>%
      left_join( variantsData3, by = c("variant_misc", "site"))
    
    ##### add with 0s the sites that do not have a variable
    allOutcomes <- unique( complete_table3$categories)
    totalOutcomeCounts <- complete_table3 %>% ungroup() %>% select( site, variant_misc, total) %>% unique()
    allSites <- unique(complete_table3$site)
    allVariants <- unique(complete_table3$variant_misc)
    
    toTest <- expand.grid(allSites, allVariants, allOutcomes) %>%
      mutate(allCombinations = paste0(Var1, '--', Var2, '--', Var3))
    
    toCompare <- complete_table3 %>%
      mutate(allCombinations = paste0(site, '--', variant_misc, '--', categories))
    missingCombinations <- toTest$allCombinations[!toTest$allCombinations %in% toCompare$allCombinations]
    toAdd <- data.frame(comb = missingCombinations) %>%
      tidyr::separate(col = comb, into = c('site', 'variant_misc', 'categories'), sep = '--') %>%
      mutate( n = 0 ) %>%
      dplyr::left_join( totalOutcomeCounts, by = c('site', 'variant_misc'))
    
    complete_table3 <- rbind( complete_table3, toAdd) %>% 
      dplyr::mutate( n = ifelse( n == 0.5, 1, n)) %>% #change obfuscation from 0.5 to 1
      dplyr::filter( site != "CHOP")
    ######
    
    ### table 1 generation
    ### table 1 categorical
    for( i in 1:length(sites)){
      if(i == 1){
        pvaluesFiles1 <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "1.txt")
        rdataFiles1 <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table1Categorical.RData")
      }else{
        pvaluesFiles1Int <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "1.txt")
        rdataFiles1Int <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table1Categorical.RData")
        
        pvaluesFiles1 <- c( pvaluesFiles1, pvaluesFiles1Int)
        rdataFiles1 <- c( rdataFiles1, rdataFiles1Int)
        
      }
    }
    
    for( i in 1:length(pvaluesFiles1)){
      print(i)
      siteid <- unlist(strsplit(x = pvaluesFiles1[i], split = "_"))[1]
      p_values <- read.delim(paste0( "./4CE_MISC_outputs/", siteid, "/replace_earlier/",pvaluesFiles1[i]))
      
      variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
      colnames(variantsN) <- c("variant_misc", "total")
      variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
      
      variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(p_values)[2], "[.]"), tail, 1))
      variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(p_values)[3], "[.]"), tail, 1))
      variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(p_values)[4], "[.]"), tail, 1))
      variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(p_values)[5], "[.]"), tail, 1))
      variantsN$site <- siteid
      p_values <- p_values[, c(1:5)]
      colnames(p_values) <- c("categories", "Alpha", "Delta", "Omicron", "total")
      p_values$site <- siteid
      
      if( i == 1){
        pvalData1 <- p_values 
        variantsData1 <- variantsN
      }else{
        pvalData1 <- rbind(pvalData1, p_values)
        variantsData1 <- rbind(variantsData1, variantsN)
        
      }
    }
    pvalData1 <- pvalData1 %>%
      select( categories, site )
    
    rm(variantsN)
    rm(p_values)
    
    for( i in 1:length(rdataFiles1)){
      print(i)
      siteid <- unlist(strsplit(x = rdataFiles1[i], split = "_"))[1]
      load(paste0( "./4CE_MISC_outputs/", siteid, "/replace_earlier/", rdataFiles1[i]))
      table1_categorical$site <- siteid
      
      if( i == 1){
        complete_table1 <- table1_categorical
      }else{
        complete_table1 <- rbind(complete_table1, table1_categorical)
      }
    }
    
    complete_table1 <- complete_table1 %>%
      left_join( pvalData1, by = c("categories", "site") ) %>%
      left_join( variantsData1, by = c("variant_misc", "site")) %>%
      unique()
    
    ##### add with 0s the sites that do not have a variable
    allVariables <- unique( complete_table1$categories)
    counts <- complete_table1 %>% ungroup() %>% select( site, variant_misc, total) %>% unique()
    allSites <- unique(complete_table1$site)
    allVariants <- unique(complete_table1$variant_misc)
    
    toTest <- expand.grid(allSites, allVariants, allVariables) %>%
      mutate(allCombinations = paste0(Var1, '--', Var2, '--', Var3))
    
    toCompare <- complete_table1 %>%
      mutate(allCombinations = paste0(site, '--', variant_misc, '--', categories))
    missingCombinations <- toTest$allCombinations[!toTest$allCombinations %in% toCompare$allCombinations]
    toAdd <- data.frame(comb = missingCombinations) %>%
      tidyr::separate(col = comb, into = c('site', 'variant_misc', 'categories'), sep = '--') %>%
      mutate( n = 0 ) %>%
      dplyr::left_join( counts, by = c('site', 'variant_misc'))
    
    complete_table1 <- rbind( complete_table1, toAdd) %>% 
      dplyr::mutate( n = ifelse( n == 0.5, 1, n)) %>% #change obfuscation from 0.5 to 1
      dplyr::filter( site != "CHOP")
    
    ### change renal involvement by renal disfunction
    complete_table1$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", complete_table1$categories)
    
    categoriesToVisualize <- c("CARDIOVASCULAR SYMPTOMS" , "GI SYMPTOMS", "LIVER DYSFUNCTION", 
                               "NEUROLOGIC SYMPTOMS", "RENAL DYSFUNCTION", "RESPIRATORY SYMPTOMS", 
                               "GENERALIZED SYMPTOMS", "Kawasaki" )
    
    
    #### Labs QC
    for( i in 1:length(sites)){
        print(sites)
        if(i == 1){
            labFiles <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table2")
        }else{
            labFilesInt <-  list.files(paste0("./4CE_MISC_outputs/", sites[i], "/replace_earlier/"), pattern = "table2")
            labFiles <- c( labFiles, labFilesInt)
        }
    }
    
    
    
    ### split the files
    at_admission_files <- labFiles[ grepl( "AtAdmission.RData", labFiles)] 
    during_admission_files <- labFiles[ grepl( "DuringAdmission.RData", labFiles)] 
    
    totalN <- read.delim("./sites_totalPatients.txt")
    
    for( i in 1:length( at_admission_files )){
        site_id <- sapply( strsplit(at_admission_files[i], "[_]"), '[', 1)
        
        #load the file
        load( paste0("./4CE_MISC_outputs/", site_id, "/replace_earlier/",site_id,  "_table2AtAdmission.RData"))
        n_total <- totalN %>% filter( site == site_id )
        
        if( i ==1 ){
            
            labs_adm <- table2_admission %>%
                dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
              mutate(site = site_id,
                     variant_misc = ifelse(variant_misc == 'total_n', 'total', variant_misc)) %>%
              left_join(variantsData1) %>%
              mutate( percs = round(n_patients / total *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "admission")
            
            rm(table2_admission)
        }else{
            int_labs_adm <- table2_admission %>%
                dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
              mutate(site = site_id,
                     variant_misc = ifelse(variant_misc == 'total_n', 'total', variant_misc)) %>%
              left_join(variantsData1) %>%  
              mutate( percs = round(n_patients / total *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "admission" )
            labs_adm <- rbind( labs_adm, int_labs_adm)
        }
    }
    
    for( i in 1:length( during_admission_files )){
        site_id <- sapply( strsplit(during_admission_files[i], "[_]"), '[', 1)
        load( paste0("./4CE_MISC_outputs/", site_id, "/replace_earlier/",site_id,  "_table2DuringAdmission.RData"))
        n_total <- totalN %>% filter( site == site_id )
        
        if( i ==1 ){
            
            labs_during <- table2_during %>%
                dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
              mutate(site = site_id,
                     variant_misc = ifelse(variant_misc == 'total_n', 'total', variant_misc)) %>%
              left_join(variantsData1) %>%  
              mutate( percs = round(n_patients / total *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "during")
            
            rm(table2_during)
        }else{
            int_labs_during <- table2_during %>%
              dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1  
              mutate(site = site_id,
                     variant_misc = ifelse(variant_misc == 'total_n', 'total', variant_misc)) %>%
              left_join(variantsData1) %>%  
              mutate( percs = round(n_patients / total *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "during" )
            labs_during <- rbind( labs_during, int_labs_during)
        }
    }
    
    labs <- rbind( labs_adm, labs_during)
    
    output$plot1 <- renderPlot({
        labs_to_plot <- labs %>%
            filter( variant_misc == input$variant, 
                    time == input$time)
        
        ggplot(labs_to_plot, aes(site, fill = site)) +
            geom_boxplot(
                stat = "identity",
                aes(lower  = q25_value,
                    upper  = q75_value,
                    middle = median_value,
                    ymin   = min_value,
                    ymax   = max_value ) 
            ) +
            facet_wrap( .~ label, scales = "free", labeller = label_wrap_gen(multi_line = TRUE) )+
            theme(axis.text=element_text(size=14),
                  axis.text.x = element_text(size=14, hjust = 1), 
                  strip.text.x = element_text(size = 16, face = "bold" ), 
                  axis.title.y = element_text(size = 14),
                  legend.title=element_text(size=14), 
                  legend.text=element_text(size=14), 
                  legend.position="top")+
            theme_bw()
        
    }, height = 1200, width = 1400)
    output$plot2 <- renderPlot({
        labs_to_plot <- labs %>%
            filter( variant_misc == input$variant, 
                    time == input$time)
        
        ggplot(labs_to_plot,  aes(x=site, y=percs, fill=site)) +
            geom_bar(stat="identity") +
            facet_wrap( .~ label, scales = "free", labeller = label_wrap_gen(multi_line = TRUE) )+
            theme(axis.text=element_text(size=14),
                  axis.text.x = element_text(size=14, hjust = 1), 
                  strip.text.x = element_text(size = 16, face = "bold" ), 
                  axis.title.y = element_text(size = 14),
                  legend.title=element_text(size=14), 
                  legend.text=element_text(size=14), 
                  legend.position="top")+
            theme_bw()
        
        
    }, height = 1200, width = 1400)
    
    ### labs meta Analysis
    output$forestsiteLab <- renderPlot({
        
      ###load the results from the actual meta-analysis
      if( input$timeLabMeta == "admission"){
        load("labs_at_admission_outputs_to_plot.RData")
        
        if( input$variantLabMeta == "Delta"){
          selection <- paste0( input$labMeta, "_alphaDelta")
          newTitle <- paste0(input$labMeta, " at Admission \n (delta vs. alpha)")
        }else if( input$variantLabMeta == "Omicron"){
          selection <- paste0( input$labMeta, "_alphaOmicron")
          newTitle <- paste0(input$labMeta, " at Admission \n (omicron vs. alpha)")
        }
        toplot <- labs_at_admission_outputs_to_plot[[selection]]
        
      }
      if( input$timeLabMeta == "during"){
        load("labs_during_admission_outputs_to_plot.RData")
        
        if( input$variantLabMeta == "Delta"){
          selection <- paste0( input$labMeta, "_alphaDelta")
          newTitle <- paste0(input$labMeta, " During Hospitalization \n (delta vs. alpha)")
        }else if( input$variantLabMeta == "Omicron"){
          selection <- paste0( input$labMeta, "_alphaOmicron")
          newTitle <- paste0(input$labMeta, " During Hospitalization \n (omicron vs. alpha)")
        }
        
        toplot <- labs_during_admission_outputs_to_plot[[selection]]
      }
      
      meta::forest( toplot)
      mtext(newTitle, cex = 1.5)

    }, height = 500, width = 900)
    
    
    
    ### compare categorical var in table 1 and 3 across sites 
    output$plot3_perc <- renderPlot({
        
        allCombined_t3 <- complete_table3 %>%
          filter( site != "CHOP") %>%
            group_by( categories, variant_misc ) %>%
            summarise( n = sum(n), 
                       total = sum( total ) ) %>%
            mutate( site = "ALL Combined")
        
        complete_table3 <- rbind( complete_table3, allCombined_t3 ) 

        if( input$variantC == "all"){
            toPlot <- complete_table3 %>%
                mutate( perc = n/total*100)
            
            ggplot(toPlot, aes(site, categories, fill= perc)) +
                geom_tile() + 
                facet_wrap( ~ variant_misc ) +
                theme_bw() +
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }else{
            toPlot <- complete_table3 %>%
                mutate( perc = n/total*100) %>% 
                filter( variant_misc == input$variantC)
            
            ggplot(toPlot, aes(site, categories, fill= perc)) +
                geom_tile() + 
                theme_bw() +
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }
    }, height = 900, width = 1100)
    
    output$plot1_perc <- renderPlot({
        
        complete_table1 <- complete_table1 
      
        allCombined_t1 <- complete_table1 %>% 
            group_by( categories, variant_misc ) %>%
            summarise( n = sum(n), 
                       total = sum( total ) ) %>%
            mutate( site = "ALL Combined")
        
        complete_table1 <- rbind( complete_table1, allCombined_t1 ) %>%
          filter( site != "CHOP") 
        
        if( input$variantC == "all"){
            toPlot <- complete_table1 %>%
                mutate( perc = n/total*100) %>%
                filter( categories %in% categoriesToVisualize )
            
            ggplot(toPlot, aes(site, categories, fill= perc)) +
                geom_tile() + 
                facet_wrap( ~ variant_misc ) +
                theme_bw() +
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }else{
            toPlot <- complete_table1 %>%
                mutate( perc = n/total*100) %>% 
                filter( variant_misc == input$variantC) %>%
                filter( categories %in% categoriesToVisualize )
            
            
            ggplot(toPlot, aes(site, categories, fill= perc)) +
                geom_tile() + 
                theme_bw() +
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }
    }, height = 900, width = 1100)
    
    ### categorical meta Analysis
    output$forestsiteCategories <- renderPlot({
      
      load("exactMethod_categories.RData")
      load( "listOfVariablesForMetaAnalysis.RData")
      names(res.exact.delta) <- list_to_evaluate$ClinicalCharacteristic_categories
      names(res.exact.omicron) <- list_to_evaluate$ClinicalCharacteristic_categories
      
      if(input$variantCategoryMeta == "Delta"){
        
        meta_analysis <- as.data.frame( res.exact.delta[[input$var_category]]$study.ci, row.names = FALSE )
        summaryValues <- as.data.frame(res.exact.delta[[input$var_category]]$ci.fixed)
        newRow <- c( est  = summaryValues$`inverse-variance`[1], 
                     lower = summaryValues$`inverse-variance`[2], 
                     upper = summaryValues$`inverse-variance`[3],
                     site = "Summary",
                     val = paste0( "                                                                                  ",  round( summaryValues$`inverse-variance`[1], 3), " [", round( summaryValues$`inverse-variance`[2], 3), ", ", round( summaryValues$`inverse-variance`[3], 3), "]"))
        
        dt <- meta_analysis %>%
          mutate( site = c("BCH", "FRBDX", "H12O", "PittCHP", "RP401ped", "UMICH"), 
                  est =  est, 
                  lower = `lower CI`, 
                  upper = `upper CI`,  
                  val = paste0( "                                                                                  ",  round(est, 3), " [", round(`lower CI`, 3), ", ", round( `upper CI`, 3), "]")) %>%
          select( est, lower, upper,
                  site, -p, -limit,  val ) %>%
          rbind( newRow )
        
        colnames( dt )[5] = ""
        title_toPlot <- paste0( tolower(input$var_category), " (delta vs. alpha)")
        
      }else if( input$variantCategoryMeta == "Omicron"){
        
        meta_analysis <- as.data.frame( res.exact.omicron[[input$var_category]]$study.ci, row.names = FALSE )
        summaryValues <- as.data.frame(res.exact.omicron[[input$var_category]]$ci.fixed)
        newRow <- c( est  = summaryValues$`inverse-variance`[1], 
                     lower = summaryValues$`inverse-variance`[2], 
                     upper = summaryValues$`inverse-variance`[3],
                     site = "Summary",
                     val = paste0( "                                                                                  ",  round( summaryValues$`inverse-variance`[1], 3), " [", round( summaryValues$`inverse-variance`[2], 3), ", ", round( summaryValues$`inverse-variance`[3], 3), "]"))
        
        dt <- meta_analysis %>%
          mutate( site = c("BCH", "FRBDX", "H12O", "PittCHP", "RP401ped", "UMICH"), 
                  est =  est, 
                  lower = `lower CI`, 
                  upper = `upper CI`,  
                  val = paste0( "                                                                                  ",  round(est, 3), " [", round(`lower CI`, 3), ", ", round( `upper CI`, 3), "]")) %>%
          select( est, lower, upper,
                  site, -p, -limit,  val ) %>%
          rbind( newRow )
        
        colnames( dt )[5] = ""
        title_toPlot <- paste0( tolower(input$var_category), " (omicron vs. alpha)")
        
      }
      
      forestploter::forest(dt[,c(4:5)],
             est = as.numeric(dt$est),
             lower = as.numeric(dt$lower), 
             upper = as.numeric(dt$upper),
             ci_column = 2,
             ref_line = 0,
             xlim = c(-1, 1),
             ticks_at = c(-1,-0.5,0, 0.5, 1), 
             title = title_toPlot)
      
    }, height = 500, width = 900)
    
    ##### outcomes meta analysis
    output$forestsiteOutcomes <- renderPlot({
      
      load("exactMethod_outcomes.RData")
      load( "listOfVariablesForMetaAnalysis.RData")
      names(res.exact.delta) <- list_to_evaluate$Outcomes_all
      names(res.exact.omicron) <- list_to_evaluate$Outcomes_all
      
      if(input$variantOutcomeMeta == "Delta"){
        
        meta_analysis <- as.data.frame( res.exact.delta[[input$var_outcomes]]$study.ci, row.names = FALSE )
        summaryValues <- as.data.frame(res.exact.delta[[input$var_outcomes]]$ci.fixed)
        newRow <- c( est  = summaryValues$`inverse-variance`[1], 
                     lower = summaryValues$`inverse-variance`[2], 
                     upper = summaryValues$`inverse-variance`[3],
                     site = "Summary",
                     val = paste0( "                                                                                  ",  round( summaryValues$`inverse-variance`[1], 3), " [", round( summaryValues$`inverse-variance`[2], 3), ", ", round( summaryValues$`inverse-variance`[3], 3), "]"))
        
        dt <- meta_analysis %>%
          mutate( site = c("BCH", "FRBDX", "H12O", "PittCHP", "RP401ped", "UMICH"), 
                  est =  est, 
                  lower = `lower CI`, 
                  upper = `upper CI`,  
                  val = paste0( "                                                                                  ",  round(est, 3), " [", round(`lower CI`, 3), ", ", round( `upper CI`, 3), "]")) %>%
          select( est, lower, upper,
                  site, -p, -limit,  val ) %>%
          rbind( newRow )
        
        colnames( dt )[5] = ""
        title_toPlot <- paste0( tolower(input$var_outcomes), " (delta vs. alpha)")
        
      }else if( input$variantOutcomeMeta == "Omicron"){
        
        meta_analysis <- as.data.frame( res.exact.omicron[[input$var_outcomes]]$study.ci, row.names = FALSE )
        summaryValues <- as.data.frame(res.exact.omicron[[input$var_outcomes]]$ci.fixed)
        newRow <- c( est  = summaryValues$`inverse-variance`[1], 
                     lower = summaryValues$`inverse-variance`[2], 
                     upper = summaryValues$`inverse-variance`[3],
                     site = "Summary",
                     val = paste0( "                                                                                  ",  round( summaryValues$`inverse-variance`[1], 3), " [", round( summaryValues$`inverse-variance`[2], 3), ", ", round( summaryValues$`inverse-variance`[3], 3), "]"))
        
        dt <- meta_analysis %>%
          mutate( site = c("BCH", "FRBDX", "H12O", "PittCHP", "RP401ped", "UMICH"), 
                  est =  est, 
                  lower = `lower CI`, 
                  upper = `upper CI`,  
                  val = paste0( "                                                                                  ",  round(est, 3), " [", round(`lower CI`, 3), ", ", round( `upper CI`, 3), "]")) %>%
          select( est, lower, upper,
                  site, -p, -limit,  val ) %>%
          rbind( newRow )
        
        colnames( dt )[5] = ""
        title_toPlot <- paste0( tolower(input$var_outcomes), " (omicron vs. alpha)")
        
      }
      
      forestploter::forest(dt[,c(4:5)],
                           est = as.numeric(dt$est),
                           lower = as.numeric(dt$lower), 
                           upper = as.numeric(dt$upper),
                           ci_column = 2,
                           ref_line = 0,
                           xlim = c(-1, 1),
                           ticks_at = c(-1,-0.5,0, 0.5, 1), 
                           title = title_toPlot)
      
    }, height = 500, width = 900)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
