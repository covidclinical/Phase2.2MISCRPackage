library(shiny)
library(dplyr)
library(DT)
library(shinythemes)

ui <- fluidPage(
    
    navbarPage(
        title = "4CE MISC: ICD codes QC", id="main_panel",
        theme = shinythemes::shinytheme("cerulean")),

    sidebarLayout(
        sidebarPanel(
            sliderInput("perc",
                        "Percentage of patients:",
                        min = 0,
                        max = 100,
                        value = 10)
        ),

        DT::dataTableOutput("table")
    )
)

server <- function(input, output) {
    
    output$table <- DT::renderDataTable(DT::datatable({
        
        ### our gold standar codes 
        clinicalCodes <- read.delim("./clinicalCharacteristics.txt", sep = "\t") %>%
            mutate( ICDcode = gsub("[.]", "", concept_code)) %>%
            select( -concept_code )
        
        ### read the total patient file 
        totalN <- read.delim("./sites_totalPatients.txt")
        
        ### read the ICD files from each site
        files <- list.files("./sitesICDcodes/")
        
        for( i in 1:length( files )){
            site_id <- sapply( strsplit(files[i], "[_]"), '[', 1)
            n_total <- totalN %>% filter( site == site_id )
            load( paste0("./sitesICDcodes/", files[i]))
            
            if( i ==1 ){
                site_codes <- diag_sum %>%
                    dplyr::mutate( perc = round(n_patients / n_total$total_n *100, 2), 
                                   site = site_id, 
                                   ICDcode = gsub("[.]", "", concept_code)) %>%
                    dplyr::filter( perc >= input$perc ) %>% 
                    dplyr::select( ICDcode, perc )
                colnames( site_codes)[2] <- paste0( site_id, "_percPatients") 
                rm( diag_sum )
            }else{
                int_site_codes <- diag_sum %>%
                    dplyr::mutate( perc = round(n_patients / n_total$total_n *100, 2), 
                                   site = site_id, 
                                   ICDcode = gsub("[.]", "", concept_code)) %>%
                    dplyr::filter( perc >=  input$perc ) %>% 
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
        compareTable
        
    }, options = list("pageLength" = 50),  filter = "top", rownames = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
