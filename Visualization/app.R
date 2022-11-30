library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(meta)


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
                                             choices = list("Total" = "total_n", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron"), 
                                             selected = "total_n"),
                                
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
                                             selected = "all"),
                                sliderInput("significant_threshold", "p-value threshold:",
                                            min = 0, max = 1, value = 0.1
                                ),
                            ),
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Table 3: percentages", plotOutput("plot3_perc"),  width = "100%"), 
                                            tabPanel("Table 3: p-values", plotOutput("plot3_pval"),  width = "100%"), 
                                            tabPanel("Table 1: percentages", plotOutput("plot1_perc"),  width = "100%"), 
                                            tabPanel("Table 1: p-values", plotOutput("plot1_pval"),  width = "100%")
                                )
                            ))
                        )
               ),
               tabPanel("Pooled proportion",
                        fluidRow(p(
                            "Meta-analysis per variant type and variable"
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                radioButtons("variants", label = h3("MISC Variant type:"),
                                             choices = list("Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron"), 
                                             selected = "Alpha"),
                                
                                radioButtons("var_category", label = h3("Chose a variable:"),
                                             choices = list("CARDIOVASCULAR SYMPTOMS"  =  "CARDIOVASCULAR SYMPTOMS",                
                                                            "GI SYMPTOMS"  =   "GI SYMPTOMS",                            
                                                            "LIVER DYSFUNCTION" =   "LIVER DYSFUNCTION",                     
                                                            "NEUROLOGIC SYMPTOMS"  = "NEUROLOGIC SYMPTOMS",                      
                                                            "RENAL DYSFUNCTION" =  "RENAL DYSFUNCTION",                
                                                            "RESPIRATORY SYMPTOMS"  = "RESPIRATORY SYMPTOMS", 
                                                            "GENERALIZED SYMPTOMS"   =  "GENERALIZED SYMPTOMS", 
                                                            "Kawasaki"   =   "Kawasaki", 
                                                            "ANTICOAGULATION THERAPY" = "Anticoagulation therapy", 
                                                            "CARDIACT ARREST" = "Cardiac arrest",                      
                                                            "COMPOSITE ADVERSE CARDIOVASCULAR OUTCOME" = "Composite adverse cardiovascular outcome",
                                                            "CORONARY ANEURYSM" = "Coronary aneurysm",  
                                                            "DIURETUC THERAPY" =  "Diuretic therapy",                          
                                                            "ECMO" = "ECMO", 
                                                            "IN ICU" =  "in_icu",                                
                                                            "INOTROPIC SUPPORT"   =  "Inotropic support",                     
                                                            "INVASIVE MONITORING (ARTERIAL LINE)"  =   "Invasive monitoring (arterial line)" ,    
                                                            "OXYGEN SUPPLEMENTATION"  =  "Oxygen supplementation",                 
                                                            "SEDATION OR MUSCLE RELAXANT"  = "Sedation or muscle relaxant" ), selected = "CARDIOVASCULAR SYMPTOMS")
                            ),
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Forest plot by site", plotOutput("forestsite"),  width = "100%"),
                                            tabPanel("Forest plot by country", plotOutput("forestcountry"),  width = "100%")
                                )
                            ))
                        )
               ), 
               tabPanel("Washout vs. non-washout: table 1 & 3",
                         fluidRow(p(
                             "Here using BCH as a proof of concept, we compare how the results change based on the variant cut-off date"
                         ),
                         br(),
                         p(tags$img(src="methods.png",width="1750px",height="400px")),
                         br(),
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 radioButtons("variantW", label = h3("MISC Variant type:"),
                                              choices = list("Total" = "total", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron", "All" = "all"), 
                                              selected = "all"),
                                 sliderInput("significant_thresholdW", "p-value threshold:",
                                             min = 0, max = 1, value = 0.1
                                 ),
                             ),
                             mainPanel(
                                 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Categorical Table1 percentages differentes", plotOutput("plot_w1"),  width = "100%"), 
                                             tabPanel("Categorical Table1 comparison", plotOutput("plot_w2"),  width = "75%"),
                                             tabPanel("Categorical Table1 p-value comparison", plotOutput("plot_w3"),  width = "100%"), 
                                             tabPanel("Table3 percentages differentes", plotOutput("plot_w4"),  width = "100%"), 
                                             tabPanel("Table3 comparison", plotOutput("plot_w5"),  width = "75%"),
                                             tabPanel("Table3 p-value comparison", plotOutput("plot_w6"),  width = "100%")
                                 )
                                 
                             ))
                         )
               ),
               
    )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output) {
    
    #### ICD code QC
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
                    dplyr::filter( perc >= input$percentage ) %>% 
                    dplyr::select( ICDcode, perc )
                colnames( site_codes)[2] <- paste0( site_id, "_percPatients") 
                rm( diag_sum )
            }else{
                int_site_codes <- diag_sum %>%
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
        compareTable
        
    }, options = list("pageLength" = 50),  filter = "top", rownames = FALSE))
    
    #### Labs QC
    at_admission_files <- list.files("./atAdmission/")
    during_admission_files <- list.files("./duringAdmission/")
    totalN <- read.delim("./sites_totalPatients.txt")
    
    for( i in 1:length( at_admission_files )){
        site_id <- sapply( strsplit(at_admission_files[i], "[_]"), '[', 1)
        
        load( paste0("./atAdmission/", at_admission_files[i]))
        n_total <- totalN %>% filter( site == site_id )
        
        if( i ==1 ){
            
            labs_adm <- table2_admission %>%
                mutate( percs = round(n_patients / n_total$total_n *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site = paste0( site_id, "\n (", n_total$total_n, ")"),
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "admission")
            
            rm(table2_admission)
        }else{
            int_labs_adm <- table2_admission %>%
                mutate( percs = round(n_patients / n_total$total_n *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site = paste0( site_id, "\n (", n_total$total_n, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "admission" )
            labs_adm <- rbind( labs_adm, int_labs_adm)
        }
    }
    
    for( i in 1:length( during_admission_files )){
        site_id <- sapply( strsplit(during_admission_files[i], "[_]"), '[', 1)
        load( paste0("./duringAdmission/", during_admission_files[i]))
        n_total <- totalN %>% filter( site == site_id )
        
        if( i ==1 ){
            
            labs_during <- table2_during %>%
                mutate( percs = round(n_patients / n_total$total_n *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site = paste0( site_id, "\n (", n_total$total_n, ")"), 
                        site_label =  paste0( site_id, "\n (", n_patients, ")"),
                        time = "during")
            
            rm(table2_during)
        }else{
            int_labs_during <- table2_during %>%
                mutate( percs = round(n_patients / n_total$total_n *100, 2),
                        label = paste0( variableName, " (", units, ")"), 
                        site = paste0( site_id, "\n (", n_total$total_n, ")"), 
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
        
        ggplot(labs_to_plot, aes(site_label, fill = site)) +
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
        
        ggplot(labs_to_plot,  aes(x=site_label, y=percs, fill=site)) +
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
    
    
    ##### Forest plot 
    pvaluesFiles3 <- list.files( path = "./table3s/", pattern = "3.txt")
    rdataFiles3 <- list.files( path = "./table3s/", pattern = "table3.RData")
    
    for( i in 1:length(pvaluesFiles3)){
        print(i)
        siteid <- unlist(strsplit(x = pvaluesFiles3[i], split = "_"))[1]
        p_values <- read.delim(paste0( "./table3s/", pvaluesFiles3[i]))
        
        variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
        colnames(variantsN) <- c("variant_misc", "total")
        variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
        
        variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(p_values)[2], "[.]"), tail, 1))
        variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(p_values)[3], "[.]"), tail, 1))
        variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(p_values)[4], "[.]"), tail, 1))
        variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(p_values)[5], "[.]"), tail, 1))
        variantsN$site <- siteid
        colnames(p_values) <- c("categories", "Alpha", "Delta", "Omicron", "total", "p.value")
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
        select( categories, p.value, site )
    
    rm(variantsN)
    rm(p_values)
    
    for( i in 1:length(rdataFiles3)){
        print(i)
        siteid <- unlist(strsplit(x = rdataFiles3[i], split = "_"))[1]
        load(paste0( "./table3s/", rdataFiles3[i]))
        table3$site <- siteid
        
        if( i == 1){
            complete_table3 <- table3
        }else{
            complete_table3 <- rbind(complete_table3, table3)
        }
    }
    
    complete_table3 <- complete_table3 %>%
        left_join( pvalData3, by = c("categories", "site") ) %>%
        left_join( variantsData3, by = c("variant_misc", "site"))
    
    ### table 1 categorical
    pvaluesFiles1 <- list.files( path = "./table1s/", pattern = "1.txt")
    rdataFiles1 <- list.files( path = "./table1s/", pattern = "table1Categorical.RData")
    
    for( i in 1:length(pvaluesFiles1)){
        print(i)
        siteid <- unlist(strsplit(x = pvaluesFiles1[i], split = "_"))[1]
        p_values <- read.delim(paste0( "./table1s/", pvaluesFiles1[i]))
        
        variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
        colnames(variantsN) <- c("variant_misc", "total")
        variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
        
        variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(p_values)[2], "[.]"), tail, 1))
        variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(p_values)[3], "[.]"), tail, 1))
        variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(p_values)[4], "[.]"), tail, 1))
        variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(p_values)[5], "[.]"), tail, 1))
        variantsN$site <- siteid
        colnames(p_values) <- c("categories", "Alpha", "Delta", "Omicron", "total", "p.value")
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
        select( categories, p.value, site )
    
    rm(variantsN)
    rm(p_values)
    
    for( i in 1:length(rdataFiles1)){
        print(i)
        siteid <- unlist(strsplit(x = rdataFiles1[i], split = "_"))[1]
        load(paste0( "./table1s/", rdataFiles1[i]))
        table1_categorical$site <- siteid
        
        if( i == 1){
            complete_table1 <- table1_categorical
        }else{
            complete_table1 <- rbind(complete_table1, table1_categorical)
        }
    }
    
    complete_table1 <- complete_table1 %>%
        left_join( pvalData1, by = c("categories", "site") ) %>%
        left_join( variantsData1, by = c("variant_misc", "site"))
    
    
    ### change renal involvement by renal disfunction
    complete_table1$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", complete_table1$categories)
    complete_table1$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", complete_table1$categories)
    complete_table1$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", complete_table1$categories)
    
    categoriesToVisualize <- c("CARDIOVASCULAR SYMPTOMS" , "GI SYMPTOMS", "LIVER DYSFUNCTION", 
                               "NEUROLOGIC SYMPTOMS", "RENAL DYSFUNCTION", "RESPIRATORY SYMPTOMS", 
                               "GENERALIZED SYMPTOMS", "Kawasaki" )
    
    
    output$forestsite <- renderPlot({
        
        complete_table1_forest <- complete_table1 %>%
            filter( categories %in% categoriesToVisualize)
        
        allDataForest <- rbind( complete_table3, complete_table1_forest )
            
        toForestPlot <- allDataForest %>%
            filter( categories == input$var_category &
                        variant_misc == input$variants )
        
        mtprop <- metaprop(event=n, n=total, studlab=site, data=toForestPlot, method = "GLMM", sm = "PLOGIT",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
        
        forest(mtprop)
    }, height = 500, width = 900)
    
    output$forestcountry<- renderPlot({
        
        complete_table1 <- complete_table1 %>%
            filter( categories %in% categoriesToVisualize)
        
        countryMap <- read.delim("./siteCountry.txt")
        
        allDataForestCountry <- rbind( complete_table3, complete_table1 ) %>%
            left_join( countryMap )
        
        toForestPlot <- allDataForestCountry %>%
            filter( categories == input$var_category &
                        variant_misc == input$variants ) %>%
            group_by( Country ) %>%
            summarise( n = sum( n ), 
                       total = sum( total ))
        
        mtprop <- metaprop(event=n, n=total, studlab=Country, data=toForestPlot, method = "GLMM", sm = "PLOGIT",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
        
        forest(mtprop)
    }, height = 500, width = 900)
    
    ### compare categorical var in table 1 and 3 across sites 
    output$plot3_perc <- renderPlot({
        
        allCombined_t3 <- complete_table3 %>% 
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
    
    output$plot3_pval <- renderPlot({
        
        allPvalue <- complete_table3 %>%
            mutate( significant = ifelse( p.value <= input$significant_threshold, 1, 0)) %>%
            ungroup() %>%
            select( categories, p.value, site, significant ) %>%
            unique()
        
        toPlot <- allPvalue
        ggplot(toPlot, aes(site, categories, fill= significant)) +
            geom_tile() + 
            theme_bw() +
            geom_text(aes(label = ifelse( significant == 1, p.value, ""))) +
            theme(axis.text=element_text(size=10),
                  axis.text.x = element_text(size=10, hjust = 1), 
                  axis.text.y = element_text(size=12, hjust = 1), 
                  strip.text.x = element_text(size = 12, face = "bold" ), 
                  axis.title.y = element_text(size = 6)
            )+
            scale_fill_gradient2(low="white", high="yellow") +
            guides( fill = "none")
        
    }, height = 900, width = 1100)
    
    output$plot1_perc <- renderPlot({
        
        allCombined_t1 <- complete_table1 %>% 
            group_by( categories, variant_misc ) %>%
            summarise( n = sum(n), 
                       total = sum( total ) ) %>%
            mutate( site = "ALL Combined")
        
        complete_table1 <- rbind( complete_table1, allCombined_t1 )
        
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
    
    output$plot1_pval <- renderPlot({
        
        allPvalue <- complete_table1 %>%
            filter( categories %in% categoriesToVisualize ) %>%
            mutate( significant = ifelse( p.value <= input$significant_threshold, 1, 0)) %>%
            ungroup() %>%
            select( categories, p.value, site, significant ) %>%
            unique()
        
        toPlot <- allPvalue
        ggplot(toPlot, aes(site, categories, fill= significant)) +
            geom_tile() + 
            theme_bw() +
            geom_text(aes(label = ifelse( significant == 1, p.value, ""))) +
            theme(axis.text=element_text(size=10),
                  axis.text.x = element_text(size=10, hjust = 1), 
                  axis.text.y = element_text(size=12, hjust = 1), 
                  strip.text.x = element_text(size = 12, face = "bold" ), 
                  axis.title.y = element_text(size = 6)
            )+
            scale_fill_gradient2(low="white", high="yellow") +
            guides( fill = "none")
        
    }, height = 900, width = 1100)
    
    ### washout vs. non washout 
    ### table 1s
    
    pvalues_bch_original <- read.delim("./BCH Output/BCH_table1.txt") %>%
        select( categories, p.value)
    
    load("./BCH Output/BCH_table1Categorical.RData")
    bch_original <- table1_categorical %>%
        left_join( pvalues_bch_original )
    bch_original$type <- "bch_original"
    bch_original$total <- ifelse( bch_original$variant_misc == "Alpha", 98, 
                                  ifelse( bch_original$variant_misc == "Delta", 18,
                                          ifelse( bch_original$variant_misc == "Omicron", 21, 137))) 
    
    bch_original$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", bch_original$categories)
    bch_original$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", bch_original$categories)
    bch_original$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", bch_original$categories)
    bch_original$categories <- gsub("Kawasaki", "KAWASAKI", bch_original$categories)
    
    
    
    
    rm( table1_categorical )
    
    bch_pvalues_remove <- read.delim("./washoutTestBCH/outputMISC_washout14/BCH_table1.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout14/BCH_table1Categorical.RData")
    bch_removing <- table1_categorical %>%
        left_join( bch_pvalues_remove )
    bch_removing$type <- "remove"
    bch_removing$total <- ifelse( bch_removing$variant_misc == "Alpha", 98, 
                                  ifelse( bch_removing$variant_misc == "Delta", 15,
                                          ifelse( bch_removing$variant_misc == "Omicron", 13, 126)))
    
    rm(table1_categorical)
    
    pvalues_bch_replace_earlier <- read.delim("./washoutTestBCH/outputMISC_washout_replaceEarlier14/BCH_table1.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout_replaceEarlier14/BCH_table1Categorical.RData")
    bch_replace_earlier <- table1_categorical  %>%
        left_join( pvalues_bch_replace_earlier )
    bch_replace_earlier$type <- "bch_replace_earlier"
    bch_replace_earlier$total <- ifelse( bch_replace_earlier$variant_misc == "Alpha", 98, 
                                         ifelse( bch_replace_earlier$variant_misc == "Delta", 16,
                                                 ifelse( bch_replace_earlier$variant_misc == "Omicron", 23, 137)))
    
    rm(table1_categorical)
    
    pvalues_bch_replace_later <- read.delim("./washoutTestBCH/outputMISC_washout_replaceLater14/BCH_table1.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout_replaceLater14/BCH_table1Categorical.RData")
    bch_replace_later <- table1_categorical  %>%
        left_join( pvalues_bch_replace_later )
    bch_replace_later$type <- "bch_replace_later"
    rm(table1_categorical)
    bch_replace_later$total <- ifelse( bch_replace_later$variant_misc == "Alpha", 99, 
                                       ifelse( bch_replace_later$variant_misc == "Delta", 25,
                                               ifelse( bch_replace_later$variant_misc == "Omicron", 13, 137)))
    
    bch_all <- rbind( bch_original, bch_removing, bch_replace_later, bch_replace_earlier )
    
    bch_all$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", bch_all$categories)
    bch_all$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", bch_all$categories)
    bch_all$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", bch_all$categories)
    bch_all$categories <- gsub("Kawasaki", "KAWASAKI", bch_all$categories)
    
    categoriesToVisualize <- c("CARDIOVASCULAR SYMPTOMS" , "GI SYMPTOMS", "LIVER DYSFUNCTION", 
                               "NEUROLOGIC SYMPTOMS", "RENAL DYSFUNCTION", "RESPIRATORY SYMPTOMS", 
                               "GENERALIZED SYMPTOMS", "KAWASAKI" )
    
    bch_allData <- bch_all %>%
        filter( categories %in% categoriesToVisualize) %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc, type )
    
    bch_originalToCompare <- bch_original %>%
        filter( categories %in% categoriesToVisualize) %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc )
    
    bch_allToCompare <-  rbind( bch_removing, bch_replace_later, bch_replace_earlier ) 
    
    bch_allToCompare$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", bch_allToCompare$categories)
    bch_allToCompare$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", bch_allToCompare$categories)
    bch_allToCompare$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", bch_allToCompare$categories)
    bch_allToCompare$categories <- gsub("Kawasaki", "KAWASAKI", bch_allToCompare$categories)
    
    
    bch_allToCompare <- bch_allToCompare %>%
        filter( categories %in% categoriesToVisualize) %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc, type ) %>%
        left_join( bch_originalToCompare,by = c( "categories", "variant_misc" ),suffix = c(".washout", ".bch_original")) %>%
        # i swapped this so that a positive number means the new washout approach is adding patients.
        mutate( diff = perc.washout - perc.bch_original) %>%
        select( categories, variant_misc, diff, type) %>%
        unique()
    
    ### table 3s
    pvalues_bch_original3 <- read.delim("./BCH Output/BCH_table3.txt") %>%
        select( categories, p.value)
    
    load("./BCH Output/BCH_table3.RData")
    bch_original3 <- table3 %>%
        left_join( pvalues_bch_original3 )
    bch_original3$type <- "bch_original"
    bch_original3$total <- ifelse( bch_original3$variant_misc == "Alpha", 98, 
                                   ifelse( bch_original3$variant_misc == "Delta", 18,
                                           ifelse( bch_original3$variant_misc == "Omicron", 21, 137))) 
    
    
    
    rm( table3 )
    rm( pvalues_bch_original3)
    
    bch_pvalues_remove3 <- read.delim("./washoutTestBCH/outputMISC_washout14/BCH_table3.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout14/BCH_table3.RData")
    bch_removing3 <- table3 %>%
        left_join( bch_pvalues_remove3 )
    bch_removing3$type <- "remove"
    bch_removing3$total <- ifelse( bch_removing3$variant_misc == "Alpha", 98, 
                                   ifelse( bch_removing3$variant_misc == "Delta", 15,
                                           ifelse( bch_removing3$variant_misc == "Omicron", 13, 126)))
    
    rm( table3 )
    rm( bch_pvalues_remove3)
    
    pvalues_bch_replace_earlier3 <- read.delim("./washoutTestBCH/outputMISC_washout_replaceEarlier14/BCH_table3.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout_replaceEarlier14/BCH_table3.RData")
    bch_replace_earlier3 <- table3  %>%
        left_join( pvalues_bch_replace_earlier3 )
    bch_replace_earlier3$type <- "bch_replace_earlier"
    bch_replace_earlier3$total <- ifelse( bch_replace_earlier3$variant_misc == "Alpha", 98, 
                                          ifelse( bch_replace_earlier3$variant_misc == "Delta", 16,
                                                  ifelse( bch_replace_earlier3$variant_misc == "Omicron", 23, 137)))
    
    rm( table3 )
    rm( pvalues_bch_replace_earlier3)
    
    pvalues_bch_replace_later3 <- read.delim("./washoutTestBCH/outputMISC_washout_replaceLater14/BCH_table3.txt") %>%
        select( categories, p.value)
    load("./washoutTestBCH/outputMISC_washout_replaceLater14/BCH_table3.RData")
    bch_replace_later3 <- table3  %>%
        left_join( pvalues_bch_replace_later3 )
    bch_replace_later3$type <- "bch_replace_later"
    rm(table3)
    rm( pvalues_bch_replace_later3)
    
    bch_replace_later3$total <- ifelse( bch_replace_later3$variant_misc == "Alpha", 99, 
                                        ifelse( bch_replace_later3$variant_misc == "Delta", 25,
                                                ifelse( bch_replace_later3$variant_misc == "Omicron", 13, 137)))
    
    bch_all3 <- rbind( bch_original3, bch_removing3, bch_replace_later3, bch_replace_earlier3 )
    
    bch_allData3 <- bch_all3 %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc, type )
    
    bch_originalToCompare3 <- bch_original3 %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc )
    
    bch_allToCompare3 <-  rbind( bch_removing3, bch_replace_later3, bch_replace_earlier3 ) %>%
        mutate( perc = round( n/ total * 100, 2)) %>%
        select( categories, variant_misc, perc, type ) %>%
        left_join( bch_originalToCompare3,by = c( "categories", "variant_misc" ),suffix = c(".washout", ".bch_original")) %>%
        mutate( diff = perc.washout - perc.bch_original) %>%
        select( categories, variant_misc, diff, type) %>%
        unique()
    
    
    
    
    output$plot_w2 <- renderPlot({
        
        if( input$variantW == "all"){
            toPlot <- bch_allData
            ggplot(toPlot, aes(type, categories, fill= perc)) +
                geom_tile() + 
                facet_wrap( ~ variant_misc ) +
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=4, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                theme_bw() +
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }else{
            toPlot <- bch_allData %>% 
                filter( variant_misc == input$variantW) 
            ggplot(toPlot, aes(type, categories, fill= perc)) +
                geom_tile() + 
                geom_text(aes(label = round(perc, 1))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=4, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                theme_bw() +
                scale_fill_gradient2(low="lightgrey", high="darkgreen")
            
            
        }
    }, height = 900, width = 1100)
    output$plot_w1 <- renderPlot({
        
        if( input$variantW == "all"){
            toPlot <- bch_allToCompare
            ggplot(toPlot, aes(type, categories, fill= diff)) +
                geom_tile() + 
                facet_wrap( ~ variant_misc ) +
                geom_text(aes(label = round(diff, 3))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=4, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                theme_bw() +
                scale_fill_gradient2(low="darkorange", high="blue")
            
        }else{
            toPlot <- bch_allToCompare %>%
                filter( variant_misc == input$variantW) %>%
                select( categories, variant_misc, type, diff ) %>%
                unique()
            
            ggplot(toPlot, aes(type, categories, fill= diff)) +
                geom_tile(aes(fill = diff)) +
                geom_text(aes(label = round(diff, 3))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=4, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                theme_bw() +
                scale_fill_gradient2(low="darkorange", high="blue")
            
        }
        
    }, height = 900, width = 1100)
    
    output$plot_w3 <- renderPlot({
        
        bch_allPvalue <- bch_all %>%
            filter( categories %in% categoriesToVisualize) %>%
            mutate( significant = ifelse( p.value <= input$significant_thresholdW, 1, 0)) %>%
            ungroup() %>%
            select( categories, p.value, type, significant ) %>%
            unique()
        
        toPlot <- bch_allPvalue
        ggplot(toPlot, aes(type, categories, fill= significant)) +
            geom_tile() + 
            geom_text(aes(label = ifelse( significant == 1, p.value, ""))) +
            theme(axis.text=element_text(size=10),
                  axis.text.x = element_text(size=10, hjust = 1), 
                  axis.text.y = element_text(size=4, hjust = 1), 
                  strip.text.x = element_text(size = 12, face = "bold" ), 
                  axis.title.y = element_text(size = 6)
            )+
            theme_bw()+
            scale_fill_gradient2(low="white", high="yellow") +
            guides( fill = "none")
        
    }, height = 900, width = 1100)
    
    output$plot_w5 <- renderPlot({
        
        if( input$variantW == "all"){
            toPlot <- bch_allData3
            
            ggplot(toPlot, aes(type, categories, fill= perc)) +
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
            toPlot <- bch_allData3 %>% 
                filter( variant_misc == input$variantW) 
            ggplot(toPlot, aes(type, categories, fill= perc)) +
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
    output$plot_w4 <- renderPlot({
        
        if( input$variantW == "all"){
            toPlot <- bch_allToCompare3 
            ggplot(toPlot, aes(type, categories, fill= diff)) +
                geom_tile() + 
                facet_wrap( ~ variant_misc ) +
                geom_text(aes(label = round(diff, 3))) +
                theme_bw() +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=12, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="darkorange", high="blue")
            
        }else{
            toPlot <- bch_allToCompare3 %>%
                filter( variant_misc == input$variantW) %>%
                select( categories, variant_misc, type, diff ) %>%
                unique()
            
            ggplot(toPlot, aes(type, categories, fill= diff)) +
                geom_tile(aes(fill = diff)) +
                theme_bw() +
                geom_text(aes(label = round(diff, 3))) +
                theme(axis.text=element_text(size=10),
                      axis.text.x = element_text(size=10, hjust = 1), 
                      axis.text.y = element_text(size=12, hjust = 1), 
                      strip.text.x = element_text(size = 12, face = "bold" ), 
                      axis.title.y = element_text(size = 6),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=10), 
                      legend.position="top")+
                scale_fill_gradient2(low="darkorange", high="blue")
            
        }
        
    }, height = 900, width = 1100)
    
    output$plot_w6 <- renderPlot({
        
        bch_allPvalue3 <- bch_all3 %>%
            mutate( significant = ifelse( p.value <= input$significant_thresholdW, 1, 0)) %>%
            ungroup() %>%
            select( categories, p.value, type, significant ) %>%
            unique()
        
        toPlot <- bch_allPvalue3
        ggplot(toPlot, aes(type, categories, fill= significant)) +
            geom_tile() + 
            theme_bw() +
            geom_text(aes(label = ifelse( significant == 1, p.value, ""))) +
            theme(axis.text=element_text(size=10),
                  axis.text.x = element_text(size=10, hjust = 1), 
                  axis.text.y = element_text(size=12, hjust = 1), 
                  strip.text.x = element_text(size = 12, face = "bold" ), 
                  axis.title.y = element_text(size = 6)
            )+
            scale_fill_gradient2(low="white", high="yellow") +
            guides( fill = "none")
        
    }, height = 900, width = 1100)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
