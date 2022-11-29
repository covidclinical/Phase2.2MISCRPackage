library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(meta)

ui <- fluidPage(
    
    navbarPage(
        title = "4CE MISC", id="main_panel",
        theme = shinythemes::shinytheme("cerulean")),
    
    titlePanel("pooled proportions"),
    
    
    sidebarLayout(
        
        sidebarPanel(
          
          radioButtons("variant", label = h3("MISC Variant type:"),
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
                                       "SEDATION OR MUSCLE RELAXANT"  = "Sedation or muscle relaxant" ), selected = "CARDIOVASCULAR SYMPTOMS")),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Forest plot", plotOutput("plot"))
            )
        )
    )
)

server <- function(input, output) {
  
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

    if( i == 1){
      variantsData3 <- variantsN
    }else{
      variantsData3 <- rbind(variantsData3, variantsN)
      
    }
  }
  
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
    
    if( i == 1){
      variantsData1 <- variantsN
    }else{
      variantsData1 <- rbind(variantsData1, variantsN)
      
    }
  }
  
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
  
  ### change renal involvement by renal disfunction
  complete_table1$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", complete_table1$categories)
  complete_table1$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", complete_table1$categories)
  complete_table1$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", complete_table1$categories)
  
  categoriesToVisualize <- c("CARDIOVASCULAR SYMPTOMS" , "GI SYMPTOMS", "LIVER DYSFUNCTION", 
                             "NEUROLOGIC SYMPTOMS", "RENAL DYSFUNCTION", "RESPIRATORY SYMPTOMS", 
                             "GENERALIZED SYMPTOMS", "Kawasaki" )
  
  
  complete_table1 <- complete_table1 %>%
    left_join( variantsData1, by = c("variant_misc", "site")) %>%
    filter( categories %in% categoriesToVisualize)
  ###
  
  allData <- rbind( complete_table3, complete_table1 )
  
  output$plot <- renderPlot({
      
     toPlot <- allData %>%
       filter( categories == input$var_category &
               variant_misc == input$variant )
     
     mtprop <- metaprop(event=n, n=total, studlab=site, data=toPlot, method = "GLMM", sm = "PLOGIT",
                        comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
     
     forest(mtprop)
     }, height = 500, width = 900)
    
}

# Create Shiny app ----
shinyApp(ui, server)
