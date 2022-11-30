library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
    
    navbarPage(
        title = "4CE MISC compare results across sites", id="main_panel",
        theme = shinythemes::shinytheme("cerulean")),
    
    titlePanel("Results across sites"),
    
    
    sidebarLayout(
        
        sidebarPanel(
            
            radioButtons("variant", label = h3("MISC Variant type:"),
                         choices = list("Total" = "total", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron", "All" = "all"), 
                         selected = "all"),
            sliderInput("significant_threshold", "p-value threshold:",
                        min = 0, max = 1, value = 0.1
            ),
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Table 3: percentages", plotOutput("plot1"),  width = "100%"), 
                        tabPanel("Table 3: p-values", plotOutput("plot2"),  width = "100%"), 
                        tabPanel("Table 1: percentages", plotOutput("plot3"),  width = "100%"), 
                        tabPanel("Table 1: p-values", plotOutput("plot4"),  width = "100%")
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
  
  allCombined_t3 <- complete_table3 %>% 
    group_by( categories, variant_misc ) %>%
    summarise( n = sum(n), 
               total = sum( total ) ) %>%
    mutate( site = "ALL Combined")
  
  complete_table3 <- rbind( complete_table3, allCombined_t3 )
  
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
  
  complete_table1$categories <- gsub("RENAL INVOLVEMENT", "RENAL DYSFUNCTION", complete_table1$categories)
  complete_table1$categories <- gsub("LIVER INVOLVEMENT", "LIVER DYSFUNCTION", complete_table1$categories)
  complete_table1$categories <- gsub("generalized symptoms", "GENERALIZED SYMPTOMS", complete_table1$categories)
  
  categoriesToVisualize <- c("CARDIOVASCULAR SYMPTOMS" , "GI SYMPTOMS", "LIVER DYSFUNCTION", 
                             "NEUROLOGIC SYMPTOMS", "RENAL DYSFUNCTION", "RESPIRATORY SYMPTOMS", 
                             "GENERALIZED SYMPTOMS", "Kawasaki" )
  
  
  complete_table1 <- complete_table1 %>%
    left_join( pvalData1, by = c("categories", "site") ) %>%
    left_join( variantsData1, by = c("variant_misc", "site")) %>%
    filter( categories %in% categoriesToVisualize)
  
  allCombined_t1 <- complete_table1 %>% 
    group_by( categories, variant_misc ) %>%
    summarise( n = sum(n), 
               total = sum( total ) ) %>%
    mutate( site = "ALL Combined")
  
  complete_table1 <- rbind( complete_table1, allCombined_t1 )
  ###


    output$plot1 <- renderPlot({
        
        if( input$variant == "all"){
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
                filter( variant_misc == input$variant) 
            
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
    
    output$plot2 <- renderPlot({
      
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
    
    output$plot3 <- renderPlot({
      
      if( input$variant == "all"){
        toPlot <- complete_table1 %>%
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
        toPlot <- complete_table1 %>%
          mutate( perc = n/total*100) %>% 
          filter( variant_misc == input$variant) 
        
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
    
    output$plot4 <- renderPlot({
      
      allPvalue <- complete_table1 %>%
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
    
    
}

# Create Shiny app ----
shinyApp(ui, server)
