library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
    
    navbarPage(
        title = "4CE MISC: compare table 1", id="main_panel",
        theme = shinythemes::shinytheme("cerulean")),
    
    titlePanel("Compare methods"),
    
    
    sidebarLayout(
        
        sidebarPanel(
            
            radioButtons("variant", label = h3("MISC Variant type:"),
                         choices = list("Total" = "total", "Alpha" = "Alpha", "Delta" = "Delta", "Omicron" ="Omicron", "All" = "all"), 
                         selected = "all"),
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Categorical Table1 percentages differentes", plotOutput("plot1"),  width = "100%"), 
                        tabPanel("Categorical Table1 comparison", plotOutput("plot2"),  width = "75%")
            )
        )
    )
)

server <- function(input, output) {
    
  
    load("./BCH Output/BCH_table1Categorical.RData")
    original <- table1_categorical
    original$type <- "original"
    original$total <- ifelse( original$variant_misc == "Alpha", 98, 
                              ifelse( original$variant_misc == "Delta", 18,
                                      ifelse( original$variant_misc == "Omicron", 21, 137)))
    
    rm( table1_categorical )
    
    load("./washoutTestBCH/outputMISC_washout14/BCH_table1Categorical.RData")
    removing <- table1_categorical
    removing$type <- "remove"
    removing$total <- ifelse( removing$variant_misc == "Alpha", 98, 
                              ifelse( removing$variant_misc == "Delta", 15,
                                      ifelse( removing$variant_misc == "Omicron", 13, 126)))
    
    rm(table1_categorical)
    
    load("./washoutTestBCH/outputMISC_washout_replaceEarlier14/BCH_table1Categorical.RData")
    replace_earlier <- table1_categorical
    replace_earlier$type <- "replace_earlier"
    replace_earlier$total <- ifelse( replace_earlier$variant_misc == "Alpha", 98, 
                                     ifelse( replace_earlier$variant_misc == "Delta", 16,
                                             ifelse( replace_earlier$variant_misc == "Omicron", 23, 137)))
    
    rm(table1_categorical)
    
    load("./washoutTestBCH/outputMISC_washout_replaceLater14/BCH_table1Categorical.RData")
    replace_later <- table1_categorical
    replace_later$type <- "replace_later"
    rm(table1_categorical)
    replace_later$total <- ifelse( replace_later$variant_misc == "Alpha", 99, 
                                   ifelse( replace_later$variant_misc == "Delta", 25,
                                           ifelse( replace_later$variant_misc == "Omicron", 13, 137)))
    
    categoriesToVisualize <- c("asthma", "CARDIOVASCULAR SYMPTOMS" , "COVID-19", "GI SYMPTOMS", "Kawasaki", "LIVER INVOLVEMENT", 
                               "NEUROLOGIC SYMPTOMS", "obesity_overweight", "RENAL INVOLVEMENT", "RESPIRATORY SYMPTOMS", 
                               "generalized symptoms" )
    
    all <- rbind( original, removing, replace_later, replace_earlier )
    allData <- all %>%
      filter( categories %in% categoriesToVisualize) %>%
      mutate( perc = round( n/ total * 100, 2)) %>%
      select( categories, variant_misc, perc, type )
    
    originalToCompare <- original %>%
      filter( categories %in% categoriesToVisualize) %>%
      mutate( perc = round( n/ total * 100, 2)) %>%
      select( categories, variant_misc, perc )
    
    allToCompare <-  rbind( removing, replace_later, replace_earlier ) %>%
      filter( categories %in% categoriesToVisualize) %>%
      mutate( perc = round( n/ total * 100, 2)) %>%
      select( categories, variant_misc, perc, type ) %>%
      left_join( originalToCompare,by = c( "categories", "variant_misc" ),suffix = c(".washout", ".original")) %>%
      # i swapped this so that a positive number means the new washout approach is adding patients.
      mutate( diff = perc.washout - perc.original) %>%
      select( categories, variant_misc, diff, type) %>%
      unique() %>% 
      filter( categories %in% categoriesToVisualize)
    
    
    
    output$plot2 <- renderPlot({
        
        if( input$variant == "all"){
            toPlot <- allData
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
            toPlot <- allData %>% 
                filter( variant_misc == input$variant) 
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
    output$plot1 <- renderPlot({
      
      if( input$variant == "all"){
        toPlot <- allToCompare
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
        toPlot <- allToCompare %>%
          filter( variant_misc == input$variant) %>%
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
}

# Create Shiny app ----
shinyApp(ui, server)
