library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  navbarPage(
    title = "4CE MISC: labs QC", id="main_panel",
    theme = shinythemes::shinytheme("cerulean")),
  
  titlePanel("Labs QC across sites"),
  
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
      )
  )
)

server <- function(input, output) {
  
  at_admission_files <- list.files("./atAdmission/")
  during_admission_files <- list.files("./duringAdmission/")
  totalN <- read.delim("./sites_totalPatients.txt")
  
  for( i in 1:length( at_admission_files )){
    site_id <- sapply( strsplit(at_admission_files[i], "[_]"), '[', 1)
    
    load( paste0("./atAdmission/", at_admission_files[i]))
    n_total <- totalN %>% filter( site == site_id )
    
    if( i ==1 ){
      
      labs_adm <- table2_admission %>%
        mutate( perc = round(n_patients / n_total$total_n *100, 2),
                label = paste0( variableName, " (", units, ")"), 
                site = paste0( site_id, "\n (", n_total$total_n, ")"),
                site_label =  paste0( site_id, "\n (", n_patients, ")"),
                time = "admission")
      
      rm(table2_admission)
    }else{
      int_labs_adm <- table2_admission %>%
        mutate( perc = round(n_patients / n_total$total_n *100, 2),
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
        mutate( perc = round(n_patients / n_total$total_n *100, 2),
                label = paste0( variableName, " (", units, ")"), 
                site = paste0( site_id, "\n (", n_total$total_n, ")"), 
                site_label =  paste0( site_id, "\n (", n_patients, ")"),
                time = "during")
      
      rm(table2_during)
    }else{
      int_labs_during <- table2_during %>%
        mutate( perc = round(n_patients / n_total$total_n *100, 2),
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
    
    ggplot(labs_to_plot,  aes(x=site_label, y=perc, fill=site)) +
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
}

# Create Shiny app ----
shinyApp(ui, server)