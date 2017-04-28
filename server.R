#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#setwd("~/ST_project_information/")
#source("load_libraries.R")


require("googlesheets")
require("tidyr")
require("zoo")
require("shiny")
require("shinydashboard")
require("scales")
require("devtools")
require("Cairo")
require("plotly")
require("ggplot2")
require("DT")


ST <- gs_title("ST Project Information")

give.n <- function(x){
  return(c(y = 0, label = length(x))) 
}

shinyServer(function(input, output) {
   

  st_info <- gs_read(ss=ST, ws="Sheet1", skip=12)
  st_data_sheet <- as.data.frame(st_info)
  st_data_sheet <- na.locf(st_data_sheet)
  st_data_sheet[st_data_sheet == "-"] = NA
  st_data_sheet[,1] <- as.character(as.Date(st_data_sheet[,1], "%y/%m/%d"))
  
  #Don't require complete rows at the moment
  #st_data_sheet <- st_data_sheet[complete.cases(st_data_sheet),]
  
  observeEvent(input$refresh_data, {
    st_info <<- gs_read(ss=ST, ws="Sheet1", skip=12)
    st_data_sheet <<- as.data.frame(st_info)
    st_data_sheet <<- na.locf(st_data_sheet)
    st_data_sheet[,1] <<- as.character(as.Date(st_data_sheet[,1], "%y/%m/%d"))
    output$raw_data <<- DT::renderDataTable({st_data_sheet})
  })
  
  #Input menus for subsetting
  output$subset_ddmenu <- renderUI({
    selectInput("subset_ddmenu", label = "Choose column to subset on:", names(st_data_sheet), selected = "Person")
  })
  output$subset_checkboxes <- renderUI({
    checkboxGroupInput("subset_checkboxes", label = "Choose elements to subset on:", choices = sort(unique(as.character(unlist(subset(st_data_sheet, select = input$subset_ddmenu))))))
  })
  
  #Create reactive data frame based on subsetting (if any)
  st_data_subset <- reactive({
    if (length(input$subset_checkboxes) > 0 ){
      match_strings <- paste(input$subset_checkboxes, collapse = "|")
      st_data_sheet[grep(match_strings, as.character(unlist(subset(st_data_sheet, select = input$subset_ddmenu)))),]
    } else {
      st_data_sheet
    }
  })
  output$subset_st_data <- DT::renderDataTable({st_data_subset()})

  #Inputs to download the current subset as a csv file
  #output$dl_subset_csv <- renderUI({
  #  downloadButton("dl_subset_csv", label = "Download subset as .csv file")
  #})
  #output$dl_filename_csv <- renderUI({
  #  textInput("dl_filename", label = "Enter a filename", placeholder = "subset_data")
  #})
  output$dl_csv_data <- downloadHandler(
    filename = function() {
      paste(input$dl_filename, ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_data_subset(), file)
    }
  )
    
  #Write raw information out to table
  output$raw_data <- DT::renderDataTable({st_data_sheet})
  
  #Dropdown menus for scatter plot
  output$splot_y <- renderUI({
    selectInput("sctplot_y", label = "Choose first value to plot:", names(st_data_subset()), selected = "Number of Unique Genes") 
  })
  output$splot_x <- renderUI({
    selectInput("sctplot_x", "Choose second value to plot:", names(st_data_subset()), selected = "Number of Unique Transcripts")
  })
  output$splot_col <- renderUI({
    selectInput("sctplot_col", "Choose how to colour the data:", names(st_data_subset()), selected = "Tissue Type")
  })
  output$splot_shape <- renderUI({
    selectInput("sctplot_shape", "Choose how to shape the points:", names(st_data_subset()), selected = "Organism")
  })
  output$splot_norm_check <- renderUI({
    radioButtons("splot_norm_check", "Do you want to normalise the data?", choices = c("No normalisation" = "no", "X Axis" = "x", "Y Axis" = "y", "Both Axes" = "x_y") , selected = "no")
  })
  output$splot_norm <- renderUI({
    selectInput("splot_norm", "Choose how to normalise the data:", names(st_data_subset()), selected = "Number of Raw Reads")
  })
  output$splot_norm_scale <- renderUI({
    textInput("splot_norm_scale", "Input a scaling factor:", value = 1)
  })
  
  #Scatterplot
  output$scatterplot <- renderPlotly({
    
    value1 <- as.numeric(unlist(subset(st_data_subset(), select = input$sctplot_y)))
    value2 <- as.numeric(unlist(subset(st_data_subset(), select = input$sctplot_x)))
    colour <- as.factor(unlist(subset(st_data_subset(), select = input$sctplot_col)))
    shape_ <- as.factor(unlist(subset(st_data_subset(), select = input$sctplot_shape)))
    norm_ <- as.numeric(unlist(subset(st_data_subset(), select = input$splot_norm)))
    norm_scale <- as.numeric(input$splot_norm_scale)
    splot_title <- paste("Scatterplot of ", input$sctplot_x, " against ", input$sctplot_y, ".", sep="")
    
    sploty_value <- switch(input$splot_norm_check,
                           no = value1,
                           x = value1,
                           y = (value1/norm_)*norm_scale,
                           x_y = (value1/norm_)*norm_scale)
    splotx_value <- switch(input$splot_norm_check,
                           no = value2,
                           x = (value2/norm_)*norm_scale,
                           y = value2,
                           x_y = (value2/norm_)*norm_scale)
    
    splot_data <- aes(x=splotx_value,y=sploty_value, shape = shape_, color = colour, 
                      text = paste("Array Batch: ", `Array Batch`,  "\nArray: ", `Array Lot Number`, "\nWell: ", `Well Position`, 
                                   "\nTissue Type: ", `Tissue Type`, "\nPerson: ", `Person` , "\n", input$sctplot_y, ": ", sploty_value,
                                   "\n", input$sctplot_x, ": ", splotx_value, sep = ""))
    
    scatterplot <- ggplot(st_data_subset(), splot_data) + geom_point() + labs(colour = input$sctplot_col, shape = input$sctplot_shape) + 
      xlab(input$sctplot_x) + ylab(input$sctplot_y) + ggtitle(splot_title) + theme(plot.title = element_text(face = "bold")) + 
      scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) + theme(plot.margin = unit(c(1,4,1,1), "cm"))
    ggplotly(scatterplot, tooltip = "text") %>% config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "sendDataToCloud"), collaborate = FALSE, displaylogo = FALSE)
  })
  
  #Dropdown menues for Box and Whisker plot
  output$bandwplot_factor <- renderUI({
    selectInput("bandwplot_factor", "Choose the group factor:", names(st_data_subset()), selected = "Organism")
  })
  output$bandwplot_data <- renderUI({
    selectInput("bandwplot_data", "Select the data for the plot:", names(st_data_subset()), selected = "Number of Unique Transcripts")
  })
  output$bandwplot_col <- renderUI({
    selectInput("bandwplot_col", "Select the group to colour on:", names(st_data_subset()), selected = "Well Position")
  })
  output$bandwplot_usecol <- renderUI({
    checkboxInput("bandwplot_usecol", "Do you want to colour the data?", value = TRUE)
  })
  
  #Box and Whisker plot
  output$bandwplot <- renderPlotly({
    
    bandwplot_factor <- as.factor(unlist(subset(st_data_subset(), select = input$bandwplot_factor)))
    bandwplot_data <- switch(input$anova_norm_check,
                            "No" = as.numeric(unlist(subset(st_data_subset(), select = input$bandwplot_data))),
                            "Yes" = as.numeric(unlist(subset(st_data_subset(), select = input$bandwplot_data)))/as.numeric(unlist(subset(st_data_subset(), select = input$anova_norm)))
    )
    
    bandwplot_lab <- switch(as.character(input$bandwplot_usecol),
                            "FALSE" = input$bandwplot_factor,
                            "TRUE" = input$bandwplot_col
                            )
    bandwplot_title <- paste("Box and Whisker Plot Showing ", input$bandwplot_data, " for ", input$bandwplot_factor, ".", sep="")
    
    bandwplot_colour <- switch(as.character(input$bandwplot_usecol),
                               "FALSE" = as.factor(unlist(subset(st_data_subset(), select = input$bandwplot_factor))),
                               "TRUE" = as.factor(unlist(subset(st_data_subset(), select = input$bandwplot_col)))
    )
    
    bandwplot_xlab <- paste(levels(bandwplot_factor), "\nN=",table(bandwplot_factor, useNA = "no"), sep="")
    
    bandwplot <- aes(bandwplot_factor, bandwplot_data, color = bandwplot_colour)
    bandwplot <- ggplot(st_data_subset(), bandwplot) + geom_boxplot() + ggtitle(bandwplot_title) + xlab(input$bandwplot_factor) + scale_x_discrete(labels=bandwplot_xlab) +
      ylab(input$bandwplot_data) + theme(plot.title = element_text(face = "bold"), text = element_text(margin = margin())) + scale_y_continuous(labels = comma) + 
      labs(colour = bandwplot_lab) + theme(plot.margin = unit(c(1,4,1,1), "cm")) +
      stat_summary(fun.data = give.n, geom = "text", fun.y = median, position=position_dodge(width=0.70))
    ggplotly(bandwplot) %>% config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "sendDataToCloud"), collaborate = FALSE, displaylogo = FALSE) %>% layout(boxmode = "group")
  })  
  
  #Normalisation for ANOVA
  output$anova_norm_check <- renderUI({
    radioButtons("anova_norm_check", "Do you want to normalise the data for the ANOVA?", choices = c("Yes", "No"), selected = "No", inline = TRUE)
  })
  output$anova_norm <- renderUI({
    selectInput("anova_norm", "Choose how to normalise the data:", names(st_data_subset()), selected = "Number of Raw Reads")
  })
  
  #Table that shows number of observations in each factor
  output$factor_table <- renderTable({table(as.factor(unlist(subset(st_data_subset(), select = input$bandwplot_factor))))})
  
  output$factor_anova <- DT::renderDataTable({
    
    anova_data <- switch(input$anova_norm_check,
      "No" = as.numeric(unlist(subset(st_data_subset(), select = input$bandwplot_data))),
      "Yes" = as.numeric(unlist(subset(st_data_subset(), select = input$bandwplot_data)))/as.numeric(unlist(subset(st_data_subset(), select = input$anova_norm)))
    )
      
    anova_factor <- as.factor(unlist(subset(st_data_subset(), select = input$bandwplot_factor)))
    anova_aov <- aov(anova_data ~ anova_factor)
    anova_posthoc <- TukeyHSD(anova_aov)
    anova_posthoc_data <- as.data.frame(anova_posthoc$anova_factor)
    datatable(anova_posthoc_data) %>% formatStyle("p adj", backgroundColor = styleInterval(0.05, c("yellow", "white")))
  })
  
})
