#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
setwd("~/ST_project_information/ST_project_information/")
source("load_libraries.R")

ST <- gs_title("ST Project Information")

give.n <- function(x){
  return(c(y = median(x)*0.0, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   

  st_info <- gs_read(ss=ST, ws="Sheet1", skip=12)
  st_data_sheet <- as.data.frame(st_info)
  st_data_sheet <- na.locf(st_data_sheet)
  st_data_sheet[,1] <- as.character(as.Date(st_data_sheet[,1], "%y/%m/%d"))
  
  observeEvent(input$refresh_data, {
    st_info <<- gs_read(ss=ST, ws="Sheet1", skip=12)
    st_data_sheet <<- as.data.frame(st_info)
    st_data_sheet <<- na.locf(st_data_sheet)
    st_data_sheet[,1] <<- as.character(as.Date(st_data_sheet[,1], "%y/%m/%d"))
    output$raw_data <<- renderTable({st_data_sheet})
  })
  
  #Write raw information out to table
  output$raw_data <- renderTable({st_data_sheet})
  
  #Dropdown menus for scatter plot
  output$splot_y <- renderUI({
    selectInput("sctplot_y", label = "Choose first value to plot:", names(st_data_sheet), selected = "Number of Unique Genes") 
  })
  output$splot_x <- renderUI({
    selectInput("sctplot_x", "Choose second value to plot:", names(st_data_sheet), selected = "Number of Unique Transcripts")
  })
  output$splot_col <- renderUI({
    selectInput("sctplot_col", "Choose how to colour the data:", names(st_data_sheet), selected = "Tissue Type")
  })
  output$splot_shape <- renderUI({
    selectInput("sctplot_shape", "Choose how to shape the points:", names(st_data_sheet), selected = "Organism")
  })
  output$splot_norm_check <- renderUI({
    radioButtons("splot_norm_check", "Do you want to normalise the data?", choices = c("No normalisation" = "no", "X Axis" = "x", "Y Axis" = "y", "Both Axes" = "x_y") , selected = "no")
  })
  output$splot_norm <- renderUI({
    selectInput("splot_norm", "Choose how to normalise the data:", names(st_data_sheet), selected = "Number of Raw Reads")
  })
  output$splot_norm_scale <- renderUI({
    textInput("splot_norm_scale", "Input a scaling factor:", value = 1)
  })
  
  #Scatterplot
  output$scatterplot <- renderPlotly({
    
    value1 <- as.numeric(unlist(subset(st_data_sheet, select = input$sctplot_y)))
    value2 <- as.numeric(unlist(subset(st_data_sheet, select = input$sctplot_x)))
    colour <- as.factor(unlist(subset(st_data_sheet, select = input$sctplot_col)))
    shape_ <- as.factor(unlist(subset(st_data_sheet, select = input$sctplot_shape)))
    norm_ <- as.numeric(unlist(subset(st_data_sheet, select = input$splot_norm)))
    norm_scale <- as.numeric(input$splot_norm_scale)
    splot_title <- paste("Scatterplot of ", input$sctplot_x, " againt ", input$sctplot_y, ".", sep="")
    
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
    
    splot_data <- aes(x=splotx_value,y=sploty_value, shape = shape_, color = colour)
    scatterplot <- ggplot(st_data_sheet, splot_data) + geom_point() + labs(colour = input$sctplot_col, shape = input$sctplot_shape) + 
      xlab(input$sctplot_x) + ylab(input$sctplot_y) + ggtitle(splot_title) + theme(plot.title = element_text(face = "bold")) + 
      scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) + theme(plot.margin = unit(c(1,4,1,1), "cm"))
    ggplotly(scatterplot) %>% config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "sendDataToCloud"), collaborate = FALSE, displaylogo = FALSE)
  })
  
  #Dropdown menues for Box and Whisker plot
  output$bandwplot_factor <- renderUI({
    selectInput("bandwplot_factor", "Choose the group factor:", names(st_data_sheet), selected = "Organism")
  })
  output$bandwplot_data <- renderUI({
    selectInput("bandwplot_data", "Select the data for the plot:", names(st_data_sheet), selected = "Number of Unique Transcripts")
  })
  output$bandwplot_col <- renderUI({
    selectInput("bandwplot_col", "Select the group to colour on:", names(st_data_sheet), selected = "Well Position")
  })
  
  #Box and Whisker plot
  output$bandwplot <- renderPlotly({
    
    bandwplot_factor <- as.factor(unlist(subset(st_data_sheet, select = input$bandwplot_factor)))
    bandwplot_data <- as.numeric(unlist(subset(st_data_sheet, select = input$bandwplot_data)))
    bandwplot_colour <- as.factor(unlist(subset(st_data_sheet, select = input$bandwplot_col)))
    bandwplot_title <- paste("Box and Whisker Plot Showing ", input$bandwplot_data, " for ", input$bandwplot_factor, ".", sep="")
    
    bandwplot <- aes(bandwplot_factor, bandwplot_data, color = bandwplot_colour)
    bandwplot <- ggplot(st_data_sheet, bandwplot) + geom_boxplot() + ggtitle(bandwplot_title) + xlab(input$bandwplot_factor) +
      ylab(input$bandwplot_data) + theme(plot.title = element_text(face = "bold"), text = element_text(margin = margin())) + scale_y_continuous(labels = comma) + 
      labs(colour = input$bandwplot_col) + theme(plot.margin = unit(c(1,4,1,1), "cm")) +
      stat_summary(fun.data = give.n, geom = "text", fun.y = median, position=position_dodge(width=0.75))
    ggplotly(bandwplot) %>% config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "sendDataToCloud"), collaborate = FALSE, displaylogo = FALSE) %>% layout(boxmode = "group")
  })  

  
})