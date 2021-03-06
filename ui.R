#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


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

header <- dashboardHeader(title = "ST Project Information")

sidebar <- dashboardSidebar(
  tags$head(
    tags$script(
      HTML(
        "
        $(document).ready(function(){
          // Bind classes to menu items, easiet to fill in manually
          var ids = ['Subsets','Scatterplots','Box_and_Whisker_plots','raw_data'];
          for(i=0; i<ids.length; i++){
            $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
          }
      
          // Register click handeler
          $('.my_subitem_class').on('click',function(){
            // Unactive menuSubItems
            $('.my_subitem_class').parent().removeClass('active');
          })
        })
        "
      )
    )
  ),
  actionButton("refresh_data", "Fetch Updated Data"),
  menuItem("Subsets", tabName = "Subsets", icon = icon("th")),
  menuItem("Scatterplots", tabName = "Scatterplots", icon = icon("bar-chart")),
  menuItem("Box and Whisker plots", tabName = "Box_and_Whisker_plots", icon = icon("bar-chart")),
  menuItem("Raw Data", tabName = "raw_data", icon = icon("table"))
)

body <- dashboardBody(
  h2("Welcome to the Spatial Transcriptomics Project Information Site"),
  p("To view the data in graphical format, click on the Graphs tab on the sidebar."),
  p("To view the raw data, click on the Raw Data tab on the sidebar"),
  p("To make sure that the data you are viewing is up to date, click the \"Fetch Updated Data\" button on the sidebar."),
  p("If you have entered new data into the spreadsheet after opening this app, clicking the button will fetch the data."),
  tabItems(
    tabItem(tabName = "Subsets",
      fluidRow(
        box(title = "Subsets", width = "auto", column(width = 6, uiOutput("subset_ddmenu")), column(width=6, uiOutput("subset_checkboxes"))),
        box(title = "Download", width = "auto", 
            column(width = 6, downloadButton("dl_csv_data", label = "Download subset as .csv file")),
            column(width = 6, textInput("dl_filename", label = "Enter a filename", value = "subset_data"))),
        box(title = "Subsetted Data", width = "auto", DT::dataTableOutput("subset_st_data"))
      )
    ),
    tabItem(tabName = "Scatterplots",
      fluidRow(
        box(title = "Scatterplots", width = "auto",
          fluidRow(
            box(width = "auto", column(width = 6, uiOutput("splot_ft")),
                column(width = 6, downloadButton("splot_dl", label = "Save plot"))),
            box(width = 4, column(width = 12, uiOutput("splot_y")), column(width = 12, uiOutput("splot_x"))),
            box(width = 4, column(width = 12, uiOutput("splot_col")), column(width = 12, uiOutput("splot_shape"))),
            box(width = 4, column(width = 12, uiOutput("splot_norm_check")), column(width = 12, uiOutput("splot_norm")), column(width = 12, uiOutput("splot_norm_scale")))
          ),
          fluidRow(
            box(width = "auto", plotlyOutput("scatterplot"))
          )
        )
      )
    ),
    tabItem(tabName = "Box_and_Whisker_plots",
      fluidRow(
        box(title = "Box and Whisker Plots", width = "auto",
          fluidRow(
            column(width = 4, box(width = "auto", uiOutput("bandwplot_factor"))),
            column(width = 4, box(width = "auto", uiOutput("bandwplot_data"))),
            column(width = 4, box(width = "auto", uiOutput("bandwplot_col")), box(width = "auto", uiOutput("bandwplot_usecol")))
          ),
          fluidRow(
            box(width = "auto", plotlyOutput("bandwplot")),
            box(width = "auto", column(width = 6, uiOutput("anova_norm_check")), column(width = 6, uiOutput("anova_norm"))), 
                column(width = 12, box(width = 3, title = "Number of observations", tableOutput("factor_table")), 
                       box(width = 9, title = "TukeyHSD showing significant differences between comparisons", DT::dataTableOutput("factor_anova")))
          )
        )
      )
    ),
    tabItem(tabName = "raw_data",
            fluidRow(
              box(width = 12, div(style = 'overflow-x: scroll', DT::dataTableOutput("raw_data"))))
    )
  )
)

dashboardPage(header,sidebar,body)