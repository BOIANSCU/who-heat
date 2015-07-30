library(shiny)
library(shinythemes)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS")
library(ggplot2)
library(digest)
source('../resources/includes/useful.R')

shinyUI(
  fluidPage(  theme = shinytheme("readable"), 
              img(src='WHO-EN-B-H.jpg', align = "right", height = 72, width = 232),
              
              titlePanel(title=h2("Health Equity Assessment Toolkit"), windowTitle="Health Equity Assessment Toolkit"),
              sidebarLayout(      
                # Setting up the input/output controls for display in the mainPanel.
                # Conditional controls are used to manage the type of data displayed in each
                # output panel (tabPanel).
                
                #######################################
                ###### SIDEBAR PANEL BEGINS HERE  #####
                #######################################
                sidebarPanel(
                  source('./ext/tagshead-css.R')$value,  # provide additional CSS styling
                  
                  # This is a hidden variable to manage communication within the javascript interface
                  myHiddenText(inputId='countryVar', label='Cry the lost country', value='Afghanistan'),
                  
                  ##      Select Database      ##                        
                  ###############################
                  
                  #######  Select data Side Panel 1 #######
                  source('./ext/select-upload-sidepanel.R')$value,
                  
                  #######  Upload your own data ######
                  source('./ext/select-database-sidepanel.R')$value,                                    
                                    
                  ##    Assess Inequalities    ##                        
                  ###############################
                  
                  #######  Disaggregated Table Here #######
                  source('./ext/disaggregated-table-sidepanel.R')$value,
                  
                  #######  Disaggregated Plot Here #######                         
                  source('./ext/disaggregated-plot-sidepanel.R')$value,
                  
                  #######  Summary Table Here #######                         
                  source('./ext/summary-table-sidepanel.R')$value,
                  
                  #######  Summary Plot Here #######                         
                  conditionalPanel(condition = "input.mainPanel == 'assess_inequality' && input.assessment_panel == 'sumplot'",
                                   h4(textOutput('focusCountry3')),
                                   hr(),
                                   
                                   uiOutput("sumplotSumMeasures"),
                                   uiOutput("sumplotHealthIndicator"),
                                   uiOutput("sumplotEquityDimension"),
                                   uiOutput("sumplotYears"),
                                   br(),
                                   radioButtons("sumplot_type", h3("Plot options"),
                                                c("Bar Chart" = "data_bar",
                                                  "Line Chart" = "data_line"),
                                                inline=T,
                                                selected="data_line"),
                                   
                                   h4('Plot dimensions'),
                                   
                                   sliderInput('plot_height_sum', h5('Height'), min=200, max=1500, value=400, step = 50,
                                               round = T,
                                               ticks = TRUE, animate = FALSE),
                                   
                                   sliderInput('plot_width_sum', h5('Width'), min=200, max=1500, value=600, step = 50,
                                               round = T,
                                               ticks = TRUE, animate = FALSE),
                                   
                                   br(),   
                                   
                                   h4('Plot names'),
                                   
                                   checkboxInput(inputId='long_names2', label='Long health indicator names', value = FALSE),
                                   textInputRow(inputId="axis_limitsmin2", label=h5("Axis-min"), value = NULL),
                                   textInputRow(inputId="axis_limitsmax2", label=h5("Axis-max"), value = NULL),                      
                                   textInput(inputId = 'main_title2', label = h5('Main title'), value = ""),
                                   textInput(inputId = 'xaxis_title2', label = h5('X-axis label'), value = ""),
                                   textInput(inputId = 'yaxis_title2', label = h5('Y-axis label'), value = "")                                   
                  ),
                  
                  ## Compare Inequalities here ##                        
                  ###############################
                  conditionalPanel(condition = "input.mainPanel == 'compare_inequality'",
                                   h4(textOutput('focusCountry4')),
                                   hr(),
                                   source('./ext/comparison-benchmark-sidepanel.R')$value,  # Side panel for comparison benchmark                            
                                   source('./ext/comparison-plot1-sidepanel.R')$value  # Side panel for comparison of inequality                                                              
                  ),  # End conditionalPanel "Disaggregated Plot"
                  source('./ext/comparison-plot2-sidepanel.R')$value,  # Side panel for comparison of national average x inequality                            
                  source('./ext/administration-sidepanel.R')$value  # Side panel displayed in the administration subpanel 
                  
                ), # End ALL Sidepanels here
                                
                
                #######################################
                ######   MAIN PANEL BEGINS HERE   #####
                #######################################
                mainPanel(
                  source('./ext/disaggregated-table-modal.R')$value,  # Modal for dowloading the disaggregated indicators CSV file
                  source('./ext/disaggregated-plot-modal.R')$value,  # Modal for dowloading the disaggregated indicators plot
                  source('./ext/summary-table-modal.R')$value,  # Modal for downloading the summary inequalities CSV file,
                  source('./ext/summary-plot-modal.R')$value,  # Modal for dowloading the summary inequalities plot
                  source('./ext/comparison-plot1-modal.R')$value,  # Modal for downloading comparison plot 1
                  source('./ext/comparison-plot2-modal.R')$value,  # Modal for downloading comparison plot 2
                  
                  
                  tabsetPanel(id = "mainPanel",
                              source('./ext/select-database-panel.R')$value,  # Select the database
                              source('./ext/explore-inequality-panel.R')$value,  # Explore inequalities (and subtabs)
                              source('./ext/compare-inequality-panel.R')$value,  # Compare with benchmark countries (and subtabs)
                              source('./ext/information-panel.R')$value  # Information panel, with About, Glossary, Administration subtabs                
                  )  # End tabPanel in mainPanel
                , width=8)  # End mainPanel    
              )  # End sidebarLayout 
  )  # End fluidPage 
)  # End shinyUi

