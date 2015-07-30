conditionalPanel( condition = "input.mainPanel == 'assess_inequality' && (input.dataSource == 'GHO' || input.dataSource == 'HETK') &&
                                        (input.assessment_panel == 'datatable' || input.assessment_panel == 'dataplot')",   #### output.owndata gopt from server.R
                  
                  uiOutput("equityCountry"), #),
                  
                  radioButtons("data_source", h5("Select data sources"),
                               c("All", "DHS", "MICS"),
                               inline=T,
                               selected="All"),
                  
                  h5("Select years"),
                  checkboxInput('mostrecent', 'Most recent year', FALSE),
                  
                  conditionalPanel( condition = "!input.mostrecent",                                           
                                    uiOutput("years"),
                                    hr()), 
                  
                  
                  conditionalPanel(condition = "input.mainPanel == 'assess_inequality'",
                                   uiOutput("healthIndicator"),
                                   uiOutput("equityDimension"),
                                   hr(),
                                   actionButton("getdata", "Fetch Data",  class = "btn-success"),
                                   uiOutput('dataTableItems')
                  ) # End conditional panel
)