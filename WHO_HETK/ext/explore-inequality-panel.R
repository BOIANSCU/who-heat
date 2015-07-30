tabPanel(tags$h5("Explore Inequality"), value='assess_inequality',
         tabsetPanel(id="assessment_panel",
                     tabPanel(h6("Disaggregated data - tables"), value='datatable', 
                              uiOutput('downloadDatatable'),
                              dataTableOutput(outputId="dataTable")
                     ), 
                     tabPanel(h6("Disaggregated data - graphs"), value='dataplot',
                              uiOutput('downloadDataplot'),
                              plotOutput('theDataPlot_web')), 
                     tabPanel(h6("Summary measures - tables"), value='sumtable', 
                              uiOutput('downloadSummtable'),
                              dataTableOutput(outputId="dataTableInequal")
                              ),              
                     tabPanel(h6("Summary measures - graphs"), value='sumplot',
                              uiOutput('downloadSummplot'),
                              plotOutput('theSumPlot_web')
                              )
                     )
)