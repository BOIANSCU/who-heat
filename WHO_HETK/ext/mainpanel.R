withoutAdmin <- tagList(
  
  tabPanel(tags$h5("Select Database"), value='data_management',
           conditionalPanel(condition = "input.dataSource != 'OWN'",
                            includeHTML("./www/selectData.html")),
           conditionalPanel(condition = "input.dataSource == 'OWN'",
                            dataTableOutput(outputId="uploadTable"))),   # Up loaded/Managed data
  
  tabPanel(tags$h5("Explore Inequality"), value='assess_inequality',
           tabsetPanel(id="assessment_panel",
                       tabPanel(h6("Disaggregated data - table"), value='datatable', 
                                dataTableOutput(outputId="dataTable"), 
                                uiOutput('downloadDatatable')
                       ), 
                       tabPanel(h6("Disaggregated data - graphs"), value='dataplot', plotOutput('theDataPlot_web')), 
                       tabPanel(h6("Summary measures - table"), value='sumtable', 
                                dataTableOutput(outputId="dataTableInequal"),
                                uiOutput('downloadSummtable')),              
                       tabPanel(h6("Summary measures - graphs"), value='sumplot', plotOutput('theSumPlot_web')))
  ),
  
  tabPanel(tags$h5("Compare Inequality"), value='compare_inequality',
           tabsetPanel(id = "comparison_panel", 
                       tabPanel(h6("Benchmark countries"), value='inequalbenchmark', dataTableOutput(outputId="dataTableBenchmark")),
                       tabPanel(h6("Disaggregated plot"), value='inequaldisag', div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))),
                       tabPanel(h6("Summary plot"), value='inequalsum', div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))))
  )                              
  #                     tabPanel(tags$h5("Notes"), value='notes_panel')   # Up loaded/Managed data
)  # End tabPanel in mainPanel


withAdmin <- tagList(
  
  tabPanel(tags$h5("Select Database"), value='data_management',
           conditionalPanel(condition = "input.dataSource != 'OWN'",
                            includeHTML("./www/selectData.html")),
           conditionalPanel(condition = "input.dataSource == 'OWN'",
                            dataTableOutput(outputId="uploadTable"))),   # Up loaded/Managed data
  
  tabPanel(tags$h5("Explore Inequality"), value='assess_inequality',
           tabsetPanel(id="assessment_panel",
                       tabPanel(h6("Disaggregated data - table"), value='datatable', 
                                dataTableOutput(outputId="dataTable"), 
                                uiOutput('downloadDatatable')
                       ), 
                       tabPanel(h6("Disaggregated data - graphs"), value='dataplot', plotOutput('theDataPlot_web')), 
                       tabPanel(h6("Summary measures - table"), value='sumtable', 
                                dataTableOutput(outputId="dataTableInequal"),
                                uiOutput('downloadSummtable')),              
                       tabPanel(h6("Summary measures - graphs"), value='sumplot', plotOutput('theSumPlot_web')))
  ),
  
  tabPanel(tags$h5("Compare Inequality"), value='compare_inequality',
           tabsetPanel(id = "comparison_panel", 
                       tabPanel(h6("Benchmark countries"), value='inequalbenchmark', dataTableOutput(outputId="dataTableBenchmark")),
                       tabPanel(h6("Disaggregated plot"), value='inequaldisag', div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))),
                       tabPanel(h6("Summary plot"), value='inequalsum', div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))))
  ),
  
  tabPanel(tags$h5("Administration"), value='admin_panel', 'Text for the admin panel')
)