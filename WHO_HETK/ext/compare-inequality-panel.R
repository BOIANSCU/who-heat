tabPanel(tags$h5("Compare Inequality"), value='compare_inequality',
         tabsetPanel(id = "comparison_panel", 
                     tabPanel(h6("Benchmark countries"), value='inequalbenchmark', dataTableOutput(outputId="dataTableBenchmark")),
                     tabPanel(h6("Disaggregated plot"), value='inequaldisag', 
                              uiOutput('downloadCompplot1'),
                             div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))),
                     tabPanel(h6("Summary plot"), value='inequalsum', 
                              # Plot points (default) or country codes on the Comparison Summary Plot
                              checkboxInput(inputId='points_ccode', 'Show country codes', value=FALSE),
                              uiOutput('downloadCompplot2'),
                              div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))))
)