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
                 
                 uiOutput('downloadSummplot'),
                 
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
                 
                 
                 
)