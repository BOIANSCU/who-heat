conditionalPanel(condition = "input.mainPanel == 'compare_inequality' && input.comparison_panel == 'inequaldisag'",
                 uiOutput("compplotDisagHealthIndicator"),
                 uiOutput("compplotDisagEquityDimension"),
                 uiOutput("compplotDisagYears"),
                 hr(),
                 h3('Plot options'),
                 h4('Plot dimensions'),
                 
                 sliderInput('plot_height2', h5('Height'), min=200, max=1500, value=400, step = 50,
                             round = T, 
                             ticks = TRUE, animate = FALSE),
                 
                 sliderInput('plot_width2', h5('Width'), min=200, max=1500, value=600, step = 50,
                             round = T,
                             ticks = TRUE, animate = FALSE),                                        
                 
                 h4("Plot names"),
                 checkboxInput(inputId='long_names3', label=h5('Long health indicator names'), value = FALSE),                                         
                 textInputRow(inputId="axis_limitsmin3", label=h5("Axis-min"), value = NULL),
                 textInputRow(inputId="axis_limitsmax3", label=h5("Axis-max"), value = NULL),
                 
                 textInput(inputId = 'main_title3', label = h5('Main title'), value = ""),
                 textInput(inputId = 'xaxis_title3', label = h5('X-axis label'), value = ""),
                 textInput(inputId = 'yaxis_title3', label = h5('Y-axis label'), value = "")
)
