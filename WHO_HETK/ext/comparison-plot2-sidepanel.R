conditionalPanel(condition = "input.mainPanel == 'compare_inequality' && input.comparison_panel == 'inequalsum'",
                 
                 uiOutput("compplotSumMeasure"),
                 uiOutput("compplotSumHealthIndicator"),
                 uiOutput("compplotSumEquityDimension"),
                 uiOutput("compplotSumYears"),
                 p(),
                 hr(),
                 p(),
                 h3('Plot options'),
                 h4('Plot dimensions'),
                 
                 sliderInput('plot_height3', h5('Height'), min=200, max=800, value=400, step = 50,
                             round = T, 
                             ticks = TRUE, animate = FALSE),
                 
                 sliderInput('plot_width3', h5('Width'), min=200, max=800, value=600, step = 50,
                             round = T, 
                             ticks = TRUE, animate = FALSE),
                 
                 
                 h4("Plot names"),
                 ## INSERT  Long health indicator names
                 textInputRow(inputId="xaxis_limitsmin4", label = h5("X-axis min"), value = NULL),
                 textInputRow(inputId="xaxis_limitsmax4", label = h5("X-axis max"), value = NULL),
                 textInputRow(inputId="yaxis_limitsmin4", label = h5("Y-axis min"), value = NULL),
                 textInputRow(inputId="yaxis_limitsmax4", label = h5("Y-axis max"), value = NULL),
                 
                 
                 textInput(inputId = 'main_title4', label = h5('Main title'), value = ""),
                 textInput(inputId = 'xaxis_title4', label = h5('X-axis label'), value = ""),
                 textInput(inputId = 'yaxis_title4', label = h5('Y-axis label'), value = "")
)