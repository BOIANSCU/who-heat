conditionalPanel(condition = "input.mainPanel == 'assess_inequality' && input.assessment_panel == 'dataplot'",
                 tags$p(),
                 radioButtons("ai_plot_type", h3("Plot options"),
                              c("Bar Chart" = "data_bar",
                                "Line Chart" = "data_line"),
                              inline=T,
                              selected="data_line"), 
                 br(),
                 
                 h4('Plot dimensions'),
                 
                 sliderInput('plot_height1', h5('Height'), min=200, max=1500, value=400, step = 50,
                             round = T,
                             ticks = TRUE, animate = FALSE),
                 
                 sliderInput('plot_width1', h5('Width'), min=200, max=1500, value=400, step = 50,
                             round = T,
                             ticks = TRUE, animate = FALSE),
                 
                 br(),
                 h4('Plot names'),
                 
                 checkboxInput(inputId='long_names1', label='Long health indicator names', value = FALSE),
                 
                 textInputRow(inputId="axis_limitsmin1", label=h5("Axis-min"), value = NULL),
                 textInputRow(inputId="axis_limitsmax1", label=h5("Axis-max"), value = NULL),
                 
                 textInput(inputId = 'main_title1', label = h5('Main title'), value = ""),
                 textInput(inputId = 'xaxis_title1', label = h5('X-axis label'), value = ""),
                 textInput(inputId = 'yaxis_title1', label = h5('Y-axis label'), value = "")
)