conditionalPanel(condition = "input.mainPanel == 'assess_inequality' && input.assessment_panel == 'sumtable'",
                 
                 h4(textOutput('focusCountry2')),
                 hr(),

                 uiOutput("sumtableSumMeasure"),                 
                 uiOutput("sumtableHealthIndicator"),
                 uiOutput("sumtableEquityDimension"),
                 uiOutput("sumtableYears"),
                 h4('Summary measure options'),
                 checkboxInput('summultiplier1', 'MLD and TI x1000', TRUE),
                 checkboxInput('summultiplier2', 'RCI x100', TRUE),
                 sliderInput('sumsigfig', h5('Estimate precision'), min=0, max=5, value=2, round=T, width='50%'),
                 hr(),
                 radioButtons(inputId='se_type', 
                              label=h5('Standard error options'), 
                              choices = c('Bootstrap and Analytic' = 'both',
                                          'Analytic' = 'analytic',
                                          'Bootstrap' = 'bootstrap',
                                          'Aggregated' = 'balance'), 
                              selected = 'balance', 
                              inline = FALSE)
)