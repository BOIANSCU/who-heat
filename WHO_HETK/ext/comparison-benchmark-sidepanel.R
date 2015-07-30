conditionalPanel(condition = "input.mainPanel == 'compare_inequality' && input.comparison_panel == 'inequalbenchmark'",
                 
                 uiOutput("compplotBenchHealthIndicator"),
                 uiOutput("compplotBenchEquityDimension"),
                 uiOutput("compplotBenchYears"),
                 
                 h4("Benchmark countries"),
                 
                 selectInput("benchmarkWBgroup", label = h5("Filter by country income group"),
                             c("Low-income", 
                               "Lower middle-income",
                               "Upper middle-income",
                               "High-income"),
                             selected = NULL,
                             multiple=T),
                 
                 selectInput("benchmarkWHOregion", label = h5("Filter by WHO region"),
                             c("Eastern Mediterranean" = "EMR", 
                               "Europe" = "EUR",
                               "South East Asia" = "SEAR",
                               "Americas" = "AMR",
                               "Africa" = "AFR",
                               "Western Pacific" = "WPR"),
                             selected = NULL,
                             multiple=T),
                 
                 
                 
                 # On the fly drop down menu to select countries of interest
                 uiOutput("benchmarkCountries"),
                 
                 sliderInput('benchmarkYears', h5('Select years'), min=0, max=5, value=2, step = 1,
                             round = T, ticks = TRUE, animate = FALSE),
                 helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
                          style="color:black; font-size: 85%"),
                 
                 actionButton("getcomparisondata1", "Fetch data", class = "btn-success")
                 
)