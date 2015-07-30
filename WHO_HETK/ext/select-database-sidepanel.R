conditionalPanel(condition = "input.mainPanel == 'data_management'",
                 
                 selectInput("dataSource", label = h5("Select data sources"),
                             c("-" = "-",
                               "Upload your own data" = "OWN", 
                               "Health Equity Monitor Database" = "HETK"
                               #"Use GHO data" = "GHO"  REMOVE THIS OPTION UNTIL hetk IS SYNCHRONISED WITH goh
                             ),
                             selected = "HETK"),
                 
                 helpText(HTML("Once you have selected your data source, select the ‘Explore Inequality' tab to fetch your data"),
                          style="color:black; font-size:100%"),
                 hr()
                 
)