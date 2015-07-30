tabPanel(tags$h5("Select Database"), value='data_management',
         conditionalPanel(condition = "input.dataSource != 'OWN'",
                          includeHTML("./www/landing_page.html")),
         conditionalPanel(condition = "input.dataSource == 'OWN'",
                          dataTableOutput(outputId="uploadTable")))