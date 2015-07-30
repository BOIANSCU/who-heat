conditionalPanel(condition = "input.mainPanel == 'data_management' && input.dataSource != '-'",                 
                 # Manage the uploading of an individual data set                         
                 conditionalPanel( condition = "input.dataSource == 'OWN'",
                                   
                                   helpText(HTML("You cannot load a file larger than 100K.  The file must be in CSV format
                                                          and conform to the sample template found <a href=\"file:///uploadTemplate.csv\" target=\"_blank\">here</a>"),
                                            style="color:black; font-size: 85%"),
                                   
                                   hr(),
                                   
                                   radioButtons(inputId = "sep_type", 
                                                label = "Data separator",
                                                choices = c("Comma" = ",",
                                                            "Semi-colon" = ";",
                                                            "Tab" = "\t"),
                                                selected = '\t',
                                                inline=T),
                                   
                                   fileInput('ownData', h5('Select local data file:'),
                                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv') ),
                                   
                                   hr()
                 )
)
