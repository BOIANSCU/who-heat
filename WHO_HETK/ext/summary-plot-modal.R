bsModal(id = "summplotModal", title = "Download summary plot (PDF)", trigger = "downloadSummplot", 
        tags$p("Set the dimensions for the plot here and download it.  The fit of the plot is determined
                                 by the size of the paper you choose, and not by the display dimensions you select.
                                 For printing purposes, you may need to reduce the number of variables plotted"),
        br(),
        tags$p("Titles and axis labels are displayed according to your selections."),
        br(),
        tags$p("Close the window once the download has commenced."),
        br(),
        selectInput(inputId="papersize2", label='Plot size:',
                    choices=c("A4 portrait" = "a4",
                              "A4 landscape" = "a4r",
                              "US Letter portrait" = "us",
                              "US Letter landscape" = "USr",
                              "Custom" = "special")),
        conditionalPanel(condition = "input.papersize2 == 'special'",  # && assessment_panel = 'dataplot'
                         helpText('For custom plots the default size is 7" x 7" (i.e., 17.78cm x 17.78cm)'),
                         textInputRow(inputId="plot2_height", label="Height cm", value = '17.78'),
                         textInputRow(inputId="plot2_width", label='Width cm', value = '17.78'),
                         br(), br()
        ),
        downloadButton(outputId = 'downloadSummPlot', label = "Start", class = NULL),
        size = "medium")