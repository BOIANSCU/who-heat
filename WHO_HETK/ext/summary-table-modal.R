bsModal(id = "summtableModal", title = "Download summary data", trigger = "downloadSummtable", 
        tags$p("The summary measures in the table will be downloaded as a text file with the values
                                  separated by a comma or a tab.  Select your preferred field separator and then download
                                  the data.  These can be opened in a text editor, or spreadsheet package."),
        br(),
        tags$p("Close the window once the download has commenced."),
        br(),
        radioButtons(inputId="filetype2", label='Field separator:',
                     choices=c("Comma separated valued" = "csv",
                               "Tab separated values" = "tsv")),
        downloadButton(outputId = 'downloadAnySumm', label = "Start", class = NULL),
        size = "medium")