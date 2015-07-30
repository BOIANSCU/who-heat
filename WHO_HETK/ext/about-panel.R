bsCollapse(id = "aboutPanel", open = "scatterPlotPanel", multiple = FALSE,
           bsCollapsePanel("Software", id = "software1", value = "standard1", type = "info", 
                           includeHTML("./www/software.html")),
           bsCollapsePanel("License", id = "license1", value = "standard2", type = "info",
                           includeHTML("./www/license.html")),
           bsCollapsePanel("Acknowledgements", id = "acknow1", value = "standard3", type = "info", 
                           includeHTML("./www/acknowledgement.html"))
)