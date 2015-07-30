bsCollapse(id = "testCollapse", open = "scatterPlotPanel", multiple = FALSE,
           bsCollapsePanel("Information 1", id = "info1", value = "standard", type = "info", 
                           "this is some information about this page."),
           bsCollapsePanel("Information 2", id = "info2", value = "distPlot", type = "info",
                           "This is some information about that page"  
           )
)