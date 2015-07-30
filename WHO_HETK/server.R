library(shiny)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
library(ggplot2)
library(grid)
library(plyr)
library(countrycode)
library(RColorBrewer)
library(RSQLite)
library(digest)


source('helper.R')


shinyServer(function(input, output, session){  
  
  ### Creating reactive input for the Data Table and Data Plot tabs
  ###
  # Countries can be filtered by input$WHOregion and input$wbGroup, but currently 'No Filter' is the default
  output$equityCountry <- renderUI({
    #    countries <- getFilteredCountries(input$WBgroup, input$WHOregion, input$dataSource)    
    countries <- getFilteredCountries(NULL, NULL, input$dataSource)
    if(is.null(countries)){ countries <- c()}
    selectInput("equityCountry", 
                h5("Country:"), 
                countries, 
                multiple=F, 
                selected=NULL)
  })
  
  
  # Year is filtered by the survey availability by country and the source (MICS/DHS of the survey)
  output$years <- renderUI({
    #    countries <- getFilteredCountries(input$WBgroup, input$WHOregion, input$dataSource)    
    selectYears <- getFilteredYear(country=input$equityCountry, datasource=input$data_source, database='HETK')
    if(is.null(selectYears)){ selectYears <- c()}
    selectInput(inputId="years", 
                label='', 
                choices=selectYears, 
                multiple=T, 
                selected=NULL)
  })
  
  
  # Focus country
  output$focusCountry1 <- renderText({ 
    if(length(input$equityCountry) > 0){
      return(input$equityCountry)
    }
  })
  
  
  output$focusCountry2 <- renderText({ 
    if(length(input$equityCountry) > 0){
      return(input$equityCountry)
    }
  })
  
  
  output$focusCountry3 <- renderText({ 
    if(length(input$equityCountry) > 0){
      return(input$equityCountry)
    }
  })
  
  
  output$focusCountry4 <- renderText({ 
    if(length(input$equityCountry) > 0){
      return(input$equityCountry)
    }
  })
  
  
  output$focusCountry5 <- renderText({ 
    if(length(input$equityCountry) > 0){
      return(input$equityCountry)
    }
  })
  
  
  
  
  
  #  Set up the selectInput for the selection of health indicator
  output$healthIndicator <- renderUI({
    selectionOptions <- healthIndicatorList(option = 'full')      
    if(is.null(selectionOptions)){ selectionOptions <- c()}
    selectInput("healthIndicator", 
                h5("Select health indicators"), 
                selectionOptions, 
                multiple=T)
  })
  
  #  Set up the selectInput for the selection of the equity dimensions
  output$equityDimension <- renderUI({
    selectInput(inputId = "equityDimension",
                h5("Select inequality dimensions"),
                choices = c("Economic status", "Geographic region", "Mother's education",
                            "Place of residence","Sex"),
                multiple=T)
  })
  
  # Set up the selectInput for the display of data in the Disaggregated Data Table view
  output$dataTableItems <- renderUI({
    dataTmp <- datasetInput()
    if(nrow(dataTmp)>0  & input$mainPanel == 'assess_inequality' & input$assessment_panel == "datatable"){ 
      selectOptions <- c("Country" = 'country', 
                         "Year" = "year", 
                         "Data source" = "source", 
                         "Health indicator" = "indic", 
                         "Inequality dimension" = "dimension",
                         "Subgroup" = "subgroup",
                         "Estimate" = "estimate",
                         "Lower 95%CI"  = "lower_95ci",
                         "Upper 95%CI"  = "upper_95ci",
                         "Population share %"   = "popshare",
                         "Flag" = "flag")
      selectedOptions <- c("country", "year", "source", "indic", "dimension", "subgroup", "estimate")
      selectInput(inputId = "dataTableItems",
                  h4("Select table content"),
                  choices = selectOptions,
                  selected = selectedOptions,
                  multiple=T)
    } else {
      return(NULL)
    }
  })
  
  
  # Create a download button contingent on data in the table
  output$downloadDatatable <- renderUI({
    theData <- datasetInput()
    if(nrow(theData)==0){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadDatatable", "Download", class = "btn-primary"))
    }  
  })
  
  
  # Handler for downloading the data selected in the modal download table
  output$downloadAnyData <- downloadHandler(
    filename = function() {
      paste(input$equityCountry, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      sep <- switch(input$filetype1, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
  # Create a download button contingent on the existence of a plot of the disaggregated data
  output$downloadDataplot <- renderUI({
    thePlot <- theDataPlot()
    if(is.null(thePlot)){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadDataplot", "Download Plot", class = "btn-primary"))
    }  
  })
  
  
  
  # Handler for downloading the data selected in the modal download table
  output$downloadAnyPlot <- downloadHandler(
    filename = function() { 
      paste(input$equityCountry, '_disag_', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
      print(theDataPlot()) 
      dev.off()
    }
  )   
  
  
  #  Select the summary measures from the legitimate choices available, given the 
  output$sumMeasures <- renderUI({
    if(input$mostrecent){
      mostrecent_years <- sort(unique(datasetInput()$year))
      return(selectInput("uddYears", 
                         h5("Select years"), 
                         mostrecent_years, 
                         multiple=T))
    }
    if(!input$mostrecent){
      selected_years <- input$years
      return(selectInput("uddYears", 
                         h5("Select years"), 
                         choices=input$years,
                         selected=selected_years,
                         multiple=T))
    }
  })
  
  
  ### Creating reactive input for the Summary Tables
  ###
  
  output$sumtableSumMeasure <- renderUI({
    if(length(input$sumtableEquityDimension)>0){
      if(input$sumtableEquityDimension %in% rankable){
        selectionOptions <- allSummaryMeasures
      }
      if(!input$sumtableEquityDimension %in% rankable){
        selectionOptions <- unrankSummaryMeasures
      }
    }
    else{
      selectionOptions <- NULL
    }
    print(selectionOptions)
    selectInput("sumtableSumMeasure", 
                h5("Select summary measure"), 
                choices=selectionOptions, 
                selected=c("Range difference" = "rd"), 
                multiple=T)
  })
  
  output$sumtableHealthIndicator <- renderUI({    
    # Multiple select for the health indicator 
    if(is.null(input$healthIndicator)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- sort(input$healthIndicator)
      print(input$healthIndicator)
      selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
    }
    selectInput("sumtableHealthIndicator", 
                h5("Select health indicators"), 
                choices=selectionOptions, 
                selected=selectionOptions, 
                multiple=T)
  })
  
  
  output$sumtableEquityDimension <- renderUI({    
    # Multiple select for the equity indicator 
    if(is.null(input$equityDimension)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- sort(input$equityDimension)
    }
    selectInput("sumtableEquityDimension", 
                h5("Select inequality dimensions"), 
                choices=selectionOptions, 
                selected=selectionOptions, 
                multiple=T)
  })
  
  
  output$sumtableYears <- renderUI({    
    # Multiple select for the years of interest
    yearsOfInterest <- sort(unique(datasetInput()$year))
    if(is.null(yearsOfInterest)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- yearsOfInterest
    }
    selectInput("sumtableYears", 
                h5("Select years"), 
                choices=selectionOptions, 
                selected=selectionOptions, 
                multiple=T)
  })
  
  # Create a download button contingent on data in the table
  output$downloadSummtable <- renderUI({ 
    theData <- datasetInequal()
    if(is.null(theData)){
      return(NULL)
    }
    if(nrow(theData)==0){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadSummtable", "Download", class = "btn-primary"))
    }  
  })
  
  # Handler for downloading the data selected in the modal download table
  output$downloadAnySumm <- downloadHandler(
    filename = function() {
      paste(input$equityCountry, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInequal(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
  
  
  
  ### Creating reactive input for the Summary Plots
  ###
  
  output$sumplotSumMeasures <- renderUI({
    if(length(input$sumplotEquityDimension)>0){
      if(input$sumplotEquityDimension %in% rankable){
        selectionOptions <- allSummaryMeasures
      }
      if(!input$sumplotEquityDimension %in% rankable){
        selectionOptions <- unrankSummaryMeasures
      }
    }
    else{
      selectionOptions <- NULL
    }
    selectInput("sumplotSumMeasures", 
                h5("Select summary measure"), 
                choices=selectionOptions, 
                selected=c("Range difference" = "rd"), 
                multiple=F)
  })
  
  
  output$sumplotHealthIndicator <- renderUI({    
    # Multiple select for the health indicator 
    if(length(input$sumtableHealthIndicator)<1){ 
      selectionOptions <- c()
    }
    else{    
      selectionOptions <- sort(input$sumtableHealthIndicator)
      selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
    }
    selectInput("sumplotHealthIndicator", 
                h5("Select health indicators"), 
                choices = selectionOptions, 
                selected = selectionOptions,
                multiple = T)
  })
  
  
  output$sumplotEquityDimension <- renderUI({    
    # Multiple select for the equity indicator 
    if(is.null(input$equityDimension)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- input$equityDimension
    }
    selectInput("sumplotEquityDimension", 
                h5("Select inequality dimensions"), 
                choices=selectionOptions, 
                multiple=F)
  })
  
  
  output$sumplotYears <- renderUI({    
    # Multiple select for the years of interest
    if(length(input$sumtableYears)<1){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- input$sumtableYears
    }
    selectInput("sumplotYears", 
                h5("Select years"), 
                choices=selectionOptions, 
                selected=selectionOptions, 
                multiple=T)
  })
  
  
  output$downloadSummplot <- renderUI({
    thePlot <- theSummaryPlot()
    if(is.null(thePlot)){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadSummplot", "Download Plot", class = "btn-primary"))
    }  
  })
  
  
  # Handler for downloading the data selected in the modal download table
  output$downloadSummPlot <- downloadHandler(
    filename = function() { 
      paste(input$equityCountry, '_summ_', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width=(as.numeric(input$plot2_width)/2.54), height=(as.numeric(input$plot2_height)/2.45), paper=input$papersize2)
      print(theSummaryPlot()) 
      dev.off()
    }
  )   
  
  
  
  
  
  ### Creating reactive input for the Comparison Plots
  
  
  ###  Comparison Benchmark Countries
  
  output$compplotBenchHealthIndicator <- renderUI({    
    # Multiple select for the health indicator 
    if(is.null(input$healthIndicator)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- sort(input$healthIndicator)
      selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
    }
    selectInput("compplotBenchHealthIndicator", 
                h5("Select health indicators"), 
                choice=selectionOptions, 
                selected=selectionOptions,
                multiple=T)
  })
  
  
  output$compplotBenchEquityDimension <- renderUI({    
    # Multiple select for the equity indicator 
    if(is.null(input$equityDimension)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- input$equityDimension
    }
    selectInput("compplotBenchEquityDimension", 
                h5("Select inequality dimensions"), 
                choices=selectionOptions, 
                multiple=F)
  })
  
  
  output$compplotBenchYears <- renderUI({    
    # Multiple select for the years of interest
    yearsOfInterest <- sort(unique(datasetInput()$year))
    if(is.null(yearsOfInterest)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- yearsOfInterest
    }
    selectInput("compplotBenchYears", 
                h5("Select years"), 
                choices=selectionOptions, 
                selected=max(selectionOptions), 
                multiple=F)
  })
  
  output$benchmarkCountries <- renderUI({
    countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion, input$dataSource)  
    selectInput("benchmarkCountries", 
                h5("Select countries"), 
                choices=countries, 
                selected=countries,
                multiple=T)
  })
  
  
  
  ###  Comparison Disaggregated Plots
  
  output$compplotDisagHealthIndicator <- renderUI({    
    # Multiple select for the health indicator 
    if(is.null(input$healthIndicator)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- sort(input$healthIndicator)
      selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
    }
    selectInput("compplotDisagHealthIndicator", 
                h5("Select health indicators"), 
                choice=selectionOptions, 
                selected=selectionOptions,
                multiple=T)
  })
  
  
  output$compplotDisagEquityDimension <- renderUI({    
    # Multiple select for the equity indicator 
    if(is.null(input$equityDimension)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- input$equityDimension
    }
    selectInput("compplotDisagEquityDimension", 
                h5("Select inequality dimensions"), 
                choices=selectionOptions, 
                multiple=F)
  })
  
  
  output$compplotDisagYears <- renderUI({    
    # Multiple select for the years of interest
    yearsOfInterest <- sort(unique(datasetInput()$year))
    if(is.null(yearsOfInterest)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- yearsOfInterest
    }
    selectInput("compplotDisagYears", 
                h5("Select years"), 
                choices=selectionOptions, 
                selected=max(selectionOptions), 
                multiple=F)
  })
  
  # Create a download button contingent on the existence of a comparison plot of the disaggregated data
  output$downloadCompplot1 <- renderUI({
    thePlot <- theDataPlot()
    if(is.null(thePlot)){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadCompplot1", "Download Plot", class = "btn-primary"))
    }  
  })
  
  
  ### Comparison Summary Plots
  
  #  Select the summary measures from the legitimate choices available, given the 
  
  
  output$compplotSumMeasure <- renderUI({
    if(length(input$compplotSumEquityDimension)>0){
      if(input$compplotSumEquityDimension %in% rankable){
        selectionOptions <- allSummaryMeasures
      }
      if(!input$compplotSumEquityDimension %in% rankable){
        selectionOptions <- unrankSummaryMeasures
      }
    }
    else{
      selectionOptions <- NULL
    }
    
    selectInput("compplotSumMeasure", 
                h5("Select summary measure"), 
                choices=selectionOptions,
                selected=c("Range difference" = "rd"), 
                multiple=F)
  })
  
  
  
  output$compplotSumHealthIndicator <- renderUI({    
    # Multiple select for the health indicator 
    if(is.null(input$healthIndicator)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- sort(input$healthIndicator)
      selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
    }
    selectInput("compplotSumHealthIndicator", 
                h5("Select health indicators"), 
                choices=selectionOptions, 
                multiple=F)
  })
  
  
  output$compplotSumEquityDimension <- renderUI({    
    # Multiple select for the equity indicator 
    if(is.null(input$equityDimension)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- input$equityDimension
    }
    selectInput("compplotSumEquityDimension", 
                h5("Select inequality dimensions"), 
                choices=selectionOptions, 
                multiple=F)
  })
  
  
  output$compplotSumYears <- renderUI({    
    # Multiple select for the years of interest
    yearsOfInterest <- sort(unique(datasetInput()$year))
    if(is.null(yearsOfInterest)){ 
      selectionOptions <- c()
    }
    else{
      selectionOptions <- yearsOfInterest
    }
    selectInput("compplotSumYears", 
                h5("Select years"), 
                choices=selectionOptions, 
                selected=max(selectionOptions), 
                multiple=F)
  })
  
  output$benchmarkCountriesSum <- renderUI({
    countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion, input$dataSource)   
    if(is.null(countries)){ countries <- c()}
    print(countries)
    selectInput("benchmarkCountriesSum", 
                h5("Countries"), 
                choices=countries, 
                selected=countries,
                multiple=T)
  })
  
  observe({
    code <- digest(input$admin_code, serialize=F)
    if(code == "6885b5b29162de9e6f0bae3347828acb"){  # 'WH02015'
      updateCheckboxInput(session, inputId='admin_show', value = TRUE)
    } else {
      updateCheckboxInput(session, inputId='admin_show', value = FALSE)
    }
  })
  
  
  
  ###########################
  # Part 1 of  *Reactive* to set up the download of data from the Global Health Observatory (GHO)
  getData1 <- reactive({
    # This *reactive* puts an approptiate URL together for according to the GHO API
    input$getdata
    isolate({
      if(input$dataSource == 'GHO'){
        gho.url <- setupGHOdata(indicator=input$gho_equityIndic, stratifier=input$equityStrata, 
                                countries=input$equityCountry, years=input$years)
        print(gho.url)
        return(gho.url)
      }
    })
  })
  
  
  # Part 2 of  *Reactive* to set up thje download of data from the Global Health Observatory (GHO) 
  getData1a <- reactive({
    # This *reactive* gets the data from GHO or the HETKdb
    input$getdata
    isolate({  # Isolate holds of the getting of the data until the Process button in the ui.R is pressed
      if(input$dataSource == 'GHO'){  # Get the data from GHO
        gho.data <- getGHOdata(getData1(), indicator=input$equityIndic)
        return (gho.data)
      }
    })
  })
  
  
  # A *Reactive* to read data in from the Health Equity Toolkit DB
  getData2 <- reactive({
    # This *reactive* gets the data from HETKdb
    input$getdata
    isolate({  # Isolate holds of the getting of the data until the Process button in the ui.R is pressed
      if(input$dataSource == 'HETK'){  # Get the data from the HETKdb
        hetk.data <- getHETKdata(indicator=input$healthIndicator, stratifier=input$equityDimension, 
                                 countries=input$equityCountry, years=input$years, mostrecent=input$mostrecent,
                                 datasource=input$data_source)
        updateTextInput(session, inputId='countryVar', label='Cry the lost country', value=unique(hetk.data$country))
        return (hetk.data)
      }
    })
  })
  
  
  # A *Reactive* to upload your own data
  getData3 <- reactive({
    # This *reactive* uploads a users own equity data
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$ownData
    
    if (is.null(inFile))
      return(NULL)
    
    minimum_headers <- c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'se', 'pop', 'flag', 'rankable', 'order', 'maxoptimum')
    tmpDF <- read.csv(inFile$datapath, sep=input$sep_type, header=T, stringsAsFactors = F)
    if(setdiff(names(tmpDF), minimum_headers) %in%  minimum_headers){
      # If the header does not contains the minimum required variables, it returns NULL
      toggleModal(session, "upload_error1")
      return(NULL)
    }
    
    # Remove rows with missing estimates or stratifiers; all else is forgiven 
    #    tmpDF <- tmpDF[!is.na(tmpDF$estimate) & (tmpDF$dimension !=''), ]  
    return(tmpDF)    
  })
  
  #  Pass a NULL own data upload back to ui.R
  output$fileUploaded <- reactive({
    theData <- getData3()
    print(theData)
    if(!is.null(theData)){
      return()
    } else {
      return(NULL)
    }
    
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  
  # A *Reactive* to fetch benchmark country disaggregated and merge it with fetched data
  getData4 <- reactive({
    # This *reactive* fetches benchmark country data for the Disaggregated TABLE
    input$getcomparisondata1
    #    input$getcomparisondata2
    print(input$getcomparisondata1)
    isolate({
      
      anchordata <- datasetInput()
      
      relevant.rows <- which(anchordata$year %in% input$compplotBenchYears &   # Select only the right years ...
                               anchordata$indic == input$compplotBenchHealthIndicator &  # health indicator, and ... 
                               anchordata$dimension == input$compplotBenchEquityDimension)  # equity dimension 
      
      anchordata <- anchordata[ relevant.rows , ]
      
      print('Pre Benchmark')    
      benchmarkdata <- getComparisonCountries(indicator = input$compplotBenchHealthIndicator, 
                                              stratifier = input$compplotBenchEquityDimension, 
                                              countries = input$benchmarkCountries, 
                                              years =  unique(anchordata$year), 
                                              elasticity = input$benchmarkYears, matchyears=F)
      print('Post Benchmark')    
      
      thedata <- rbind(anchordata, benchmarkdata)  # Merge the relevant initial data with benchmarkdata
      print(thedata)
      if(is.null(thedata)){
        return(NULL)
      }
      if(nrow(thedata)==0){
        return(NULL)
      }
      else{
        return(thedata)
      }
    })
  })
  
  
  
  # A *Reactive* to fetch benchmark country FOR the Disaggregated Comparison Plot
  getData4a <- reactive({
    # This *reactive* fetches benchmark country data for the PLOT
    print("Reactive: getData4a")
    if(input$mainPanel != "compare_inequality" & input$comparison_panel != 'inequaldisag'){
      return(NULL)
    }
    if(length(input$compplotDisagYears)==0 | length(input$compplotDisagHealthIndicator)==0 | length(input$compplotDisagEquityDimension)==0){
      return(NULL)
    }
    print(paste(input$compplotBenchYears, input$compplotDisagYears, input$compplotBenchHealthIndicator, input$compplotDisagHealthIndicator, input$compplotBenchEquityDimension, input$compplotDisagEquityDimension, sep=' >> '))
    if(input$compplotBenchYears == input$compplotDisagYears & input$compplotBenchHealthIndicator == input$compplotDisagHealthIndicator & input$compplotBenchEquityDimension == input$compplotDisagEquityDimension){
      return( getData4() )
    } else {
      
      anchordata <- datasetInput()
      
      relevant.rows <- which(anchordata$year %in% input$compplotDisagYears &   # Select only the right years ...
                               anchordata$indic == input$compplotDisagHealthIndicator &  # health indicator, and ... 
                               anchordata$dimension == input$compplotDisagEquityDimension)  # equity dimension 
      
      anchordata <- anchordata[ relevant.rows , ]
      
      benchmarkdata <- getComparisonCountries(indicator = input$compplotDisagHealthIndicator, 
                                              stratifier = input$compplotDisagEquityDimension, 
                                              countries = input$benchmarkCountries, 
                                              years =  unique(anchordata$year), 
                                              elasticity = input$benchmarkYears, matchyears=F)
      
      thedata <- rbind(anchordata, benchmarkdata)  # Merge the relevant initial data with benchmarkdata
      if(is.null(thedata) | nrow(thedata)==0){
        return(NULL)
      }
      else{
        return(thedata)
      }
    }
  })
  
  
  
  
  # A *Reactive* to fetch benchmark country summary data
  getData5 <- reactive({
    # This *reactive* fetches benchmark country summary data
    if(length(input$equityCountry) > 0){
      thecountries <- unique(c(input$equityCountry, input$benchmarkCountries))
      print(thecountries)
      print(input$compplotSumMeasure) 
      print(input$compplotSumHealthIndicator)
      print(input$compplotSumEquityDimension) 
      print(thecountries) 
      print(input$compplotSumYears)
      print(input$benchmarkYearsSum)
      thedata <- getComparisonSummaries(summeasure=input$compplotSumMeasure, 
                                        indicator=input$compplotSumHealthIndicator, 
                                        stratifier=input$compplotSumEquityDimension, 
                                        countries=thecountries, 
                                        years=input$compplotSumYears, 
                                        elasticity=input$benchmarkYears, matchyears=T)
      thedata$anchor <- 0
      thedata$anchor[thedata$country==input$equityCountry] <- 1
      
    } else {
      thedata <- NULL
    }
    return(thedata)
  })
  
  
  
  
  
  
  # Return the requested dataset based on the UI selection (dataSource)
  datasetInput <- reactive({
    switch(input$dataSource,
           "GHO" = getData1a(),
           "HETK" = getData2(),
           "OWN" = getData3()
    )
  })
  
  
  
  
  ## Download the datset
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  
  
  # Generate a view of the Managed Data
  output$uploadTable <- renderDataTable({
    theData <- datasetInput()
    if(is.null(theData)){
      return(NULL)
    }
    if(nrow(theData)==0){
      return(NULL)
    } 
    
    theData <- theData[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'lower_95ci', 'upper_95ci', 'popshare', 'flag')] 
    theData[, c('estimate', 'lower_95ci', 'upper_95ci', 'popshare')]  <-
      round(theData[, c('estimate', 'lower_95ci', 'upper_95ci', 'popshare')] , 2)
    names(theData)[names(theData)=="country" ] <- "Country" 
    names(theData)[names(theData)=="year" ] <- "Year"
    names(theData)[names(theData)=="source" ] <- "Data source"    
    names(theData)[names(theData)=="indic" ] <- "Health indicator" 
    names(theData)[names(theData)=="dimension" ] <- "Inequality dimension" 
    names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
    names(theData)[names(theData)=="estimate" ] <- "Estimate"
    names(theData)[names(theData)=="lower_95ci" ] <- "Lower 95%CI"
    names(theData)[names(theData)=="upper_95ci" ] <- "Upper 95%CI"
    names(theData)[names(theData)=="popshare" ] <- "Population share %"
    names(theData)[names(theData)=="flag" ] <- "Flag"
    #
    theData
  }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
  )
  
  
  
  
  # Generate a view of the Managed Data
  output$dataTable <- renderDataTable({
    print('Enter the disaggregated data table')
    theData <- datasetInput()
    if(is.null(theData)){
      return(NULL)
    }
    if(nrow(theData)==0){
      return(NULL)
    }
    if(theData$source[1] %in% c('DHS', 'MICS')){
      theData <- theData[, input$dataTableItems]
      #      theData <- theData[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'lower_95ci', 'upper_95ci', 'popshare', 'flag')]
    }
    else{
      theData <- theData[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'se')]
    }
    names(theData)[names(theData)=="country" ] <- "Country" 
    names(theData)[names(theData)=="year" ] <- "Year"
    names(theData)[names(theData)=="source" ] <- "Data source"    
    names(theData)[names(theData)=="indic" ] <- "Health indicator" 
    names(theData)[names(theData)=="dimension" ] <- "Inequality dimension" 
    names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
    names(theData)[names(theData)=="estimate" ] <- "Estimate"
    names(theData)[names(theData)=="lower_95ci" ] <- "Lower 95%CI"
    names(theData)[names(theData)=="upper_95ci" ] <- "Upper 95%CI"
    names(theData)[names(theData)=="popshare" ] <- "Population share %"
    names(theData)[names(theData)=="flag" ] <- "Flag"
    # Reduce the significant figures to 2
    names_df <- names(theData)
    print(names_df)
    the_numeric_names <- which(names_df %in% c("Estimate", "Lower 95%CI", "Upper 95%CI", "Population share %"))
    print(the_numeric_names)
    for(i in names_df[the_numeric_names]){
      print(i)
      theData[, i] <- 
        round(theData[,i], 2)
    }
    theData
  }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
  )
  
  
  
  datasetInequal <- reactive({
    
    if(input$dataSource!='HETK'){
      tmpDF <- datasetInput()      
      relevant.rows <- which(tmpDF$year %in% input$sumtableYears & tmpDF$indic %in% input$sumtableHealthIndicator & tmpDF$dimension %in% input$sumtableEquityDimension)
      
      tmpDF <- tmpDF[ relevant.rows, ]
      
      print(tmpDF)
      if(is.null(relevant.rows)){
        return(NULL)
      }
      if(!is.null(relevant.rows)){
        ineqDF <- calcInequal(tmpDF, inequal.types='all')
        print(ineqDF)
        return(ineqDF)
      }
    }
    if(input$dataSource=='HETK' & input$assessment_panel=='sumtable'){
      print('Getting equity data table')
      ineqDF <- getInequal(indicator=input$sumtableHealthIndicator, 
                           stratifier=input$sumtableEquityDimension, 
                           countries=input$equityCountry, 
                           years=input$sumtableYears,  
                           inequal_types=input$sumtableSumMeasure)
      return(ineqDF)
    }    
    if(input$dataSource=='HETK' & input$assessment_panel=='sumplot'){
      print('Getting equity data plot')
      ineqDF <- getInequal(indicator=input$sumtableHealthIndicator, 
                           stratifier=input$sumtableEquityDimension, 
                           countries=input$equityCountry, 
                           years=input$sumtableYears,  
                           inequal_types=input$sumplotSumMeasures)
      ineqDF$boot.se[ ineqDF$boot.se == 0] <- NA
      ineqDF$se[ ineqDF$se == 0] <- NA
      return(ineqDF)
    }     
  })
  
  
  
  # Generate a view of the HETKB 
  output$dataTableInequal <- renderDataTable({
    if(!is.null(datasetInequal())){
      theData <- datasetInequal()
     
      if(input$summultiplier1==T){
        theData$inequal[theData$measure=='ti'] <- theData$inequal[theData$measure=='ti'] *1000
        theData$inequal[theData$measure=='mld'] <- theData$inequal[theData$measure=='mld'] *1000
        theData$se[theData$measure=='ti'] <- theData$se[theData$measure=='ti'] *1000
        theData$se[theData$measure=='mld'] <- theData$se[theData$measure=='mld'] *1000
        theData$se.lowerci[theData$measure=='ti'] <- theData$se.lowerci[theData$measure=='ti'] *1000
        theData$se.lowerci[theData$measure=='mld'] <- theData$se.lowerci[theData$measure=='mld'] *1000
        theData$se.upperci[theData$measure=='ti'] <- theData$se.upperci[theData$measure=='ti'] *1000
        theData$se.upperci[theData$measure=='mld'] <- theData$se.upperci[theData$measure=='mld'] *1000
        theData$boot.se[theData$measure=='ti'] <- theData$boot.se[theData$measure=='ti'] *1000
        theData$boot.se[theData$measure=='mld'] <- theData$boot.se[theData$measure=='mld'] *1000
        theData$boot.lowerci[theData$measure=='ti'] <- theData$boot.lowerci[theData$measure=='ti'] *1000
        theData$boot.lowerci[theData$measure=='mld'] <- theData$boot.lowerci[theData$measure=='mld'] *1000
        theData$boot.upperci[theData$measure=='ti'] <- theData$boot.upperci[theData$measure=='ti'] *1000
        theData$boot.upperci[theData$measure=='mld'] <- theData$boot.upperci[theData$measure=='mld'] *1000
        theData$combo.se[theData$measure=='ti'] <- theData$combo.se[theData$measure=='ti'] *1000
        theData$combo.se[theData$measure=='mld'] <- theData$combo.se[theData$measure=='mld'] *1000
        theData$combo.lowerci[theData$measure=='ti'] <- theData$combo.lowerci[theData$measure=='ti'] *1000
        theData$combo.upperci[theData$measure=='mld'] <- theData$combo.upperci[theData$measure=='mld'] *1000
        theData$combo.lowerci[theData$measure=='ti'] <- theData$combo.lowerci[theData$measure=='ti'] *1000
        theData$combo.upperci[theData$measure=='mld'] <- theData$combo.upperci[theData$measure=='mld'] *1000
        
      }
      if(input$summultiplier2==T){
        theData$inequal[theData$measure=='rci'] <- theData$inequal[theData$measure=='rci'] *100
        theData$se[theData$measure=='rci'] <- theData$se[theData$measure=='rci'] *100
        theData$se.lowerci[theData$measure=='rci'] <- theData$se.lowerci[theData$measure=='rci'] *100
        theData$se.upperci[theData$measure=='rci'] <- theData$se.upperci[theData$measure=='rci'] *100
        theData$boot.se[theData$measure=='rci'] <- theData$boot.se[theData$measure=='rci'] *100
        theData$boot.lowerci[theData$measure=='rci'] <- theData$boot.lowerci[theData$measure=='rci'] *100
        theData$boot.upperci[theData$measure=='rci'] <- theData$boot.upperci[theData$measure=='rci'] *100
        theData$combo.se[theData$measure=='rci'] <- theData$combo.se[theData$measure=='rci'] *100
        theData$combo.lowerci[theData$measure=='rci'] <- theData$combo.lowerci[theData$measure=='rci'] *100
        theData$combo.upperci[theData$measure=='rci'] <- theData$combo.upperci[theData$measure=='rci'] *100
      }
      
      
      var_names <- names(theData)
      # Round the data to selected significant figure
      theData[, c('inequal', 'se', 'boot.se', 'combo.se', 'se.lowerci', 'se.upperci', 'boot.lowerci', 'boot.upperci', 'combo.lowerci', 'combo.upperci' )] <- 
        round(theData[, c('inequal', 'se', 'boot.se', 'combo.se', 'se.lowerci', 'se.upperci', 'boot.lowerci', 'boot.upperci', 'combo.lowerci', 'combo.upperci' )], input$sumsigfig)
      
      if(input$se_type == 'analytic'){
        theData <- theData[, setdiff(var_names, c('boot.se', 'boot.upperci', 'boot.lowerci', 'combo.se', 'combo.lowerci', 'combo.upperci', 'se'))]
        names(theData)[names(theData)=="se.upperci" ] <- "Analytic Upper 95%CI"
        names(theData)[names(theData)=="se.lowerci" ] <- "Analytic Lower 95%CI"
      }
      if(input$se_type == 'bootstrap'){
        theData <- theData[, setdiff(var_names, c('se', 'se.upperci', 'se.lowerci', 'combo.se', 'combo.lowerci', 'combo.upperci', 'boot.se'))]
        names(theData)[names(theData)=="boot.upperci" ] <- "Bootstrap Upper 95%CI"
        names(theData)[names(theData)=="boot.lowerci" ] <- "Bootstrap Lower 95%CI"
      }
      if(input$se_type == 'both'){
        theData <- theData[, setdiff(var_names, c('combo.se', 'combo.lowerci', 'combo.upperci', 'se', 'boot.se'))]
        names(theData)[names(theData)=="boot.upperci" ] <- "Bootstrap Upper 95%CI"
        names(theData)[names(theData)=="boot.lowerci" ] <- "Bootstrap Lower 95%CI"
        names(theData)[names(theData)=="se.upperci" ] <- "Analytic Upper 95%CI"
        names(theData)[names(theData)=="se.lowerci" ] <- "Analytic Lower 95%CI"
      }
      
      if(input$se_type == 'balance'){
        theData <- theData[, setdiff(var_names, c('boot.se', 'boot.upperci', 'boot.lowerci', 'se', 'se.lowerci', 'se.upperci', 'combo.se'))]
        names(theData)[names(theData)=="combo.upperci" ] <- "Upper 95%CI"
        names(theData)[names(theData)=="combo.lowerci" ] <- "Lower 95%CI"
        
      }
      
      names(theData)[names(theData)=="country" ] <- "Country" 
      names(theData)[names(theData)=="year" ] <- "Year"
      names(theData)[names(theData)=="indic" ] <- "Health indicator" 
      names(theData)[names(theData)=="dimension" ] <- "Inequality dimension" 
      names(theData)[names(theData)=="inequal" ] <- "Estimate"
      names(theData)[names(theData)=="measure" ] <- "Summary measure"
      
      print(theData)
    }
    if(is.null(theData)){
      return(NULL)
    }
    if(nrow(theData)==0){
      return(NULL)
    }
    theData
  }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
  )
  
  
  # Generate a TEMPORARY view of the Comparison Summary Data
  output$dataTableBenchmark <- renderDataTable({
    if(!is.null(getData4())){
      theData <- getData4()[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate')]
      names(theData)[names(theData)=="country" ] <- "Country" 
      names(theData)[names(theData)=="year" ] <- "Year"
      names(theData)[names(theData)=="source" ] <- "Data source"
      names(theData)[names(theData)=="indic" ] <- "Health indicator" 
      names(theData)[names(theData)=="dimension" ] <- "Inequality dimension"
      names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
      names(theData)[names(theData)=="estimate" ] <- "Estimate"
      theData    
    }
  }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
  )
  
  
  # Generate a TEMPORARY view of the Comparison Summary Data
  output$dataTableCompSum <- renderDataTable({
    if(!is.null(getData5())){
      getData5()
    }
  }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
  )
  
  
  
  # Generate a reactive element for plotting the Managed Data.
  # Pass to the webpage using renderPlot(print(theDataPlot))
  theDataPlot <- reactive({ 
    print("Reactive: theDataPlot")
    plotData <- datasetInput()[, c('country', 'year', 'indic', 'subgroup', 'dimension', 'estimate', 'se')]
    if(nrow(plotData)>0){
      chartopt <- list()
      # Chart options for axis max and min values
      chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax1))
      chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin1))
      # Chart options for whether the chart only carries geographic region data
      geo_only <- geoOnly(plotData)
      if(geo_only){
        chartopt <- lappend(chartopt, 'geo_only' = geo_only)     
      }
      
      if(input$main_title1 != ""){
        chartopt <- lappend(chartopt, 'main_title' = input$main_title1)
      }
      if(input$xaxis_title1 != ""){
        chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title1)
      }
      if(input$yaxis_title1 != ""){
        chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title1)
      }
      
      
      
      if(input$long_names1==T){
        relevant_names <- which(names(healthIndicatorAbbreviations) %in% unique(plotData$indic))
        plotData$indic <- factor(plotData$indic,
                                 levels = names(healthIndicatorAbbreviations)[relevant_names],
                                 labels = unname(healthIndicatorAbbreviations)[relevant_names]) 
      }
      
      if(input$mainPanel == 'assess_inequality' & input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_bar'){        
        p <- plotFigure1(plotData, chartoptions=chartopt)
        return(p)
      }
      if(input$mainPanel == 'assess_inequality' & input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_line'){
        
        p <- plotFigure2(plotData, chartoptions=chartopt)
        return(p)
      }
    }
    else{
      return(NULL)
    }
    
  })  
  
  
  
  # Generate a reactive element for plotting the Summary Data.
  # Pass to the webpage using renderPlot(print(theDataPlot))
  theSummaryPlot <- reactive({ 
    print("Reactive: theSummaryPlot")
    if(is.null(datasetInequal())){
      return(NULL)
    } else {
      plotData <- datasetInequal()
      if(nrow(plotData)>0 ){
        
        chartopt <- list()
        chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax2))
        chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin2))
        
        if(input$main_title2 != ""){
          chartopt <- lappend(chartopt, 'main_title' = input$main_title2)
        }
        if(input$xaxis_title2 != ""){
          chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title2)
        }
        if(input$yaxis_title2 != ""){
          chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title2)
        }
        
        relevant.rows <- which(plotData$year %in% input$sumplotYears & plotData$indic %in% input$sumplotHealthIndicator & 
                                 plotData$dimension %in% input$sumplotEquityDimension & plotData$measure %in% input$sumplotSumMeasures)
        
        if(length(relevant.rows)>0){  # This will generally fail because the Health Indicator has not *yet* been selected
          
          plotData <- plotData[relevant.rows, ]      
          
          if(input$long_names2==T){
            relevant_names <- which(names(healthIndicatorAbbreviations) %in% unique(plotData$indic))
            plotData$indic <- factor(plotData$indic,
                                     levels = names(healthIndicatorAbbreviations)[relevant_names],
                                     labels = unname(healthIndicatorAbbreviations)[relevant_names]) 
          }
          
          
          if(input$mainPanel == 'assess_inequality' & input$assessment_panel == 'sumplot' & input$sumplot_type=='data_bar'){                   
            p <- plotFigure3(plotData, chartoptions=chartopt)
            return(p)
          }
          if(input$mainPanel == 'assess_inequality' & input$assessment_panel == 'sumplot' & input$sumplot_type=='data_line'){          
            p <- plotFigure4(plotData, chartoptions=chartopt)
            return(p)
          }    
        }
      }
      else{
        return(NULL)
      }
    }
  })  
  
  
  
  # Generate a reactive element for plotting the Disaggregated Comparison data.
  # Pass to the webpage using renderPlot(print(theDataPlot))
  theComparisonPlot1 <- reactive({ 
    print("Reactive: theComparisonPlot1")
    if(is.null(getData4a())){
      return(NULL)
    }
    else{
      plotData <- getData4a()[, c('country', 'year', 'indic', 'subgroup', 'dimension', 'estimate', 'se')]
      
      if(input$long_names3==T){
        relevant_names <- which(names(healthIndicatorAbbreviations) %in% unique(plotData$indic))
        plotData$indic <- factor(plotData$indic,
                                 levels = names(healthIndicatorAbbreviations)[relevant_names],
                                 labels = unname(healthIndicatorAbbreviations)[relevant_names]) 
      }
      
      
      chartopt <- list()
      chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax3))
      chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin3))
      
      if(input$main_title3 != ""){
        chartopt <- lappend(chartopt, 'main_title' = input$main_title3)
      }
      if(input$xaxis_title3 != ""){
        chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title3)
      }
      if(input$yaxis_title3 != ""){
        chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title3)
      }
      
      p <- plotFigure5(plotData, chartoptions=chartopt)
      return(p)
    }
  })  
  
  
  
  # Generate a reactive element for plotting the Summary Comparison data.
  # Pass to the webpage using renderPlot(print(theDataPlot))
  theComparisonPlot2 <- reactive({ 
    print("Reactive: theComparisonPlot2")
    print(getData5())
    if(is.null(getData5())){
      return(NULL)
    }
    if(nrow(getData5())==0){
      return(NULL)
    }    
    else{
      print('Never got here')
      plotData <- getData5()[, c('country', 'ccode', 'year', 'indic', 'estimate', 'dimension', 'measure', 'inequal', 'boot.se', 'se', 'anchor')]
      
      chartopt <- list()
      chartopt <- lappend(chartopt, 'xaxmax' = as.integer(input$xaxis_limitsmax4))
      chartopt <- lappend(chartopt, 'xaxmin' = as.integer(input$xaxis_limitsmin4))
      chartopt <- lappend(chartopt, 'yaxmax' = as.integer(input$yaxis_limitsmax4))
      chartopt <- lappend(chartopt, 'yaxmin' = as.integer(input$yaxis_limitsmin4))
      chartopt <- lappend(chartopt, 'yaxmin' = as.integer(input$yaxis_limitsmin4))
      
      if(input$points_ccode == TRUE){
        chartopt <- lappend(chartopt, 'points_dot' = input$points_ccode)
      }
      
      if(input$main_title4 != ""){
        chartopt <- lappend(chartopt, 'main_title' = input$main_title4)
      }
      if(input$xaxis_title4 != ""){
        chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title4)
      }
      if(input$yaxis_title4 != ""){
        chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title4)
      }
      
      p <- plotFigure6(plotData, chartoptions=chartopt)
      return(p)
    }
  })  
  
  benchmarkText <- reactive({
    theText <- NULL
    input$getcomparisondata1
    isolate({
      theText <- paste(readLines('./www/benchmarkSelection.html'), collapse=" ")
    })
    print(theText)
    return(theText)
  })
  
  #   output$benchmarktxt <- htmlOutput(benchmarkText())
  
  
  
  output$theDataPlot_web <- renderPlot({
    print(theDataPlot())  # Remember that print(theDataPlot) just prints the code
  }, res=90, height=exprToFunction(input$plot_height1), width=exprToFunction(input$plot_width1))
  
  
  output$theComparisonPlot1_web <- renderPlot({
    if(is.null(theComparisonPlot1())){
      return(NULL)
    }
    print(theComparisonPlot1())  # Remember that print(theDataPlot) just prints the code
  }, res=90, height=exprToFunction(input$plot_height2), width=exprToFunction(input$plot_width2))
  
  
  output$theComparisonPlot2_web <- renderPlot({    
    if(is.null(theComparisonPlot2())){
      return(NULL)
    }
    print(theComparisonPlot2())  # Remember that print(theDataPlot) just prints the code
  }, res=90, height=exprToFunction(input$plot_height3), width=exprToFunction(input$plot_width3))
  
  
  
  ## Download the plot of the managed data
  
  # Include a downloadable file of the plot in the output list.
  output$theDataPlot.print <- downloadHandler(
    # downloadHandler(filename, content, contentType)'
    
    # filename  -- make sure it has a png extension
    function(){
      testStr <- input$downloadPlotFileName
      if(tolower(substr(testStr, nchar(testStr)-3, nchar(testStr))) != ".png"){
        testStr <- paste(testStr, "png", sep='.')
      }      
      return(testStr)
    },
    
    # content
    content <-function(file) {
      png <- function(..., width, height) {  
        # This is included to manage the size of the png plot 
        # see: ... http://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
        grDevices::png(...,  
                       width=input$downloadPlotWidth, 
                       height=input$downloadPlotHeight, 
                       res = 300, # as.integer(input$downloadPlotHeightRes), 
                       units = "cm")
      }
      ggsave(file, plot=theDataPlot(), device=png)
    }
  )
  
  
  output$theSumPlot_web <- renderPlot({
    print(theSummaryPlot())  # Remember that print(theSummaryPlot) just prints the code
  }, res=90, height=exprToFunction(input$plot_height_sum), width=exprToFunction(input$plot_width_sum))
  
  
  
  
  #####  Comparison Plot 1  Download
  # Create a download button contingent on the existence of a plot of the comparison disaggregated data
  output$downloadCompplot1 <- renderUI({
    thePlot <- theComparisonPlot1()
    if(is.null(thePlot)){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadCompplot1", "Download Plot", class = "btn-primary"))
    }  
  })
  
  
  
  # Handler for downloading the data selected in the modal download plot
  output$downloadCompPlot1 <- downloadHandler(
    filename = function() { 
      paste(input$equityCountry, '_comp_', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
      print(theComparisonPlot1()) 
      dev.off()
    }
  )   
  
  
  #####  Comparison Plot 2  Download
  # Create a download button contingent on the existence of a plot of the comparison disaggregated data
  output$downloadCompplot2 <- renderUI({
    thePlot <- theComparisonPlot2()
    if(is.null(thePlot)){
      return(NULL)
    } else {
      list(br(),
           actionButton("downloadCompplot2", "Download Plot", class = "btn-primary"))
    }  
  })
  
  
  
  # Handler for downloading the data selected in the modal download plot
  output$downloadCompPlot2 <- downloadHandler(
    filename = function() { 
      paste(input$equityCountry, '_comp_', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
      print(theComparisonPlot2()) 
      dev.off()
    }
  )   
  
  
  
  
})
