######### plotter.R                                                                         #
# This file manages the six different kinds of plot used in the equity analysis tool kit.   #
# Plot 1: Barchart for a single country (Disaggregation of data)                            #
# Plot 2: Horizontal line chart for a single country (Disaggregation of data)               #
# Plot 3: Barchart for a single country (Summary of data)                                   #
# Plot 4: Slopped line chart for a single country (Summary of data)                         #
# Plot 5: Horizontal line chart for multiple countries (Disaggregation of data)             #
# Plot 6: Scatter plot for multiple countries (Summary of data)                             #
#############################################################################################


library(ggplot2)
library(grid)
library(RColorBrewer)

#######PLOT 1
plotFigure1 <- function(plotData, chartoptions=NULL){
  # Plot 1: Barchart for a single country (Disaggregation of data)
  
  print("plotFigure1() in plotter.R")
  print(chartoptions)
  
  if(!('geo_only' %in% names(chartoptions))){
    legendRow <- 1
    plotPalette <- c()
    sexPalette <- c('#FF7F0F', '#B85A0D')  # ORANGE
    ecoPalette <- c("#D8BD35", '#ADB828', '#82A93C','#54A338', '#29A03C' )  # GREENS
    eduPalette <- c('#E75727', '#D23E4E', '#C94D8C')  # REDS
    ruralPalette <- c('#3CB7CC', '#39737C')  # TURQUOISE
    legendBreaks <- c()
    
    educationLevels <- length(unique(plotData$subgroup[plotData$dimension=="Mother's education"]))
    if(educationLevels==2){
      eduPalette <- eduPalette[c(1,3)]
    }
    if(educationLevels==1){
      eduPalette <- eduPalette[2]
    }
    
    if('Economic status' %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, ecoPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Economic status"])))
    }
    
    if("Mother's education" %in% plotData$dimension){ 
      ##  NEED TO PUT A LINE HERE TO CHECK THE NUMBER OF education CATEGORIES
      plotPalette <- c(plotPalette, eduPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Mother's education"])))
    }
    
    if("Place of residence" %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, ruralPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Place of residence"])))
    }
    if('Sex' %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, sexPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Sex"])))
    }
    if("Geographic region" %in% plotData$dimension){
      numberOfRegions <- length(unique(plotData$subgroup[plotData$dimension=="Geographic region"]))
      plotPalette <- c(plotPalette, rep('#00008B', numberOfRegions))
    }
    
    if(length(legendBreaks)>2){
      legendRow <- 2
    }
    if("Mother's education" %in% plotData$dimension & length(legendBreaks)>2){
      legendRow <- 3
    }
    if("Economic status" %in% plotData$dimension){
      legendRow <- 5
    }
  }
  
  
  p <- ggplot(plotData, aes(x = as.factor(year), y = estimate, fill=subgroup)) + 
    geom_bar(position=position_dodge(), stat="identity", color='white')
  
  p <- p + facet_grid(indic ~ dimension, scale="free_x", space="free" )
  
  if(!("Geographic region" %in% plotData$dimension)){
    p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill = "White"), 
                   panel.grid.major = element_line(colour = "Grey"))
    
    p <- p + scale_fill_manual(name="",
                               values = plotPalette)
  } else {
    
    p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill = "White"), 
                   panel.grid.major = element_line(colour = "Grey"),
                   legend.title=element_text(colour="white"))
    
  }
  
  if("Geographic region" %in% plotData$dimension & !('geo_only' %in% names(chartoptions))){
    
    p <- p + scale_fill_manual(name="",
                               breaks=legendBreaks,
                               labels=legendBreaks,
                               values=plotPalette)
  } 
  
  if('geo_only' %in% names(chartoptions)){
    p <- p +  guides(fill=guide_legend(ncol=5))
  } else {
    p <- p + guides(fill=guide_legend(nrow=legendRow))
  }
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("Year")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Estimate")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  if(F){  #  YES! This is skipped over
    p <- p + geom_errorbar(aes(ymin=lower_95ci, ymax=upper_95ci), position=position_dodge(.9))
  }
  p <- p + ylim(chartoptions$axmin,  chartoptions$axmax) 
  
  
  p
  return(p)
}



#######PLOT 2
plotFigure2 <- function(plotData, chartoptions=NULL){
  #  Plot 2: Horizontal line chart for a single country (Disaggregation of data)
  
  if(!('geo_only' %in% names(chartoptions))){
    plotPch <- c(17, 15, 18, 19, 15)
    legendRow <- 1
    plotPalette <- c()
    sexPalette <- c('#FF7F0F', '#B85A0D')  # ORANGE
    ecoPalette <- c("#D8BD35", '#ADB828', '#82A93C','#54A338', '#29A03C' )  # GREENS
    eduPalette <- c('#E75727', '#D23E4E', '#C94D8C')  # REDS
    ruralPalette <- c('#3CB7CC', '#39737C')  # TURQUOISE
    
    educationLevels <- length(unique(plotData$subgroup[plotData$dimension=="Mother's education"]))
    if(educationLevels==2){
      eduPalette <- eduPalette[c(1,3)]
    }
    if(educationLevels==1){
      eduPalette <- eduPalette[2]
    }
    
    
    legendBreaks <- c()
    if('Economic status' %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, ecoPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Economic status"])))
    }
    
    if("Mother's education" %in% plotData$dimension){ 
      ##  NEED TO PUT A LINE HERE TO CHECK THE NUMBER OF education CATEGORIES
      plotPalette <- c(plotPalette, eduPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Mother's education"])))
    }
    
    if("Place of residence" %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, ruralPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Place of residence"])))
    }
    if('Sex' %in% plotData$dimension){ 
      plotPalette <- c(plotPalette, sexPalette)
      legendBreaks <- c(legendBreaks, as.character(unique(plotData$subgroup[plotData$dimension=="Sex"])))
    }
    if("Geographic region" %in% plotData$dimension){
      numberOfRegions <- length(unique(plotData$subgroup[plotData$dimension=="Geographic region"]))
      plotPalette <- c(plotPalette, rep('#00008B', numberOfRegions))
    }
    
    if(length(legendBreaks)>2){
      legendRow <- 2
    }
    if("Mother's education" %in% plotData$dimension & length(legendBreaks)>2){
      legendRow <- 3
    }
    if("Economic status" %in% plotData$dimension){
      legendRow <- 5
    }
  }
  
  p <- ggplot(plotData, aes(estimate, as.factor(year))) + geom_line()
  
  p <- p + geom_point(aes(color=subgroup, shape=dimension), size=4) 
  
  if(!('geo_only' %in% names(chartoptions))){
    p <- p + scale_shape_manual(values=plotPch, guide=F)
  } else {
    p <- p + scale_shape_manual(values=15, guide=F)
  }
  
  if(!("Geographic region" %in% plotData$dimension)){
    p <- p + scale_colour_manual(name="",
                                 values = plotPalette)
  }
  if("Geographic region" %in% plotData$dimension & !('geo_only' %in% names(chartoptions))){
    
    p <- p + scale_colour_manual(name="",
                                 breaks=legendBreaks,
                                 labels=legendBreaks,
                                 values=plotPalette)
  }
  
  
  if(F){
    p <- p + facet_grid(" indic + dimension ~ . ", scale="free_y", space = "free_y")
  }
  if(T){
    p <- p + facet_grid("dimension + indic ~ . ", scale="free_y", space = "free_y")  
  }
  
  if(!('geo_only' %in% names(chartoptions))){
    p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill='white'),
                   panel.grid.major = element_line(colour = "dark grey"),
                   panel.grid.minor = element_line(colour = "dark grey"),
                   axis.line = element_line(colour = "black"),
                   legend.key = element_rect(fill = "white"))
  } else {    
    p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill='white'),
                   panel.grid.major = element_line(colour = "dark grey"),
                   panel.grid.minor = element_line(colour = "dark grey"),
                   axis.line = element_line(colour = "black"),
                   legend.key = element_rect(fill = "white"),
                   legend.title=element_text(colour="white"))
    
  }
  
  p <- p +  guides(fill=guide_legend(ncol=1, title=NULL))
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("Estimate")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Year")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  if(!('geo_only' %in% names(chartoptions))){
    p <- p + guides(col=guide_legend(nrow=legendRow))
  } else {
    p <- p + guides(col=guide_legend(ncol=4))
  }
  
  p <- p + xlim(chartoptions$axmin,  chartoptions$axmax) 
  
  p
}


#### PLOT 3
plotFigure3 <- function(plotData, chartoptions=NULL){
  # Plot 3: Barchart for a single country (Summary data)
  
  print("plotFigure3() in plotter.R")
  
  # Make sure that the data frame includes all the possible bars by adding missing data across the factor levels
  plotData <- plotData[,c('year', 'indic', 'inequal')]
  plotData <-  rbind(plotData, cbind(expand.grid (year = unique(plotData$year), 
                                                  indic = unique(plotData$indic),
                                                  inequal = NA)))
  
  
  p <- ggplot(plotData, aes(x = as.factor(year), y = inequal, fill=indic)) + 
    geom_bar(position=position_dodge(), stat="identity")
  
  
  p <- p + ylab("Estimate") + xlab("Year")
  
  
  p <- p + theme(panel.background = element_rect(fill='white'),
                 panel.grid.major = element_line(colour = "dark grey"),
                 panel.grid.minor = element_line(colour = "dark grey"),
                 axis.line = element_line(colour = "black"),
                 legend.key = element_rect(fill = "white"),
                 legend.position="bottom",
                 legend.title=element_blank())
  
  if('errorbars' %in% names(chartoptions)){
    if(chartoptions$errorbars == T){
      p <- p + geom_errorbar(aes(ymin=lower_95ci, ymax=upper_95ci), position=position_dodge(.9))
    }
  }
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("Year")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Estimate")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  p <- p +  guides(fill=guide_legend(ncol=1))
  
  p <- p + ylim(chartoptions$axmin,  chartoptions$axmax)
  
  p
  return(p)
}



####PLOT 4
plotFigure4 <- function(plotData, chartoptions=NULL){
  #  Plot 4: Line chart for a single country (Disaggregation of data)
  print("plotFigure4() in plotter.R")
  
  p <- ggplot(plotData, aes(x=as.integer(year), y=inequal, group=indic, color=indic))
  p <- p + geom_line() + geom_point()
  
  p <- p + theme(panel.background = element_rect(fill='white'),
                 panel.grid.major = element_line(colour = "dark grey"),
                 panel.grid.minor = element_line(colour = "dark grey"),
                 axis.line = element_line(colour = "black"),
                 legend.key = element_rect(fill = "white"),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position="bottom",
                 legend.title=element_blank())
  
  p <- p + scale_x_continuous(breaks=sort(unique(plotData$year)))
  
  print(plotData)
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("Year")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Estimate")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  p <- p + ylim(chartoptions$axmin,  chartoptions$axmax)
  
  p <- p + guides(col=guide_legend(ncol=1))
  
  return(p)
}



plotFigure5 <- function(plotData, chartoptions=NULL){
  #  Plot 5: Horizontal line chart for benchmark countries (Disaggregation of data)
  plotPch <- c(19)
  plotPalette <- c()
  sexPalette <- c('#FF7F0F', '#B85A0D')  # ORANGE
  ecoPalette <- c("#D8BD35", '#ADB828', '#82A93C','#54A338', '#29A03C' )  # GREENS
  eduPalette <- c('#E75727', '#D23E4E', '#C94D8C')  # REDS
  ruralPalette <- c('#3CB7CC', '#39737C')  # TURQUOISE
  thedimension <- unique(plotData$dimension)
  
  educationLevels <- length(unique(plotData$subgroup[plotData$dimension=="Mother's education"]))
  if(educationLevels==3){
    plotPalette <- eduPalette
  }
  if(educationLevels==2){
    plotPalette <- eduPalette[c(1,3)]
  }
  if(educationLevels==1){
    plotPalette <- eduPalette[2]
  }
  
  if('Economic status' == thedimension){ plotPalette <- ecoPalette }
  if("Place of residence" == thedimension){ plotPalette <- ruralPalette }
  if('Sex' == thedimension){ plotPalette <- sexPalette }
  if("Geographic region" == thedimension){
    numberOfRegions <- length(unique(plotData$subgroup[plotData$dimension=="Geographic region"]))
    plotPalette <- rep('#00008B', numberOfRegions) 
  }
  
  p <- ggplot(plotData, aes(estimate, as.factor(year))) + geom_line()
  
  if(!("Geographic region" %in% plotData$dimension)){
    p <- p + geom_point(aes(color=subgroup), size=4)
    p <- p + scale_colour_manual(name="", values = plotPalette)
  }
  
  if("Geographic region" %in% plotData$dimension){ 
    p <- p + geom_point(size=4, color='#00008B')
    p <- p + scale_colour_manual(values=plotPalette, guide=F)
  }
  
  
  p <- p + facet_grid("indic + country ~ . ", scale="free_y", space = "free_y")
  
  p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                 strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                             hjust = 0.5, vjust = 0.5),
                 panel.background = element_rect(fill='white'),
                 panel.grid.major = element_line(colour = "dark grey"),
                 panel.grid.minor = element_line(colour = "dark grey"),
                 axis.line = element_line(colour = "black"),
                 legend.key = element_rect(fill = "white"))  
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("Estimate")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Year")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  p <- p + xlim(chartoptions$axmin,  chartoptions$axmax)
  
  p <- p + guides(col=guide_legend(nrow=5))
  
  p
}




plotFigure6 <- function(plotData, chartoptions=NULL){
  #  Plot 6: Scatter plot showing benchmark countries National Average against inequality
  plotPch <- c(19, 15)
  plotPalette <- c('#00616B', '#6B0A00')
  
  plotData <- plotData[order(plotData$anchor), ]
  
  p <- ggplot(plotData, aes(x=estimate, y=inequal), color=plotPalette)
  
  if(!'points_dot' %in% names(chartoptions)){  # Plot dots, or Country codes
    p <- p + geom_point(aes(color=as.factor(anchor), shape=as.factor(anchor)), size=4)
    
    p <- p + theme(legend.position="bottom", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill='white'),
                   panel.grid.major = element_line(colour = "dark grey"),
                   panel.grid.minor = element_line(colour = "dark grey"),
                   axis.line = element_line(colour = "black"),
                   legend.key = element_rect(fill = "white"))
    
  } else {
    p <- p + geom_text(aes(label=ccode, color=as.factor(anchor)), hjust=0.5, vjust=0.5, size=3.5)
    
    p <- p + theme(legend.position="none", panel.margin = unit(1, "mm"), 
                   strip.text.y = element_text(colour = "black", angle = 0, size = 10,
                                               hjust = 0.5, vjust = 0.5),
                   panel.background = element_rect(fill='white'),
                   panel.grid.major = element_line(colour = "dark grey"),
                   panel.grid.minor = element_line(colour = "dark grey"),
                   axis.line = element_line(colour = "black"))
    
  }
  
  p <- p + scale_shape_manual(name  ="",
                              breaks=c(0, 1),
                              labels=c("Benchmark countries", plotData$country[plotData$anchor==1][1]),
                              values=plotPch)
  
  p <- p + scale_colour_manual(name  ="",
                               breaks=c(0, 1),
                               labels=c("Benchmark countries", plotData$country[plotData$anchor==1][1]),
                               values=plotPalette)  
  
  p <- p + xlim(chartoptions$xaxmin,  chartoptions$xaxmax)
  
  p <- p + ylim(chartoptions$yaxmin,  chartoptions$yaxmax)
  
  
  
  if('xaxis_title' %in% names(chartoptions)){
    p <- p +   xlab(chartoptions$xaxis_title)
  } else {
    p <- p + xlab("National average")
  }
  
  if('yaxis_title' %in% names(chartoptions)){
    p <- p +   ylab(chartoptions$yaxis_title)
  } else {
    p <- p + ylab("Inequality")
  }
  
  if('main_title' %in% names(chartoptions)){
    p <- p +   labs(title=chartoptions$main_title)
  }
  
  p
}

