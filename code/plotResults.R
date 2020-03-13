getPieChart <- function(dataset,column,fontSize,titleStr,alphaVal,legendStr,addLabels){
  nargin  <- length(as.list(match.call())) -1
  if (nargin<4){titleStr <- ''}
  if (nargin<5){alphaVal <- 1}
  if (nargin<6){legendStr <- ''}
  if (nargin<7){addLabels <- TRUE}
  classes          <- unique(toupper(dataset[,column]))
  dataset[,column] <- toupper(dataset[,column])
  N_elements <- c()
  prop       <- c()
  for (element in classes){
    entries    <- length(which(dataset[,column]==element))
    N_elements <- c(N_elements,entries)
    percentage <- entries*100/nrow(dataset)
    prop       <- c(prop,percentage)
  }
  prop <- round(prop, digits = 2)
  df   <- data.frame(classes,N_elements,prop,stringsAsFactors = FALSE)
  # Add label position
  df <- df %>%arrange(desc(classes))%>%mutate(lab.ypos = cumsum(prop) - 0.5*prop)
  #Create plot object
  p <- ggplot(df, aes(x = 2, y = prop, fill = classes)) +
    geom_bar(stat = "identity", color = "white",width=0.2) + coord_polar('y',start=0) 
  if (addLabels){
    p <- p + geom_text_repel(aes(y=lab.ypos,label = prop), color = "black",size=fontSize)
  }
  if (length(classes)>3){x_start=1} 
  else{x_start=1.6}
  p <- p + xlim(x_start, 2.2) + scale_fill_viridis(discrete = T, option = "E",alpha=alphaVal)
  if (nchar(titleStr)>=1){
    p <- p + ggtitle(titleStr) 
  }
  if (nchar(legendStr)>=1){
    p <- p + theme_void(base_size = 2*fontSize,legend.title = legendStr) + theme(legend.position = c(0.5,0.5),legend.title = element_blank())
  }else{
    p <- p + theme_void(base_size = 2*fontSize) + theme(legend.position = c(0.5,0.5),legend.title = element_blank())
  }
  plot(p)
}

#Function to get a multivariable bar plot (different classes and groups)
barPlot_2vars <- function(DF,xLabel,yLabel,legendStr,fontSize,mode){
p <- ggplot(DF, aes(fill=V2, y=counts, x=V1)) + geom_bar(position=mode, stat="identity")
p <- p + theme_bw(base_size = 2*fontSize) + xlab(xLabel) + ylab(yLabel)
p <- p + labs(fill = legendStr) + scale_fill_viridis(discrete = T, option = "E")
plot(p)
}