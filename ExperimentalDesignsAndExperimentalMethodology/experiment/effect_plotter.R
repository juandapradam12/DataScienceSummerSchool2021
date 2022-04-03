#This script contains code for a function (effect_plotter) 
#that takes a regression table output and creates an effect plot,
#using ggplot2.

#The functions is designed in particular for regressions with 
#factor variables, such that the variable label and 
#all levels (including reference) are displayed in the plot.

#This function was written by Kirk Bansak, and last updated on
#March 5, 2018.

library(ggplot2)


# Arguments ---------------------------------------------------------------

#1. model.output: coeftest output object, using desired variance estimator

#2. names.variables: vector of the variable names (strings) to display, 
#in same order as model

#3. names.levels: list of vectors of levels (strings), incl. reference,
#for each variable to display, with levels in same order as model,
#beginning with reference level.

#4. effect.label: string to display describing the x-axis

#5. x.lower: number for x-axis lower bound (NULL uses ggplot default)

#6. x.upper: number for x-axis upper bound (NULL uses ggplot default)


# Function ----------------------------------------------------------------


effect_plotter <- function(model.output, 
                           names.variables,
                           names.levels,
                           effect.label,
                           x.lower = NULL,x.upper = NULL){

  if(length(names.variables) != length(names.levels)){
    stop("Number of sets of levels does not match number of variables!")
  }
  
  n.vars <- length(names.variables)
  name <- c()
  code <- c()
  group <- c()
  pe <- c()
  se <- c()
  solo <- c()
  
  for (i in 1:n.vars){
    
    nv <- names.variables[i]
    nl <- names.levels[[i]]
  
    if(length(nl) > 1){
      name <- c(name,nv,nl,NA)
    } else {
      name <- c(name,nv,NA)
    }
    
    code.tmp <- c()
    solo.tmp <- c()
    if (length(nl) > 1){
      code.tmp <- c(0,0,rep(1,length(nl) - 1),0)
      solo.tmp <- c(0,0,rep(0,length(nl) - 1),0)
    } else {
      code.tmp <- c(1,0)
      solo.tmp <- c(1,0)
    }
    code <- c(code,code.tmp)
    solo <- c(solo,solo.tmp)
    
    group.tmp <- c()
    if (length(nl) > 1){
      group.tmp <- c("empty",rep(paste("group",i,sep=""),
                                 length(nl)),"empty")
    } else {
      group.tmp <- c(1,"empty")
    }
    group <- c(group,group.tmp)

    pe.tmp <- c()
    se.tmp <- c()
    if (length(nl) > 1){
      pe.tmp <- c(NA,rep(0,length(nl)),NA)
      se.tmp <- c(NA,rep(0,length(nl)),NA)
    } else {
      pe.tmp <- c(0,NA)
      se.tmp <- c(0,NA)
    }
    pe <- c(pe,pe.tmp)
    se <- c(se,se.tmp)
    
  }
  
  code[code==1] <- seq(1:sum(code))
  pdat <- data.frame(name,group,code,pe,se,solo)
  pdat <- pdat[-nrow(pdat),]
  pdat$order <- rev(seq(1:nrow(pdat)))
  
  #Extract pe and se estimates
  pevec <- model.output[,1]
  sevec <- model.output[,2]
  
  #Input the estimates into the framework dataframe
  for (i in 1:max(pdat$code)){
    pdat$pe[pdat$code == i] <- pevec[i+1]
    # +1 because of intercept
    pdat$se[pdat$code == i] <- sevec[i+1]
  }
  
  pdat$name <- as.character(pdat$name)
  pdat$group <- as.character(pdat$group)
  pdat$name[is.na(pdat$name)] <- ""
  
  if (!is.null(x.lower) & !is.null(x.upper)){
    theplot <- plotit(d = pdat, effect.label = effect.label,
                      x.lower = x.lower, x.upper = x.upper)
  } else {
    theplot <- plotit(d = pdat, effect.label = effect.label)
  }
  
  return(theplot)
  
}



plotit <- function(d,effect.label,x.lower = NULL,x.upper = NULL){  
  
  CIs <- function(d){
    d$upper <-d$pe + 1.96*d$se
    d$lower <-d$pe - 1.96*d$se
    return(d)
  }
  d<- CIs(d)
  
  plot.labels <- as.character(d$name)
  plot.labels[d$group != "empty" & 
                !is.na(plot.labels) & d$solo == 0] <- 
    paste("    ",
          plot.labels[d$group != "empty" & 
                        !is.na(plot.labels) & d$solo == 0], sep = "")
  plot.face <- ifelse(is.na(d$pe) | d$solo == 1, "bold", "plain")
  d$ref <- ifelse(d$pe != 0 | is.na(d$pe), "normal", "reference")
  
  d$order <- as.factor(d$order)
  
  #Construct plot
  p <- ggplot(d,aes(y=pe,x=order,color=group,size=0.3,shape=ref)) + 
    scale_shape_manual(values = c(16,1))

  if (!is.null(x.lower) & !is.null(x.upper)){
    p <- p + coord_flip(ylim = c(x.lower,x.upper)) 
  } else {
    p <- p + coord_flip() 
  }
  p <- p + ylab(effect.label)
  p <- p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype=1) 
  p <- p + geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),
                           position="dodge",size=.3)
  p <- p + scale_colour_discrete("Attribute:") + 
    scale_x_discrete(name="",labels=rev(plot.labels)) 

  theme_bw1 <- function(base_size = 7, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family)
      theme(
        axis.text.x = element_text(size = base_size, colour = "black",
                                   hjust = .5 , vjust=1),
        axis.text.y = element_text(size = base_size , colour = "black",
                                   hjust = 0 , vjust=.5,
                                   face = rev(plot.face)),
        axis.ticks.y = element_line(colour = "grey50"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = base_size,angle=90,
                                    vjust=.01,hjust=.1),
        legend.position = "none"
      )
  }
  
  p <- p + theme_bw1()
  p <- p + theme(panel.grid.major = element_line(size = 0.28)) + 
    theme(panel.grid.minor = element_blank())
  p <- p + ggtitle("") + 
    theme(plot.title = element_text(lineheight=.8, 
                                    face="bold", vjust=1.5))
  
  return(p)
  
}



#For this version, model.output is a list of model.outputs
#and names.sets is a vector of names for the (sub)sets of data
#to which each model.output pertains

effect_plotter2 <- function(model.outputs, 
                            names.variables,
                            names.levels,
                            effect.label,
                            x.lower = NULL,x.upper = NULL,
                            names.sets,
                            nrows = 1){
  
  if(length(names.variables) != length(names.levels)){
    stop("Number of sets of levels does not match number of variables!")
  }
  
  pdats <- list()
  
  for (k in 1:length(model.outputs)){
    
    n.vars <- length(names.variables)
    name <- c()
    code <- c()
    group <- c()
    pe <- c()
    se <- c()
    solo <- c()
    
    for (i in 1:n.vars){
      
      nv <- names.variables[i]
      nl <- names.levels[[i]]
      
      if(length(nl) > 1){
        name <- c(name,nv,nl,NA)
      } else {
        name <- c(name,nv,NA)
      }
      
      code.tmp <- c()
      solo.tmp <- c()
      if (length(nl) > 1){
        code.tmp <- c(0,0,rep(1,length(nl) - 1),0)
        solo.tmp <- c(0,0,rep(0,length(nl) - 1),0)
      } else {
        code.tmp <- c(1,0)
        solo.tmp <- c(1,0)
      }
      code <- c(code,code.tmp)
      solo <- c(solo,solo.tmp)
      
      group.tmp <- c()
      if (length(nl) > 1){
        group.tmp <- c("empty",rep(paste("group",i,sep=""),
                                   length(nl)),"empty")
      } else {
        group.tmp <- c(1,"empty")
      }
      group <- c(group,group.tmp)
      
      pe.tmp <- c()
      se.tmp <- c()
      if (length(nl) > 1){
        pe.tmp <- c(NA,rep(0,length(nl)),NA)
        se.tmp <- c(NA,rep(0,length(nl)),NA)
      } else {
        pe.tmp <- c(0,NA)
        se.tmp <- c(0,NA)
      }
      pe <- c(pe,pe.tmp)
      se <- c(se,se.tmp)
      
    }
    
    code[code==1] <- seq(1:sum(code))
    pdat <- data.frame(name,group,code,pe,se,solo)
    pdat <- pdat[-nrow(pdat),]
    pdat$order <- rev(seq(1:nrow(pdat)))
    
    #Extract pe and se estimates
    pevec <- model.outputs[[k]][,1]
    sevec <- model.outputs[[k]][,2]
    
    #Input the estimates into the framework dataframe
    for (i in 1:max(pdat$code)){
      pdat$pe[pdat$code == i] <- pevec[i+1]
      # +1 because of intercept
      pdat$se[pdat$code == i] <- sevec[i+1]
    }
    
    pdat$name <- as.character(pdat$name)
    pdat$group <- as.character(pdat$group)
    pdat$name[is.na(pdat$name)] <- ""
    
    pdats[[k]] <- pdat
    pdats[[k]]$set <- names.sets[k]
    rm(pdat)
    
  }
  
  sdat <- do.call("rbind", pdats)
  sdat$set <- factor(sdat$set, levels = names.sets)

  if (!is.null(x.lower) & !is.null(x.upper)){
    theplot <- plotit2(d = sdat, effect.label = effect.label,
                      x.lower = x.lower, x.upper = x.upper, nrows = nrows)
  } else {
    theplot <- plotit2(d = sdat, effect.label = effect.label, nrows = nrows)
  }
  
  return(theplot)
  
}


plotit2 <- function(d,effect.label,x.lower = NULL,x.upper = NULL,nrows = 1){  
  
  CIs <- function(d){
    d$upper <-d$pe + 1.96*d$se
    d$lower <-d$pe - 1.96*d$se
    return(d)
  }
  d<- CIs(d)
  
  plot.labels <- as.character(d$name)
  plot.labels[d$group != "empty" & 
                !is.na(plot.labels) & d$solo == 0] <- 
    paste("    ",
          plot.labels[d$group != "empty" & 
                        !is.na(plot.labels) & d$solo == 0], sep = "")
  plot.face <- ifelse(is.na(d$pe) | d$solo == 1, "bold", "plain")
  d$ref <- ifelse(d$pe != 0 | is.na(d$pe), "normal", "reference")
  
  d$order <- as.factor(d$order)
  
  #Construct plot
  p <- ggplot(d,aes(y=pe,x=order,color=group,size=0.3,shape=ref)) + 
    scale_shape_manual(values = c(16,1))
  
  if (!is.null(x.lower) & !is.null(x.upper)){
    p <- p + coord_flip(ylim = c(x.lower,x.upper)) 
  } else {
    p <- p + coord_flip() 
  }
  p <- p + ylab(effect.label)
  p <- p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype=1) 
  p <- p + geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),
                           position="dodge",size=.3)
  p <- p + scale_colour_discrete("Attribute:") + 
    scale_x_discrete(name="",labels=rev(plot.labels)) 
  
  theme_bw1 <- function(base_size = 7, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family)
    theme(
      axis.text.x = element_text(size = base_size, colour = "black",
                                 hjust = .5 , vjust=1),
      axis.text.y = element_text(size = base_size , colour = "black",
                                 hjust = 0 , vjust=.5,
                                 face = rev(plot.face)),
      axis.ticks.y = element_line(colour = "grey50"),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = base_size,angle=90,
                                  vjust=.01,hjust=.1),
      legend.position = "none"
    )
  }
  
  p <- p + theme_bw1()
  p <- p + theme(panel.grid.major = element_line(size = 0.28)) + 
    theme(panel.grid.minor = element_blank())
  p <- p + ggtitle("") + 
    theme(plot.title = element_text(lineheight=.8, 
                                    face="bold", vjust=1.5))
  p <- p + facet_wrap(~set, nrow = nrows)
  
  return(p)
  
}

