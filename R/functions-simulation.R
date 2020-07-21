theme_tufte_revised <- function(base_size = 11, base_family = "Gill Sans", ticks = TRUE) {
  
  ret <- ggplot2::theme_bw(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = 'black'),
      axis.title.x = ggplot2::element_text(vjust = -0.3),
      axis.title.y = ggplot2::element_text(vjust = 0.8),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face="plain"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  
  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  }
  
  ret
}


get_params <-  function(x,params) {
  map2(params,x,~(
    .x(.y)
  ))
} 
qfixed <- function(x, value) value


get_takeup_coef <- function(df  = df_wtp_and_costs,params) {
  sampled <- sample(df$iteration,1)
  tmp <- 
    df  %>% 
    filter(iteration == sampled & fpl == params$pop_fpl & type == params$plan_type & outcome=="s") %>% 
    select(-type,-outcome,-fpl,-iteration) 
  
  tmp_coef <- 
    tmp %>% 
    gather(coef,value) %>% 
    mutate(value = as.numeric(paste0(value))) %>% 
    pull(value) 
  names(tmp_coef) <- names(tmp)
  
  return(tmp_coef)
  
}

get_cost_coef <- function(df = df_wtp_and_costs, params) {
  sampled <- sample(df$iteration,1)
  tmp <- 
    df %>% 
    filter(iteration == sampled & fpl == params$pop_fpl & type == params$plan_type & outcome=="cost") %>% 
    select(-type,-outcome,-fpl,-iteration) 
  
  tmp_coef <- 
    tmp %>% 
    gather(coef,value) %>% 
    mutate(value = as.numeric(paste0(value))) %>% 
    pull(value) 
  names(tmp_coef) <- names(tmp)
  
  return(tmp_coef)
  
}

get_takeup <- function(params, premium ) {
  
  if (is.null(premium)) prem = params$plan_premium else prem = premium
  
  tmp_out <- 
    data.frame(fpl = params$pop_fpl, 
               type = params$plan_type, 
               prem = prem) %>% 
    mutate(estimate = params$takeup_coef['intercept'] + params$takeup_coef['wtp'] * prem + params$takeup_coef['i_wtp_2'] * prem^2 + params$takeup_coef['i_wtp_3'] * prem ^3) %>% 
    pull(estimate) %>% unname()
  
  return(pmax(0,pmin(1,tmp_out)))
  
}

get_cost <- function(params, premium ) {
  
  if (is.null(premium)) prem = params$plan_premium else prem = premium
  
  tmp_out <- 
    data.frame(fpl = params$pop_fpl, 
               type = params$plan_type, 
               prem = prem) %>% 
    mutate(estimate = params$cost_coef['intercept'] + params$cost_coef['wtp'] * prem + params$cost_coef['i_wtp_2'] * prem^2 + params$cost_coef['i_wtp_3'] * prem ^3) %>% 
    pull(estimate) %>% unname()
  
  return(pmax(0,tmp_out))
  
}

fn_uncomp <- function(cost, uninsured_oop_share , phi ) {
  # x is the share of the uninsured’s total health care costs that they pay out of pocket 
  # φ denotes the percentage increase in costs that result from insurance coverage (moral hazard)
  (1 - uninsured_oop_share) * (cost / (1 + phi))
}




TwoWaySA<-function(indata,outcome="NHB",parm1,parm2,range1,range2,lambda){
  
  # Get Outcome
  lhs <- indata %>% select(psa_id,contains("dQALY"),contains("dCOST")) %>%
    mutate(psa_id=row_number()) %>% 
    reshape2::melt(id.vars='psa_id') %>%
    tidyr::separate(variable,c("outcome","strategy"),"_") %>%
    reshape2::dcast(psa_id+strategy~outcome) %>%
    mutate(NHB = dQALY-dCOST * lambda ,
           NMB = dQALY*lambda - dCOST)
  
  
  # Get Parameters
  rhs <- indata %>% select(-contains("dQALY"),-contains("dCOST"),
                           -contains("NMB"),-contains("NHB"),-psa_id)
  
  # Map to existing code inputs
  Strategies <- unique(lhs$strategy)
  Parms <- rhs %>% tbl_df()  %>% data.frame() 
  cat(outcome)
  lhs$Y <- lhs[,outcome]
  Outcomes <- lhs %>% select(strategy,psa_id,Y) %>%
    reshape2::dcast(psa_id~strategy,value.var="Y") %>%
    select(-psa_id)
  
  
  #Extract parameter column number in Parms matrix
  x1<-which(colnames(Parms)==parm1)
  x2<-which(colnames(Parms)==parm2)
  dep<-length(Strategies) #Number of dependent variables, i.e., strategies
  indep<-ncol(Parms) #Number of independent variables, i.e., parameters
  
  Sim <- data.frame(Outcomes,Parms)
  
  if (ncol(Parms)==2) {
    Parms$constant = 1
    Sim$constant = 1
  }
  
  #Determine range of of the parameer to be plotted
  if (!missing("range1")&!missing("range2")){ #If user defines a range
    vector1<-seq(from=range1[1],to=range1[2],length.out=301)
    vector2<-seq(from=range2[1],to=range2[2],length.out=301)
  } else if (!missing("range1")&missing("range2")){ #Default range given by the domanin of the parameter's sample
    #vector to define 400 samples between the 2.5th and 97.5th percentiles
    vector1<-seq(from=range1[1],to=range1[2],length.out=301)
    y2 = seq(2.5,97.5,length.out=301)
    j2 = round(y2*(length(Parms[,x2])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    vector2<-sort(Parms[j2,x2])   
  } else if (missing("range1")&!missing("range2")){ #Default range given by the domanin of the parameter's sample
    #vector to define 400 samples between the 2.5th and 97.5th percentiles
    vector2<-seq(from=range2[1],to=range2[2],length.out=301)
    y1 = seq(2.5,97.5,length.out=301)
    j1 = round(y1*(length(Parms[,x1])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    vector1<-sort(Parms[j1,x1])   
  } else{
    y1 = seq(2.5,97.5,length.out=301)
    y2 = seq(2.5,97.5,length.out=301)
    j1 = round(y1*(length(Parms[,x1])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    j2 = round(y2*(length(Parms[,x2])/100))
    vector1<-sort(Parms[j1,x1])
    vector2<-sort(Parms[j2,x2])
  }
  #Generate a formula by pasting column names for both dependent and independent variables
  f <- as.formula(paste('cbind(',paste(colnames(Sim)[1:dep],collapse=','),
                        ') ~ (','poly(',parm1,',8)+','poly(',parm2,',8)+' ,
                        paste(colnames(Parms)[c(-x1,-x2)], collapse='+'),')'))
  #Run Multiple Multivariate Regression (MMR) Metamodel
  Tway.mlm = lm(f,data=Sim)
  
  TWSA <- expand.grid(parm1=vector1,parm2=vector2)
  
  #Generate matrix to use for prediction
  Sim.fit<-matrix(rep(colMeans(Parms)),nrow=nrow(TWSA),ncol=ncol(Parms), byrow=T)
  Sim.fit[,x1]<-TWSA[,1]
  Sim.fit[,x2]<-TWSA[,2]
  Sim.fit<-data.frame(Sim.fit) #Transform to data frame, the format required for predict
  colnames(Sim.fit)<-colnames(Parms) #Name data frame's columns with parameters' names
  
  #Predict Outcomes using MMMR Metamodel fit
  Sim.TW = data.frame(predict(Tway.mlm, newdata = Sim.fit))
  #Find optimal strategy in terms of maximum Outcome
  Optimal <- max.col(Sim.TW)
  #Get Outcome of Optimal strategy
  OptimalOut<-apply(Sim.TW,1,max)
  
  plotdata = Sim.fit #Append parameter's dataframe to predicted outcomes dataframe
  
  #A simple trick to define my variables in my functions environment
  plotdata$parm1<-plotdata[,parm1];
  plotdata$parm2<-plotdata[,parm2];
  
  plotdata$Strategy<-factor(Optimal,labels=Strategies[as.numeric(names(table(Optimal)))])
  plotdata$value<-OptimalOut
  
  txtsize<-12
  p <- ggplot(plotdata, aes(x=parm1,y=parm2))+
    geom_tile(aes(fill=Strategy)) +
    theme_bw() +
    #ggtitle(expression(atop("Two-way sensitivity analysis",
    #                        atop("Net Health Benefit")))) +
    scale_fill_discrete("Strategy: ", l=50)+
    xlab(parm1)+
    ylab(parm2)+
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))+
    scale_fill_grey(start = 0, end = 1)
  return(p)
}


OneWaySA<-function(indata,outcome="NHB",lambda,parm,range){
  
  # Get Outcome
  lhs <- indata %>% select(psa_id,contains("dQALY"),contains("dCOST")) %>% 
    mutate(psa_id = row_number()) %>% 
    reshape2::melt(id.vars='psa_id') %>% 
    tidyr::separate(variable,c("outcome","strategy"),"_") %>% 
    reshape2::dcast(psa_id+strategy~outcome) %>% 
    mutate(NHB = dQALY-dCOST * lambda , 
           NMB = dQALY*lambda - dCOST) 
  
  # Get Parameters
  rhs <- indata %>% select(-contains("dQALY"),-contains("dCOST"),
                           -contains("NMB"),-contains("NHB"),-psa_id)
  
  # Map to existing code inputs
  Strategies <- unique(lhs$strategy)
  Parms <- rhs %>% tbl_df() %>% data.frame()
  lhs$Y <- lhs[,outcome]
  Outcomes <- lhs %>% select(strategy,psa_id,Y) %>% 
    reshape2::dcast(psa_id~strategy,value.var="Y") %>% 
    select(-psa_id)
  
  
  #Extract parameter column number in Parms matrix
  x<-which(colnames(Parms)==parm)
  dep<-length(Strategies) #Number of dependent variables, i.e., strategies outcomes
  indep<-ncol(Parms) #Number of independent variables, i.e., parameters
  Sim <- data.frame(Outcomes,Parms)
  #Determine range of of the parameer to be plotted
  if (!missing("range")){ #If user defines a range
    vector<-seq(range[1],range[2],length.out=400)
  }
  else{ #Default range given by the domanin of the parameter's sample
    #vector to define 400 samples between the 2.5th and 97.5th percentiles
    y = seq(2.5,97.5,length=400) 
    j = round(y*(length(Parms[,x])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    vector<-sort(as.data.frame(Parms)[j,x])    
  }
  
  #Generate a formula by pasting column names for both dependent and independent variables. Imposes a 1 level interaction
  f <- as.formula(paste('cbind(',paste(colnames(Sim)[1:dep],collapse=','), ') ~ (','poly(',parm,',2)+' ,paste(colnames(Parms)[-x], collapse='+'),')'))
  #Run Multiple Multivariate Regression (MMR) Metamodel
  Oway.mlm = lm(f,data=Sim)
  
  #Generate matrix to use for prediction 
  Sim.fit<-matrix(rep(colMeans(Parms)),nrow=length(vector),ncol=ncol(Parms), byrow=T)
  Sim.fit[,x]<-vector
  Sim.fit<-data.frame(Sim.fit) #Transform to data frame, the format required for predict
  colnames(Sim.fit)<-colnames(Parms) #Name data frame's columns with parameters' names
  
  #Predict Outcomes using MMMR Metamodel fit
  plotdata = data.frame(predict(Oway.mlm, newdata = Sim.fit))
  colnames(plotdata) <- Strategies #Name the predicted outcomes columns with strategies names
  
  #Reshape dataframe for ggplot
  plotdata = stack(plotdata, select=Strategies) #
  plotdata = cbind(Sim.fit, plotdata) #Append parameter's dataframe to predicted outcomes dataframe
  
  #A simple trick to define my variables in my functions environment
  plotdata$parm<-plotdata[,parm];
  library(directlabels)
  txtsize<-12 #Text size for the graphs
  ggplot(data = plotdata, aes(x = parm, y = values, lty = ind)) +
    geom_line() +
    #ggtitle("One-way sensitivity analysis \n Net Health Benefit") + 
    xlab(parm) +
    ylab("E[NHB]") +
    scale_colour_hue("Strategy", l=50) +
    #scale_x_continuous(breaks=number_ticks(6)) + #Adjust for number of ticks in x axis
    #scale_y_continuous(breaks=number_ticks(6)) +
    theme_bw() +
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))+
    geom_dl(aes(label = ind), method = list(dl.combine("last.bumpup"), cex = 0.8))
}


CEAC<-function(lambda_range,indata){
  # Get Outcome
  lhs <- indata %>% select(contains("dQALY"),contains("dCOST")) %>% 
    mutate(psa_id = row_number()) %>% 
    reshape2::melt(id.vars='psa_id') %>% 
    tidyr::separate(variable,c("outcome","strategy"),"_") %>% 
    reshape2::dcast(psa_id+strategy~outcome) 
  
  # Get Parameters
  rhs <- indata %>% select(-contains("dQALY"),-contains("dCOST"),-psa_id) 
  
  # Map to existing code inputs
  Strategies <- unique(lhs$strategy)
  Parms <- rhs %>% tbl_df()
  Outcomes <- lhs %>% select(strategy,psa_id,contains("dCOST"),contains("dQALY")) 
  
  # Outcomes must be ordered in a way that for each strategy the cost must appear first then the effectiveness
  lambda<- lambda_range
  
  NHB <- array(0, dim=c(dim(Outcomes)[1],length(Strategies))) # Matrix to store NHB for each strategy
  colnames(NHB)<-Strategies
  CEA<-array(0,dim=c(length(lambda),length(Strategies)))
  
  #
  NHB <- lambda %>% purrr::map(~(Outcomes$dQALY-Outcomes$dCOST * .x)) %>% do.call("cbind",.)  
  colnames(NHB) <- paste0("lambda_",lambda)
  NHB <- data.frame(NHB)
  NHB$strategy <- Outcomes$strategy
  NHB$psa_id <- Outcomes$psa_id 
  NHB2 <- NHB %>% reshape2::melt(id.vars=c("strategy","psa_id"))  
  NHB2 <- NHB2 %>% split(NHB2$variable) 
  foo <- NHB2 %>% map2(.,names(.),~select(.x,-variable)) %>% 
    map2(.,names(.),~mutate(.x,lambda=as.numeric(gsub("lambda_","",.y)))) %>% 
    map2(.,names(.),~mutate(.x,NHB="NHB"))  %>% 
    map2(.,names(.),~reshape2::dcast(.x,psa_id~NHB+strategy)) 
  
  Optimal <- CEA <- list()
  for (i in names(foo)) {
    max.temp <- foo[[i]][,-1] %>% apply(.,1,max)
    Optimal[[i]] <- foo[[i]][,-1] %>% tbl_df() %>% mutate_all(funs(as.integer(.==max.temp)))
    CEA[[i]] <- colMeans(Optimal[[i]])
  }
  CEA <- do.call("rbind",CEA) %>% tbl_df() %>% mutate(lambda=as.numeric(gsub("lambda_","",names(foo))))
  colnames(CEA)<- gsub("NHB_","",colnames(CEA))
  
  CEAC<-reshape2::melt(CEA, id.vars = "lambda") 
  library(directlabels)
  txtsize<-12
  
  CEAC <- CEAC %>% mutate(variable = paste0("  ",variable,"  "))
  
  p <- ggplot(data = CEAC, aes(x = lambda, y = value, color = variable)) +
    geom_point() +
    geom_line() +
    #ggtitle("Cost-Effectiveness Acceptability Curves") + 
    scale_colour_hue("Strategies: ",l=50) +
    #scale_x_continuous(breaks=number_ticks(6))+
    xlab(expression("Policy Adoption Threshold "(lambda))) +
    ylab("Pr Cost-Effective") +
    theme_bw() +
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))+scale_colour_grey(start = .5, end = 1)+
    geom_dl(aes(label = variable), method = list(dl.combine( "last.points"), cex = 0.8))
  return(p)
}


TornadoDiag <- function(indata,outcome,lambda) { 
  # Get Outcome
  lhs <- indata %>% select(psa_id,contains("dQALY"),contains("dCOST")) %>% 
    mutate(psa_id = row_number()) %>% 
    reshape2::melt(id.vars='psa_id') %>% 
    tidyr::separate(variable,c("outcome","strategy"),"_") %>% 
    reshape2::dcast(psa_id+strategy~outcome) %>% 
    mutate(NHB = dQALY-dCOST * lambda , 
           NMB = dQALY*lambda - dCOST) 
  
  # Get Parameters
  rhs <- indata %>% select(-contains("dQALY"),-contains("dCOST"),
                           -contains("NMB"),-contains("NHB"),-psa_id)
  
  # Map to existing code inputs
  Strategies <- unique(lhs$strategy)
  Parms <- rhs %>% tbl_df()
  lhs$Y <- lhs[,outcome]
  Outcomes <- lhs %>% select(strategy,psa_id,Y) %>% 
    reshape2::dcast(psa_id~strategy,value.var="Y") %>% 
    select(-psa_id)
  
  # Find the Optimal 
  opt<-which.max(colMeans(Outcomes)); opt
  
  # calculate min and max vectors of the parameters (e.g., lower 2.5% and 97.5%)
  X <- as.matrix(Parms)
  y <- as.matrix(Outcomes[,opt])
  Y <- as.matrix(Outcomes)
  
  ymean <- mean(y)
  n <- nrow(Parms)
  nParams <- ncol(Parms)
  
  paramNames <- colnames(Parms)
  
  Parms.sorted <- apply(Parms,2,sort,decreasing=F)  #Sort in increasing order each column of Parms
  lb <- 2.5
  ub <- 97.5 
  Xmean <- rep(1,nParams) %*% t(colMeans(X))
  XMin <- Xmean
  XMax <- Xmean
  paramMin <- as.vector(Parms.sorted[round(lb*n/100),])
  paramMax <- as.vector(Parms.sorted[round(ub*n/100),])
  
  diag(XMin) <- paramMin
  diag(XMax) <- paramMax
  
  XMin <- cbind(1, XMin)
  XMax <- cbind(1, XMax)
  
  X <- cbind(1,X)
  B <- solve(t(X) %*% X) %*% t(X) %*% y # Regression for optimal strategy
  
  library(matrixStats)
  
  bigBeta <- solve(t(X) %*% X) %*% t(X) %*% Y # Regression for all strategies
  
  yMin <- rowMaxs(XMin %*% bigBeta - ymean)
  yMax <- rowMaxs(XMax %*% bigBeta - ymean)
  ySize <- abs(yMax - yMin) 
  
  rankY<- order(ySize)
  
  xmin <- min(c(yMin, yMax)) + ymean
  xmax <- max(c(yMin, yMax)) + ymean
  
  paramNames2 <- paste(paramNames, "[", round(paramMin,2), ",", round(paramMax,2), "]")
  
  strategyNames<-Strategies
  colfunc <- colorRampPalette(c("black", "white"))
  strategyColors <- colfunc(length(Strategies))
  
  ## Polygon graphs:
  nRect <- 0
  x1Rect <- NULL
  x2Rect <- NULL
  ylevel <- NULL
  colRect <- NULL
  
  for (p in 1:nParams){
    xMean <- colMeans(X)
    xStart = paramMin[rankY[p]]
    xEnd = paramMax[rankY[p]]
    xStep = (xEnd-xStart)/1000
    for (x in seq(xStart,xEnd, by = xStep)){
      #for each point determine which one is the optimal strategy
      xMean[rankY[p] + 1] <- x    # +1 moves beyond the constant
      yOutcomes <- xMean %*% bigBeta
      yOptOutcomes <- max(yOutcomes)
      yOpt <- strategyNames[which.max(yOutcomes)]
      if (x == xStart){
        yOptOld <- strategyNames[which.max(yOutcomes)]
        y1 <- yOptOutcomes
      }
      #if yOpt changes, then plot a rectangle for that region
      if (yOpt != yOptOld | x == xEnd){
        nRect <- nRect + 1
        x1Rect[nRect] <- y1
        x2Rect[nRect] <- yOptOutcomes
        ylevel[nRect] <- p
        colRect[nRect] <- yOptOld
        yOptOld <- yOpt
        y1 <- yOptOutcomes
      }
    }
  }
  
  txtsize <-8
  d=data.frame(x1=x2Rect, x2=x1Rect, y1=ylevel-0.4, y2=ylevel+0.4, t=colRect, r = ylevel)
  
  p <- ggplot(d, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = t)) +
    xlab(paste0("Expected ",outcome)) +
    ylab("Parameters") + 
    geom_rect()+
    theme_bw() + 
    scale_y_continuous(limits = c(0.5, nParams + 0.5),breaks=seq(1:ncol(Parms)), labels=paramNames2[rankY]) +
    scale_fill_grey(start = 0, end = .9)+
    geom_vline(xintercept=ymean, linetype="dotted") + 
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=1),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))+ labs(fill="")
  return(p)
}


predict.ga <- function(object, n, n0, verbose = T){
  #### Function to compute the preposterior for each of the 
  #### basis functions of the GAM model.
  #### Inputs:
  #### - object: gam object
  #### - n: scalar or vector of new sample size to compute evsi on
  #### - n0: scalar or vector of effective prior sample size
  #### - verbose: Prints the variance reduction factor for each parameter
  
  ### Name of parameters
  names.data <- colnames(object$model)
  ### Create dataframe with parameter values
  data <- data.frame(object$model[,-1])
  ## Name columns of dataframe 
  colnames(data) <- names.data[-1]
  
  ### Number of parameters
  n.params <- ncol(data)
  
  ### Sanity checks
  if(!(length(n)==1 | length(n)==n.params)){
    stop("Variable 'n' should be either a scalar or a vector 
         the same size as the number of parameters")
  }
  if(!(length(n0)==1 | length(n0)==n.params)){
    stop("Variable 'n0' should be either a scalar or a vector 
         the same size as the number of parameters")
  }
  
  ### Make n & n0 consistent with the number of parameters
  if(length(n) == 1){
    n <- rep(n, n.params)
  }
  if(length(n0) == 1){
    n0 <- rep(n0, n.params)
  }
  
  ### Compute variance reduction factor
  v.ga <- sqrt(n/(n+n0))
  if (verbose){
    print(paste("Variance reduction factor =", round(v.ga, 3)))
  }
  
  ### Number of smoothers
  n.smooth <- length(object$smooth)
  ### Number of total basis functions
  n.colX <- length(object$coefficients)
  ### Number of observations 
  n.rowX <- nrow(object$model)
  
  ### Initialize matrix for preposterior of total basis functions
  X <- matrix(NA, n.rowX, n.colX)
  X[, 1] <- 1
  
  for (k in 1:n.smooth) { # k <- 1
    klab <- substr(object$smooth[[k]]$label, 1, 1)
    if (klab == "s"){
      Xfrag <- Predict.smooth.ga(object$smooth[[k]], data, v.ga[k])
    } else {
      Xfrag <- Predict.matrix.tensor.smooth.ga(object$smooth[[k]], data, v.ga)
    }
    X[, object$smooth[[k]]$first.para:object$smooth[[k]]$last.para] <- Xfrag
  }
  
  ### Coefficients of GAM model
  Beta <- coef(object)
  
  ### Compute conditional Loss
  Ltilde <- X %*% Beta
  
  return(Ltilde)
}

Predict.smooth.ga <- function (object, data, v.ga = 1) {
  #### Function to compute the preposterior for each of the 
  #### basis functions of a smooth for one parameter
  
  ### Produce basis functions for one parameter
  X <- PredictMat(object, data) # ‘mgcv’ version 1.8-17
  ## Number of observations
  n.obs <- nrow(X)
  
  ### Apply variance reduction to compute the preposterior 
  ### for each of the basis functions
  ## Vector of ones
  ones <- matrix(1, n.obs, 1)
  ## Compute phi on each of the basis function
  X.ga <- v.ga*X + (1-v.ga)*(ones %*% colMeans(X))
  
  return(X.ga)
}

Predict.matrix.tensor.smooth.ga <- function (object, 
                                             data, 
                                             v.ga = rep(1, ncol(data))){
  #### Function to compute the preposterior for each of the 
  #### basis functions for one or more parameters and calculates
  #### the tensor product if more than one parameter is selected
  #### (Heavily based on function Predict.matrix.tensor.smooth from
  #### mgcv package)
  
  m <- length(object$margin)
  X <- list()
  for (i in 1:m) { # i <- 1
    term <- object$margin[[i]]$term
    dat <- list()
    for (j in 1:length(term)) { # j <- 1
      dat[[term[j]]] <- data[[term[j]]]
    }
    X[[i]] <- if (!is.null(object$mc[i])) # before: object$mc[i]
      PredictMat(object$margin[[i]], dat, n = length(dat[[1]])) # ‘mgcv’ version 1.8-17
    else Predict.matrix(object$margin[[i]], dat)
    n.obs <- nrow(X[[i]])
  } # end for 'i'
  mxp <- length(object$XP)
  if (mxp > 0) 
    for (i in 1:mxp) if (!is.null(object$XP[[i]])) 
      X[[i]] <- X[[i]] %*% object$XP[[i]]
  
  ### Apply variance reduction to compute the preposterior 
  ### for each of the basis functions
  ## Vector of ones
  ones <- matrix(1, n.obs, 1)
  ## Initialize and fill list with preposterior of basis functions 
  ## for each parameter
  X.ga <- list()
  for (i in 1:m) { # i <- 1
    X.ga[[i]] <- v.ga[i]*X[[i]] + (1-v.ga[i])*(ones %*% colMeans(X[[i]]))
  }
  
  ### Compute tensor product
  T.ga <- tensor.prod.model.matrix(X.ga) # ‘mgcv’ version 1.8-17
  
  return(T.ga)
}


## For Simulating Medicaid



cov_sim <- function(params) {
  
  p <- params[grep("^p_",names(params))] %>% unlist()
  
  R <-  params[grep("^R_",names(params))] %>% unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "type") %>% 
    separate(type,into= c("exa","exp"), sep ="_TO_") %>% 
    set_names(c("exa","exp","value")) %>% 
    spread(exp,value) %>% 
    select(-exa) %>% 
    as.matrix()
  
  DD <-  params[grep("^DD_",names(params))] %>% unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "type") %>% 
    separate(type,into= c("exa","exp"), sep ="_TO_") %>% 
    set_names(c("exa","exp","value")) %>% 
    spread(exp,value) %>% 
    select(-exa) %>% 
    as.matrix()
  
  baseline <- t(p) %*% R
  expmedicaid <- t(p) %*% (R + DD) 
  
  mvpf_med <- calculate_wtp_public(params)
  mvpf_subsidy <- simulate_subsidy(params)
  
  R_subsidy <- R
  R_subsidy[4,2] <- R_subsidy[4,4] * params$frac_uninsured_elig *  mvpf_subsidy$takeup
  R_subsidy[4,4] <- 1 - sum(R_subsidy[4,1:3])
  
  subsidy <- t(p) %*% R_subsidy
  # Now need to include estimation of MVPF cost and benefits.
  
  out <- 
    list(baseline = baseline, med = expmedicaid, subsidy = subsidy) %>% 
    bind_rows() %>% 
    tbl_df() %>% 
    mutate(type = insurance_sipp_lut) %>% 
    select(type,baseline,med, subsidy) %>% 
    gather(key,value,-type) %>% 
    mutate(iteration = 1) %>% 
    unite("tmp",key,type) %>% 
    spread(tmp,value) %>% 
    bind_cols(mvpf_med %>% data.frame()) %>% 
    rename(med_mvpf = mvpf, 
           med_mvpf_num = mvpf_num,
           med_mvpf_denom = mvpf_denom,
           med_wtp = wtp,
           med_cost = cost,
           med_N = N) %>% 
    bind_cols(mvpf_subsidy %>% data.frame()) %>% 
    rename(subsidy_takeup = takeup,
           subsidy_C_H = C_H, 
           subsidy_uncomp = uncomp, 
           subsidy_mvpf_num = mvpf_num,
           subsidy_mvpf_denom = mvpf_denom,
           subsidy_mvpf = mvpf)
  
  
  return(out)
}

calculate_wtp_public <- function(params, scaling_factor = 1) {
  
  #The net cost of Medicaid equals the average increase in medical spending due to Medicaid
  # plus the average decrease in out-of-pocket spenign due to Medicaid (see equation 22). 
  p_1 <- params$OOP_Tx / params$G
  
  p_0 <- params$OOP_Cx / params$G_Cx
  
  MCD_SPEND = params$G - params$G_Cx
  
  C  = MCD_SPEND + params$OOP_Cx
  
  # The monetary transfer from Medicaid to external parties, N, is the difference between G and C.
  N <- params$G - C
  
  welfare_weight <- params$v_i / params$v_j
  
  # We estimate the transfer component and pure-insurance component separately, and combine them
  # for our estimate of \gamma(1).
  
  # Transfer component (p.29)
  # using a linear approximation and the estimates of E[m(0,\theta)] and E[m(1,\theta)]
  Tr <- (p_0-p_1)*(0.5 * (params$G_Cx +  params$G))
  
  net_cost_as_frac_gross <- C / params$G  
  moral_hazard_cost <- params$G - Tr - N
  
  # wtp <- Tr + params$I 
  # 
  # mvpf_gov <- wtp / C
  # mvpf_indiv <- (wtp + params$G * welfare_weight * (N / params$G)) / params$G
  # 
  # mvpf_num_gov <- wtp 
  # mvpf_denom_gov <- C
  # mvpf_num_indiv <- (wtp + params$G * welfare_weight * (N / params$G))
  # mvpf_denom_indiv <- params$G
  # 
  # mvpf_num <- params$gov_incidence * mvpf_num_gov + (1 - params$gov_incidence) * mvpf_num_indiv 
  # mvpf_denom <- params$gov_incidence * mvpf_denom_gov  + (1 - params$gov_incidence) * mvpf_denom_indiv
  # 
  # mvpf <- mvpf_num / mvpf_denom
  #out <- list(mvpf = mvpf  , mvpf_num = (mvpf_num / 12)/100 , mvpf_denom = (mvpf_denom / 12)/100, wtp = wtp , cost = C  , N = N)
  
  ######################################################
  # Scale all relevant values by government cost of 
  # Medicaid so it can be measured in terms of a single 
  # dollar spent on Medicaid. 
  ######################################################
  scaling_factor = C
  
  wtp <- Tr + params$I + params$fudge
  
  mvpf_gov <- (wtp/scaling_factor ) / (C/scaling_factor )
  mvpf_indiv <- (wtp/scaling_factor  + params$G/scaling_factor  * welfare_weight * (N / params$G)) / (params$G/scaling_factor )
  
  mvpf_num_gov <- wtp / scaling_factor 
  mvpf_denom_gov <- C /scaling_factor 
  mvpf_num_indiv <- (wtp/scaling_factor  + params$G/scaling_factor  * welfare_weight * (N / params$G))
  mvpf_denom_indiv <- params$G/scaling_factor 
  
  mvpf_num <- params$gov_incidence * mvpf_num_gov + (1 - params$gov_incidence) * mvpf_num_indiv 
  mvpf_denom <- params$gov_incidence * mvpf_denom_gov  + (1 - params$gov_incidence) * mvpf_denom_indiv
  
  mvpf <- mvpf_num / mvpf_denom
  
  out <- list(mvpf = mvpf  , mvpf_num = mvpf_num  , mvpf_denom = mvpf_denom, wtp = wtp , cost = C  , N = N)
  return(out)
}


simulate_subsidy <- function(params) {
  takeup <- get_takeup(params, premium = params$plan_premium)
  # takeup_deriv = 1/(get_takeup(params, premium = params$plan_premium+1) - get_takeup(params, premium =  params$plan_premium))
  # cost_reformed <- get_cost(params, premium = params$plan_premium)
  # uncomp = fn_uncomp(cost = cost_reformed, uninsured_oop_share = params$uninsured_oop_share, phi = params$phi)
  # 
  # mvpf_num = takeup + 
  #   params$eta * 
  #   (
  #     pmax(0,uncomp) / 
  #       (-1 * takeup_deriv)
  #   ) 
  # mvpf_denom = takeup + 
  #   ((pmax(0,cost_reformed - params$gov_incidence * uncomp - params$plan_premium)) / 
  #      (-1 * takeup_deriv)) 
  # mvpf = mvpf_num / mvpf_denom
  # 
  
  
  # Alternative Version
  s_star <- get_takeup(params, premium = params$plan_premium)
  ds_dpH <- get_takeup(params, premium = params$plan_premium)-get_takeup(params, premium = params$plan_premium+1)
  C_H <- get_cost(params, premium = params$plan_premium)
  p_H <- params$plan_premium
  uncomp = fn_uncomp(cost = C_H, uninsured_oop_share = params$uninsured_oop_share, phi = params$phi)
  
  welfare_weight <- params$v_i / params$v_j
  
  mvpf_num <-  s_star + welfare_weight * uncomp * ds_dpH
  
  cost_of_new_enrollees <- ds_dpH * (C_H - params$gov_incidence * uncomp - p_H) 
  mvpf_denom <- s_star  + cost_of_new_enrollees
  
  mvpf<- mvpf_num / mvpf_denom
  
  
  output <- list(
    takeup = takeup, 
    # takeup_deriv = takeup_deriv, 
    C_H = C_H,
    uncomp = uncomp, 
    mvpf_num = mvpf_num,
    mvpf_denom = mvpf_denom, 
    mvpf = mvpf
  )
  return(output)
}




