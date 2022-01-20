aMNLFA.scores<-function(input.object){
  
  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mycatindicators = input.object$catindicators
  mycountindicators = input.object$countindicators
  myMeanImpact = input.object$meanimpact
  myVarImpact = input.object$varimpact
  myMeasInvar = input.object$measinvar
  mytime = input.object$time
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  thresholds = input.object$thresholds
  
  
  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist<-unique(varlist)
  
  header<-readLines(file.path(dir,"header.txt"))
  header2<-readLines(file.path(dir,"header2.txt"))
  
  USEVARIABLES<-paste("USEVARIABLES=")
  semicolon<-paste(";")
  AUXILIARY<-paste("AUXILIARY=")
  AUXILIARY<-append(AUXILIARY,myauxiliary)
  AUXILIARY<-noquote(append(AUXILIARY,semicolon))
  AUXILIARY<-utils::capture.output(cat(AUXILIARY))
  CATEGORICAL<-paste("CATEGORICAL=")
  CATEGORICAL<-append(CATEGORICAL,mycatindicators)
  CATEGORICAL<-noquote(append(CATEGORICAL,semicolon))
  CATEGORICAL<-utils::capture.output(cat(CATEGORICAL))
  COUNT<-paste("COUNT=")
  COUNT<-append(COUNT,mycountindicators)
  COUNT<-noquote(append(COUNT,semicolon))
  COUNT<-utils::capture.output(cat(COUNT))
  ANALYSIS<-noquote("ANALYSIS: ESTIMATOR=ML;ALGORITHM=INTEGRATION;INTEGRATION=MONTECARLO;PROCESSORS=4;")
  
  ETA<-paste("ETA BY ")
  l<-length(myindicators)
  loadings<-list()
  for (i in 1:l){
    loadings[i]<-paste(ETA,myindicators[i],"*(l",i,");",sep="")
  }
  loadings<-noquote(loadings)
  loadings<-unlist(loadings)
  tech1<-paste("OUTPUT: tech1;")
  MODEL<-paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT<-paste("CONSTRAINT=")
  varMODEL<-paste("MODEL: [ETA@0];ETA*(veta);")
  fixvarMODEL<-paste("MODEL: [ETA@0];ETA@1;")
  MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(")
  
  
  #############################################################################
  ###Read in final calibration model to obtain final parameter estimates#######
  ##############Output scoring model input#####################################
  #############################################################################
  
  # Remove excess output else creates error
  round3output_v0 <- read.delim(file.path(dir,"round3calibration.out",sep=""), blank.lines.skip=FALSE)
  start_delete_index <- which(grepl("RESULTS IN PROBABILITY SCALE", unlist(round3output_v0)))
  end_delete_index <- which(grepl("QUALITY OF NUMERICAL RESULTS", unlist(round3output_v0)))
  if(length(start_delete_index)>0&length(end_delete_index)>0){
    round3output_edit <- data.frame(round3output_v0[-(start_delete_index:(end_delete_index-1)),])
    colnames(round3output_edit) <- colnames(round3output_v0)
    write.table(round3output_edit,file.path(dir,"round3calibration.out",sep=""),append=F,row.names=FALSE,col.names=TRUE,quote=FALSE)
  }else{
    message("RESULTS IN PROBABILITY SCALE or QUALITY OF NUMERICAL RESULTS not found, continuing as normal")
  }
  
  round3output<-MplusAutomation::readModels(file.path(dir,"round3calibration.out",sep=""))
  round3input<-round3output$input$model.constraint
  
  keepvarimpact<-list()
  for (i in 1:length(round3input)){
    if (length(grep("+",round3input[i],value=TRUE,fixed=TRUE))>0) keepvarimpact<-append(keepvarimpact,round3input[i])
  }
  
  foo<-strsplit(as.character(keepvarimpact),"+",fixed=TRUE)
  foo<-strsplit(as.character(unlist(foo)),"*",fixed=TRUE)
  foo<-unlist(foo)
  foo<-foo[!is.na(match(foo,myVarImpact))]
  keepvarimpact<-utils::capture.output(cat(foo))
  round3output<-as.data.frame(round3output$parameters$unstandardized)
  
  ETAPREDLIST<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  
  
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  lambdaconstraints<-constraints[which(substr(constraints$param,1,1)=="L"),]
  lambdaconstraints$itemnum<-substr(lambdaconstraints$param,2,2)
  lambdaconstraints$item<-myindicators[as.numeric(lambdaconstraints$itemnum)]
  lambdaconstraints$predictornum<-substr(lambdaconstraints$param,3,4)
  lambdaconstraints$predictor<-ifelse(lambdaconstraints$predictornum=="00","intercept",myMeasInvar[as.numeric(substr(lambdaconstraints$param,3,3))])
  
  lambdainvlist<-unique(unlist(lambdaconstraints$item))
  
  lambda<-round3output[which(grep(".BY",round3output$paramHeader)>0),]
  
  round4<-unlist(round3output$param)
  keepround4<-myindicators
  for (i in 1:length(round4)){
    if (length(grep(round4[i],myMeasInvar))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myMeanImpact))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myVarImpact))>0) keepround4<-append(keepround4,round4[i])
  }
  keepround4<-unique(keepround4)
  keepround4<-keepround4[!is.na(keepround4)]
  keepround4<-utils::capture.output(cat(keepround4))
  
  lc<-lambdaconstraints$predictor[which(lambdaconstraints$predictor!="intercept")]
  lc<-unique(lc)
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  varimpact<-constraints[which(substr(constraints$param,1,1)=="V"),]
  varimpact<-substr(varimpact$param,2,3)
  keepvarimpact<-myVarImpact[as.numeric(varimpact)]
  con<-unique(append(keepvarimpact,lc))
  CONSTRAINT<-noquote(append(CONSTRAINT,con))
  CONSTRAINT<-append(CONSTRAINT,semicolon)
  CONSTRAINT<-utils::capture.output(cat(CONSTRAINT))
  CONSTRAINT<-ifelse(length(con)>0,CONSTRAINT,"!")
  
  meanimpact<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  keepmeanimpact<-meanimpact$param
  ETAON3<-"!"
  if(length(keepmeanimpact)>0) ETAON3<-paste("ETA ON ",keepmeanimpact,"@",sep="")
  #writing scoring model
  scoringinput<-as.data.frame(NULL)
  
  scoringinput[1,1]<-paste("TITLE: Scoring Model")
  scoringinput[2,1]<-header2[2]
  scoringinput[3,1]<-header2[3]
  scoringinput[4,1]<-header2[4]
  scoringinput[5,1]<-header2[5]
  scoringinput[6,1]<-ifelse(length(header2)>5,header[6],"!")
  scoringinput[7,1]<-ifelse(length(header2)>6,header[7],"!")
  scoringinput[8,1]<-ifelse(length(header2)>7,header[8],"!")
  scoringinput[9,1]<-ifelse(length(header2)>8,header[9],"!")
  scoringinput[10,1]<-paste("USEVARIABLES= ",keepround4,semicolon,sep="")
  clus<-ifelse(is.null(mytime),"!",paste("CLUSTER=",myID,semicolon,sep=""))
  if(is.null(mytime)){
    aux<-append(myauxiliary,myID)
    aux<-unique(aux)
  }else{
    aux<-myauxiliary
  }
  aux<-ifelse(length(aux)>0,paste("AUXILIARY=",aux,semicolon,sep=""),"!")
  scoringinput[11,1]<-aux
  scoringinput[12,1]<-clus
  scoringinput[13,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  scoringinput[14,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  scoringinput[15,1]<-CONSTRAINT
  scoringinput[16,1]<-ANALYSIS
  scoringinput[17,1]<-ifelse(is.null(mytime),"!","TYPE=COMPLEX;")
  scoringinput[18,1]<-ifelse(length(keepvarimpact)>0,varMODEL,fixvarMODEL)
  
  
  l<-length(myindicators)
  scoreloadings<-list()
  for (i in 1:l){
    scoreloadings[i]<-paste(ETA,myindicators[i],sep="")
    scoreloadings[i]<-ifelse(lambda$pval[i] !=999,paste(" @",lambda$est[i],semicolon,sep=""),paste("*(l",i,")",semicolon,sep=""))
    scoreloadings[i]<-paste(ETA,myindicators[i],scoreloadings[i],sep="")
  }
  
  for (i in 1:l){
    scoringinput[18+i,1]<-scoreloadings[i]
  }
  for (i in 1:length(ETAON3)){
    scoringinput[18+l+i,1]<-paste(ETAON3[i],ETAPREDLIST$est[i],semicolon,sep="")
  }
  
  
  intdif<-round3output[grep(".ON",round3output$paramHeader),]
  intcode<-character(0)
  if (nrow(intdif)>0)
  {
    intdif$item<-utils::read.table(text = intdif$paramHeader, sep = ".", as.is = TRUE)$V1
    intdif<-intdif[which(intdif$item!="ETA"),]
    keepcols<-c("param","est","item")
    intdif<-intdif[,keepcols]
    intcode<-paste(intdif$item," ON ",intdif$param,"@",intdif$est,semicolon,sep="")
    
    for (i in 1:length(intcode)){
      scoringinput[18+l+length(ETAON3)+i,1]<-intcode[i]
    }
  }
  
  
  
  thresh<-round3output[which(round3output$paramHeader=="Thresholds"|round3output$paramHeader=="Intercepts"&round3output$param!="ETA"),]
  for (i in 1:dim(thresh)[1]){
    scoringinput[18+l+length(ETAON3)+length(intcode)+i,1]<-paste("[",thresh$param[i],"@",thresh$est[i],"];",sep="")
  }
  MODELCON<-paste("MODEL CONSTRAINT:")
  scoringinput[19+l+length(ETAON3)+length(intcode)+dim(thresh)[1],1]<-ifelse(length(con)>0,MODELCON,"!")
  varval<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"&substr(round3output$param,1,1)=="V"),]
  vstart<-data.frame(NULL)
  for (v in 1:length(keepvarimpact)){
    vstart[v,1]<-utils::capture.output(cat(paste(varval$est[v],"*",keepvarimpact[v],"+",sep="")))
  }
  vstart<-paste(utils::capture.output(cat(noquote(unlist(vstart)))),sep="")
  
  scoringinput[20+l+length(ETAON3)+length(intcode)+dim(thresh)[1],1]<-ifelse(length(keepvarimpact)>0,paste("veta=1*exp("),"!")
  scoringinput[21+l+length(ETAON3)+length(intcode)+dim(thresh)[1],1]<-ifelse(length(keepvarimpact)>0,vstart ,"!")
  scoringinput[22+l+length(ETAON3)+length(intcode)+dim(thresh)[1],1]<-ifelse(length(keepvarimpact)>0,paste("0);"),"!")
  
  #lambda constraints
  lval<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"&substr(round3output$param,1,1)=="L"),]
  #reorg so that all params for same item on same row
  lval$item<-substr(lval$param,2,3)
  lval$predictor<-as.numeric(substr(lval$param,3,4))
  lval$eq<-numeric(0)
  
  if ((dim(lval)[1])>0){
    for (i in 1:dim(lval)[1]){
      if (lval$predictor[i]==0) lval$eq[i]<-paste("l",lval$item[i],"=",lval$est[i],sep="")
      for (j in 1:length(myMeasInvar)){
        if (lval$predictor[i]==j) lval$eq[i]<-paste("+",lval$est[i],"*",myMeasInvar[j],sep="")
      }}}
  keep<-c("item","predictor","eq")
  lval<-lval[keep]
  wide<-stats::reshape(lval, idvar = "item", timevar = "predictor", direction = "wide")
  wide<-wide[,-1]
  for (i in 1:dim(wide)[1]){
    line<-wide[i,]
    line<-line[!is.na(line)]
    line<-utils::capture.output(cat(line))
    line<-append(line,semicolon)
    line<-utils::capture.output(cat(line))
    scoringinput[23+l+length(ETAON3)+length(intcode)+dim(thresh)[1]+i,1]<-ifelse(dim(lval)[1]>0,line,"!")
  }
  scoringinput[24+l+length(ETAON3)+length(intcode)+dim(thresh)[1]+length(dim(wide)[1]),1]<-tech1
  scoringinput[25+l+length(ETAON3)+length(intcode)+dim(thresh)[1]+length(dim(wide)[1]),1]<-paste("SAVEDATA: SAVE=FSCORES; FILE=scores.dat;")
  
  utils::write.table(scoringinput,paste(dir,"/scoring.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  #write.inp.file(scoringinput,fixPath(file.path(dir,"scoring.inp",sep="")))
  message("Check '", dir, "/' for Mplus inp file for scoring model (run this manually).")
}