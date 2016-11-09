

# define a generator that produces y and x
# the idea is we have 2*nLevels levels each with and a and b.
# for non-signalling variables we pick a/b versions 50/50.
# signalling/non-signalling chosen by signalProb
# for signalling variables we pick a more often if y is true (controlled
#    by signalStrength)
defineGenerator <- function(nVars,nLevels,signalProb,signalStrength) {
  maps <- lapply(seq_len(nVars),
                 function(vi) {
                   levelsBase <- paste('l',sprintf("%02d",seq_len(nLevels)),sep='_')
                   signaling <- runif(nLevels)>=(1-signalProb)
                   signal <- signalStrength*signaling
                   levels <- lapply(paste0(levelsBase,ifelse(signal,'s','n')),
                                    function(li) {
                                      list('TRUE'=paste(li,'a',sep='_'),
                                           'FALSE'=paste(li,'b',sep='_'))
                                    })
                   list(varName=paste('v',sprintf("%02d",vi),sep='_'),
                        levels=levels,
                        signaling=signaling,
                        signal=signal)
                 }
  )
  maps
}

# generaet data according to the distribution defined by defineGenerator()
# duplicate each indpendent (or input column) nDups times
# generate n rows.
generateData <- function(generator,nDups,n) {
  d <- data.frame(y=runif(n)>0.5)
  extraCols <- lapply(generator,
                      function(defi) {
                        idxs <- sample.int(length(defi$levels),n,replace=TRUE)
                        choice <- as.character((runif(n) - defi$signal[idxs]*(1-2*d$y))>0.5)
                        col <- vapply(seq_len(n),
                                      function(i) {
                                        defi$levels[[idxs[[i]]]][[choice[[i]]]]
                                      },character(1))
                        col
                      })
  varNames <- vapply(generator,
                     function(defi) {
                       defi$varName
                     },character(1))
  names(extraCols) <- varNames
  extraCols <- as.data.frame(extraCols,stringsAsFactors=FALSE)
  d <- cbind(d,extraCols)
  if(nDups>0) {
    for(di in seq_len(nDups)) {
      names(extraCols) <- paste(varNames,sprintf("%02d",di),sep='_')
      d <- cbind(d,extraCols)
    }
  }
  d
}


rlaplace <- function(n,sigma) {
  # Laplace noise is: rexp(n,rate = 1/sigma) - rexp(n,rate = 1/sigma)
  rexp(n,rate = 1/sigma) - rexp(n,rate = 1/sigma)
}


buildCountEncodingModelV <- function(varname, var, y,
                                     ...,
                                     deltaTrue=0, deltaFalse=0, 
                                     laplaceRate=0, absNoise=FALSE) {
  if(length(list(...))>0) {
    stop('unexpected arguments in buildCountEncodingModelV')
  }
  epsilon <- 1.0e-3
  nRare <- 4
  data.frame(v=var,y=y,stringsAsFactors = FALSE) %>% 
    group_by(v) %>% 
    summarize(nTrue=sum(y),nFalse=sum(!y)) -> di
  if(deltaTrue!=0) {
    di %>%
      mutate(nTrue=pmax(0,nTrue+deltaTrue)) -> di
  }
  if(deltaFalse!=0) {
    di %>%
      mutate(nFalse=pmax(0,nFalse+deltaFalse)) -> di
  }
  di %>% 
    mutate(rare=(nTrue+nFalse)<nRare) -> di
  if(laplaceRate!=0) {
    # trying both of these noise defs
    if(absNoise) {
      noiser <- function(col) {
        abs(rlaplace(length(col),laplaceRate))+col
      }
    } else {
      noiser <- function(col) {
        pmax(0,rlaplace(length(col),laplaceRate)+col)
      }
    }
    di %>%
      mutate(nFalse=noiser(nFalse),
             nTrue=noiser(nTrue)) -> di
  } 
  di %>% 
    mutate(ldiff=log((nTrue+epsilon)/(nFalse+epsilon))) %>%
    as.data.frame(stringsAsFactors=FALSE) -> di
  levels <- di$v
  di$v <- NULL
  colnames(di) <- paste(varname,colnames(di),sep='_')
  map <- split(di,levels)
  map
}

applyCountEncodingModelV <- function(cModel,var) {
  res <- cModel[var]
  missing <- vapply(res,is.null,logical(1))
  if(sum(missing)>0) {
    ex <- cModel[[1]]
    ex[,1:3] <- 0
    ex[,4] <- TRUE
    for(i in which(missing)) {
      res[[i]] <- ex
    }
  }
  dplyr::bind_rows(res)
}



buildCountEncodingModel <- function(d, varNames,
                                    ...,
                                    laplaceRate=0, absNoise=FALSE) {
  if(length(list(...))>0) {
    stop('unexpected arguments in buildCountEncodingModel')
  }
  varNames <- setdiff(varNames,'y')
  models <- lapply(varNames,
                   function(vi) {
                     buildCountEncodingModelV(varname=vi, d[[vi]], d$y,
                                             laplaceRate=laplaceRate,
                                             absNoise=absNoise)
                   })
  names(models) <- varNames
  models
}

encodeFrame <- function(models,d) {
  colList <- vector('list',length(models))
  for(ii in seq_len(length(models))) {
    vi <- names(models)[[ii]]
    mi <- models[[vi]]
    fi <- applyCountEncodingModelV(mi,d[[vi]])
    colList[[ii]] <- fi
  }
  res <- dplyr::bind_cols(colList)
  if('y' %in% colnames(d)) {
    res$y <- d$y
  }
  res
}




jacknifeFrameV <- function(varname, var, y) {
  res <- vector('list',length(y))
  wy <- which(y)
  if(length(wy)>0) {
    eTrue <- buildCountEncodingModelV(varname, var, y, deltaTrue=-1)
    fTrue <- applyCountEncodingModelV(eTrue,var[wy])
    for(i in seq_len(length(wy))) {
      res[[wy[i]]] <- fTrue[i,,drop=FALSE]
    }
  }
  wy <- which(!y)
  if(length(wy)>0) {
    eFalse <- buildCountEncodingModelV(varname, var, y, deltaFalse=-1)
    fFalse <- applyCountEncodingModelV(eFalse,var[wy])
    for(i in seq_len(length(wy))) {
      res[[wy[i]]] <- fFalse[i,,drop=FALSE]
    }
  }
  dplyr::bind_rows(res)
}


jacknifeFrame <- function(d,varNames) {
  varNames <- setdiff(varNames,'y')
  colList <- vector('list',length(varNames))
  for(ii in seq_len(length(varNames))) {
    vi <- varNames[[ii]]
    fi <- jacknifeFrameV(vi,d[[vi]],d$y)
    colList[[ii]] <- fi
  }
  res <- dplyr::bind_cols(colList)
  res$y <- d$y
  res
}

mkModel <- function(d) {
  mvars <- setdiff(colnames(d),'y')
  f <- paste('y',paste(mvars,collapse = ' + '),sep=' ~ ')
  model <- glm(f,data=d,family=binomial)
}


runIndicators <- function(dTrain,dTest) {
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  mI <- mkModel(dTrain)
  trainScore <- sigr::formatChiSqTest(mI,format='ascii')$pseudoR2
  dTestP$predI <- predict(mI,newdata = dTest,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predI','y',format='ascii')$pseudoR2
  data.frame(model='Native Indicators',
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore),
             stringsAsFactors = FALSE)
}


runNaive <- function(dTrain,dTest) {
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  cmodelN <- buildCountEncodingModel(dTrain,vars)
  dEncTrainN <- encodeFrame(cmodelN,dTrain)
  m1 <- mkModel(dEncTrainN)
  trainScore <- sigr::formatChiSqTest(m1,format='ascii')$pseudoR2
  dTestEncN <- encodeFrame(cmodelN,dTest)
  dTestP$predN <- predict(m1,newdata = dTestEncN,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predN','y')$pseudoR2
  data.frame(model='Naive Count Coding',
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore),
             stringsAsFactors = FALSE)
}


runSplit <- function(dTrain,dTest) {
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  isCal <- runif(nrow(dTrain))<=0.5
  dTrainCal <- dTrain[isCal,,drop=FALSE]
  dTrainModel <- dTrain[!isCal,,drop=FALSE]
  cmodelS <- buildCountEncodingModel(dTrainCal,vars)
  dEncS <- encodeFrame(cmodelS,dTrainModel)
  mS <- mkModel(dEncS)
  trainScore <- sigr::formatChiSqTest(mS,format='ascii')$pseudoR2
  dTestEncS <- encodeFrame(cmodelS,dTest)
  dTestP$predS <- predict(mS,newdata = dTestEncS,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predS','y')$pseudoR2
  data.frame(model='Split Count Coding',
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore),
             stringsAsFactors = FALSE)
}

runJackknifed <- function(dTrain,dTest) {
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  # jackknifed application (uses all data for test encoding)
  dJ <- jacknifeFrame(dTrain,vars)
  mJ <- mkModel(dJ)
  trainScore <- sigr::formatChiSqTest(mJ,format='ascii')$pseudoR2
  cmodelN <- buildCountEncodingModel(dTrain,vars)
  dTestEncN <- encodeFrame(cmodelN,dTest)
  dTestP$predJ <- predict(mJ,newdata = dTestEncN,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predJ','y')$pseudoR2
  data.frame(model='Jackknife Count Coding',
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore))
}

runVtreat <- function(dTrain,dTest) {
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  # default splitting not good for this problem
  cfe <- vtreat::mkCrossFrameCExperiment(dTrain,vars,'y',TRUE,
                                         #splitFunction = vtreat::oneWayHoldout,
                                         ncross=10,
                                         rareCount = 2,
                                         rareSig = 0.5,
                                         smFactor = 1,
                                         parallelCluster = cl)
  sf <- cfe$treatments$scoreFrame
  newvars <- sf$varName[(sf$sig<1/length(vars)) & (sf$code %in% c('catB','catP'))]
  mV <- mkModel(cfe$crossFrame[,c('y',newvars),drop=FALSE])
  trainScore <- sigr::formatChiSqTest(mV,format='ascii')$pseudoR2
  dTestEncV <- vtreat::prepare(cfe$treatments,dTest,
                               pruneSig=NULL,varRestriction = newvars,
                               parallelCluster = cl)
  dTestP$predV <- predict(mV,newdata = dTestEncV,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predV','y')$pseudoR2
  data.frame(model='vtreat Impact Coding',
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore),
             stringsAsFactors = FALSE)
}


runLaplaceNoised <- function(dTrain,dTest,
                             ...,
                             laplaceRate,absNoise) {
  if(length(list(...))>0) {
    stop('unexpected arguments in runLaplaceNoised')
  }
  dTestP <- data.frame(y=dTest$y)
  vars <- setdiff(colnames(dTrain),'y')
  cmodelL <- buildCountEncodingModel(dTrain,vars,
                                     laplaceRate=laplaceRate,
                                     absNoise=absNoise)
  dEncL <- encodeFrame(cmodelL,dTrain)
  mL <- mkModel(dEncL)
  trainScore <- sigr::formatChiSqTest(mL,format='ascii')$pseudoR2
  dTestEncL <- encodeFrame(cmodelL,dTest)
  dTestP$predL <- predict(mL,newdata = dTestEncL,type='response')
  testScore <- sigr::formatChiSqTest(dTestP,'predL','y')$pseudoR2
  mnam = 'Laplace Noised Count Coding'
  if(absNoise) {
    mnam <- paste(mnam,'(abs)')
  }
  data.frame(model=mnam,
             data=c('train','test'),
             pseudoR2=c(trainScore,testScore),
             stringsAsFactors = FALSE)
}


