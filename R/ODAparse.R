#' @title Parse ODA output files.
#'
#' @description Parses model details, model predictions, and loads objects.
#'
#' @param run The numerical value of the run folder in which the data file is
#'   stored
#'
#' @param ... Additional run numbers specified as a list
#'
#' @details When run, \code{ODAparse} will return model performance metrics and
#'   data loaded within the global environment, which can be further evaluated.
#'
#'   The working directory must be directed toward the project files to be
#'   loaded and located within a \code{Run} folder inside the project tree.
#'
#'   For each ODA model, \code{ODAparse} will return the \strong{Effect Strength
#'   for Sensitivity} or ESS and the \strong{Effect Strength for Predictive
#'   Value} or ESP. Mean \strong{Percent Accuracy in Classification} (PAC) and
#'   \strong{Mean Predicitve Value} (MPV) are reported, as these are a common
#'   metrics for predictive model performance.
#'
#'   In binary classification problems the following relationships are defined:
#'
#'   \deqn{PAC = \frac{(Sensitivity+Specificity)}{2} \times 100}
#'
#'   \deqn{ESS = \frac{(PAC-50)}{(100-50)} \times 100}
#'
#'   \deqn{MPV = \frac{(PPV+NPV)}{2} \times 100}
#'
#'   \deqn{ESP = \frac{(MPV-50)}{(100-50)} \times 100}
#'
#'   Unlike mean PAC and MPV, both ESS and ESP are normed against chance
#'   (Yarnold \emph{et al}. 2005 and 2016) providing an intuitive scaling of
#'   model accuracy within the ODA paradigm.
#'
#'   The significance values reported are from 1. Monte Carlo simulations for
#'   all observations and 2. Fisher's Exact tests for LOO analysis. Depending on
#'   whether a GEN, CAT, or ordered ODA model is detected, and if LOO jackknife
#'   is performed, the exact \code{oda.model} output will vary.
#'
#'   The user is referred to the ODA User guide \code{\link{ODAmanual}} and is
#'   encouraged to review the data contained within the MODEL.OUT file for more
#'   information.
#'
#' @return The following objects with the run number appended are returned for
#'   ODA models: \item{oda.data}{Data frame based on the data.csv file from
#'   specified \code{run} folder.} \item{oda.key}{Data frame containing 2
#'   columns: the variable names from the \code{oda.data} and an ODA friendly
#'   alias e.g., v1 v2 for each attribute variable included in the ODA model.}
#'   \item{oda.list}{Data frame containing the confusion matrix for each ODA
#'   model contained in the model.out file. This output can be used to evaluate
#'   the reproducibility of the model results using a Novometric bootstrap
#'   analysis, see \code{\link{NOVOboot}}.} \item{oda.model}{Data frame
#'   containing the parsed model output and the ESS, ESP, and significance for
#'   ODA models.} \item{oda.perf}{Data frame containing the overall accuracy,
#'   sensitivity, specificity, positive predictive value, and negative
#'   predictive value for each attribute included in the ODA model. Univariate
#'   OR and 95\% CI are also presented. The Haldane-Anscombe-Gart correction is
#'   made observed cell counts of zero and a warning is displayed.}
#'   \item{oda.stats}{Data frame containing the classification summary for each
#'   ODA model contained in the model.out file.} \item{oda.sda}{Data frame
#'   containing predictions based on the model attributes and observed
#'   classifications. Correct and incorrect classifications, e.g., False
#'   positive and false negative status, are captured for each observation in
#'   the dataset. Structural decomposition can be completed using these data in
#'   subsequent ODA models.}
#'
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @importFrom stringr str_extract
#'
#' @author Nathaniel J. Rhodes
#'
#' @seealso \code{\link{ODAmanual}} \code{\link{ODAclean}} \code{\link{ODAload}}
#'   \code{\link{ODArun}} \code{\link{NOVOboot}}
#'
#' @examples
#' # Not run:
#' # ODAparse(1)
#'
#' @references Yarnold P.R. and Soltysik R.C. (2005). \emph{Optimal data
#'   analysis: Guidebook with software for Windows}. APA Books.
#'
#'   Yarnold, P.R. and Soltysik, R.C. (2016). \emph{Maximizing Predictive
#'   Accuracy}. ODA Books. DOI: 10.13140/RG.2.1.1368.3286.
#'
ODAparse <- function(run="",...) {
  if(run==""){
    stop(
      print("Message: User must specify a run number of the ODAparse results to load.\n")
    )
  }
  out <- "MODEL.OUT"
  addlruns <- list(...)
  if (length(addlruns) > 0) {
    allruns <- c(run, unlist(addlruns))
  }else {
    allruns <- run
  }
  msumm <- list()
  psumm <- list()
  ksumm <- list()
  dsumm <- list()
  lsumm <- list()
  sdsum <- list()
  ssumm <- list()
  for (thisrun in allruns){
    #### Extract the ODA models, variables, and class summaries from the OUT file####
    rundir <- paste(getwd(),"Runs",thisrun,sep="/")
    runfile <- paste(rundir,list.files(rundir,pattern="*.csv"),sep="/")
    outfile <- paste(getwd(),"Runs",thisrun,out,sep="/")
    if(!file.exists(outfile)){
      stop(cat("Error: No valid OUT file found in the directory ~/Run",thisrun,".\n"))
    }
    file <- readLines(outfile)
    data_raw <- data.frame(file,stringsAsFactors = FALSE)
    index1 <- grep("^*ODA model:",file) + 2               # Beginning of ODA model
    index2 <- grep("^*Monte Carlo summary",file) - 3      # End of ODA model
    index3 <- grep("^*Summary for Class",file)            # Beginning of ODA performance summary
    index5 <- grep("^*No solution found for this problem",file)-16
    vars <- grep("^*Class:",file)+1
    vars <- vars[!vars %in% index5]
    vars <- data_raw[vars,]
    vars <- gsub(" ","",vars)
    vars <- gsub("Attribute:","",vars)
    vars <- strsplit(vars, "(?=[Vv])", perl=T)
    vars <- matrix(unlist(vars), ncol=2, byrow=T)
    vars <- as.data.frame(vars, stringsAsFactors = T)
    attribute <- as.numeric(as.character(vars[,2]))
    vars <- grep("^*Class:",file)
    vars <- vars[!vars %in% index5]
    vars <- data_raw[vars,]
    vars <- gsub(" ","",vars)
    vars <- gsub("Class:","",vars)
    vars <- strsplit(vars, "(?=[Vv])", perl=T)
    vars <- matrix(unlist(vars), ncol=2, byrow=T)
    vars <- as.data.frame(vars, stringsAsFactors = T)
    class <- as.numeric(as.character(vars[,2]))
    n.model <- seq_along(index1)
    cat.check <- grep("^*CAT ",file) # determine if a categorical model specified
    loo.check <- grep("^LOO OFF;",file)
    loo.cat.check <- grep("^Warning  7: WEIGHTed CATEGORICAL LOO not available. LOO switched to OFF.",file)
    if(length(loo.cat.check)>0){cat("Message: Weighted categorical LOO not available. LOO swithched to OFF.\n")}
    wt.check <- grep("^WEIGHT ",file)
    #### Warning if one or more attributes had no solutions found.####
    if(length(index5) > 0){
      stop(paste0("The following are attributes for which no solutions were found: "),data_raw[index5,],"\n",
           "Warning: Remove the attribute(s) above and complete ODArun again before running ODAparse.\n")
    }
    #### Method for standard CAT and non-CAT ODA Models: ####
    # CAT model if indices differ > 1, otherwise non-CAT if indices differ > 0 #
    if (max(index2-index1)>0 & length(index1)==length(index2)){
      models <- c()
      classes <- c()
      for(i in n.model) {
        models[[i]] <- cbind(data_raw[(index1[i]:index2[i]),])
        classes[[i]] <- gsub(".*THEN *","",models[[i]])
        models[[i]] <- gsub(".*IF\\s*|THEN.*","",models[[i]])
      }
      models <- as.data.frame(matrix(models,ncol=1,byrow=T,dimnames=list(NULL,c("models"))),stringsAsFactors = T)
      classes <- as.data.frame(matrix(classes,ncol=1,byrow=T,dimnames=list(NULL,c("classes"))),stringsAsFactors = T)
      p.start <- grep("^Estimated p:",file)
      p.val <- data_raw[p.start,]
      p.val <- stringr::str_extract(string = p.val, pattern = "[\\d+]?\\.*\\d*$")
      if(length(wt.check>0)){
        if(length(loo.check)>0 | length(loo.cat.check)>0){
          index4 <- grep("^*Summary for Class",file)+20
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 21))
          e.start <- grep("^Monte Carlo summary ", file)
          ess <- data_raw[e.start+40,]
          ess <- strsplit(ess, "      ")
          ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
          ess <- gsub("Effect strength PAC ","",ess)
          ess <- gsub("Effect strength PV ","",ess)
          wt.ess <- data_raw[e.start+42,]
          wt.ess <- strsplit(wt.ess, "      ")
          wt.ess <- matrix(unlist(wt.ess), ncol=2, byrow=TRUE)
          wt.ess <- gsub("Effect strength wtd PAC ","",wt.ess)
          wt.ess <- gsub("Effect strength wtd PV ","",wt.ess)
          c.lab <- as.vector(c("Overall ESS:","Overall ESP:","Weighted ESS:","Weighted ESP:"))
          c.model <- setNames(data.frame(paste0(n.model),models, classes, ess,wt.ess,p.val),
                              c("Model:","IF Attributes:", "THEN Predict:",c.lab,"MC P-value:"))
        }
        else{
          index4 <- grep("^*Summary for Class",file)+20
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 21))
          e.start <- grep("^Results of leave-one-out analysis", file)
          ess <- data_raw[e.start-6,]
          ess <- strsplit(ess, "      ")
          ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
          ess <- gsub("Effect strength PAC ","",ess)
          ess <- gsub("Effect strength PV ","",ess)
          wt.ess <- data_raw[e.start-4,]
          wt.ess <- strsplit(wt.ess, "      ")
          wt.ess <- matrix(unlist(wt.ess), ncol=2, byrow=TRUE)
          wt.ess <- gsub("Effect strength wtd PAC ","",wt.ess)
          wt.ess <- gsub("Effect strength wtd PV ","",wt.ess)
          loo.start <- grep("^Summary for Class", file)
          ess.loo <- data_raw[loo.start-4,]
          ess.loo <- strsplit(ess.loo, "      ")
          ess.loo <- matrix(unlist(ess.loo), ncol=2, byrow=TRUE)
          ess.loo <- gsub("Effect strength PAC ","",ess.loo)
          ess.loo <- gsub("Effect strength PV ","",ess.loo)
          wt.ess.loo <- data_raw[loo.start-2,]
          wt.ess.loo <- strsplit(wt.ess.loo, "      ")
          wt.ess.loo <- matrix(unlist(wt.ess.loo), ncol=2, byrow=TRUE)
          wt.ess.loo <- gsub("Effect strength wtd PAC ","",wt.ess.loo)
          wt.ess.loo <- gsub("Effect strength wtd PV ","",wt.ess.loo)
          fp.start <- grep("^Fisher's exact test \\(directional",file)
          fp.val <- data_raw[fp.start,]
          if(length(fp.val)>0){
            fp.val <- strsplit(fp.val, "=")
            fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
            fp.val <- fp.val[,2]
            c.lab <- as.vector(c("Overall ESS:","Overall ESP:","Weighted ESS:","Weighted ESP:","MC P-value:","LOO ESS:","LOO ESP:","Weighted LOO ESS:","Weighted LOO ESP:","LOO P-value:"))
            c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, wt.ess, p.val, ess.loo, wt.ess.loo, fp.val),
                                c("Model:","IF Attributes:", "THEN Predict:",c.lab))
          }else{
            c.lab <- as.vector(c("Overall ESS","Overall ESP:","Weighted ESS:","Weighted ESP:","MC P-value:","LOO ESS:","LOO ESP:","Weighted LOO ESS:","Weighted LOO ESP:"))
            c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, wt.ess, p.val, ess.loo, wt.ess.loo),
                                c("Model","IF Attributes:", "THEN Predict:",c.lab))
          }
        }
      }
      else{
        if(length(loo.check)>0 | length(loo.cat.check)>0){
          index4 <- grep("^*Summary for Class",file)+12
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
          e.start <- grep("^Monte Carlo summary ", file)
          ess <- data_raw[e.start+35,]
          ess <- strsplit(ess, "      ")
          ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
          ess <- gsub("Effect strength PAC ","",ess)
          ess <- gsub("Effect strength PV ","",ess)
          c.lab <- as.vector(c("Overall ESS:","Overall ESP:"))
          c.model <- setNames(data.frame(paste0(n.model),models, classes, ess,p.val),
                              c("Model:","IF Attributes:", "THEN Predict:",c.lab,"MC P-value:"))
        }else{
          index4 <- grep("^*Summary for Class",file)+12
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
          e.start <- grep("^Results of leave-one-out analysis", file)
          ess <- data_raw[e.start-4,]
          ess <- strsplit(ess, "      ")
          ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
          ess <- gsub("Effect strength PAC ","",ess)
          ess <- gsub("Effect strength PV ","",ess)
          loo.start <- grep("^Summary for Class", file)
          ess.loo <- data_raw[loo.start-2,]
          ess.loo <- strsplit(ess.loo, "      ")
          ess.loo <- matrix(unlist(ess.loo), ncol=2, byrow=TRUE)
          ess.loo <- gsub("Effect strength PAC ","",ess.loo)
          ess.loo <- gsub("Effect strength PV ","",ess.loo)
          fp.start <- grep("^Fisher's exact test \\(directional",file)
          fp.val <- data_raw[fp.start,]
          if(length(fp.val)>0){
            fp.val <- strsplit(fp.val, "=")
            fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
            fp.val <- fp.val[,2]
            c.lab <- as.vector(c("Overall ESS:","Overall ESP:","MC P-value:","LOO ESS:","LOO ESP:","LOO P-value:"))
            c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, p.val, ess.loo, fp.val),
                                c("Model","Attributes", "Predictions",c.lab))
          }else{
            c.lab <- as.vector(c("Overall ESS:","Overall ESP:","MC P-value:","LOO ESS:","LOO ESP:"))
            c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, p.val, ess.loo),
                                c("Model","Attributes", "Predictions",c.lab))
          }
        }
      }
      ### Return ODA model summary ###
      msumm[[thisrun]] <- c.model

      l.list <- list()
      for(i in seq_along(index1)){
        l.list[[i]]<-data_raw[c(index1[i]:index2[i]),]
      }
      string.list <- list()
      for(i in seq_along(index1)){
        string.list[[i]]<-strsplit(l.list[[i]], "THEN")
      }
      mat.list <- list()
      for(i in seq_along(index1)){
        mat.list[[i]]<-matrix(unlist(string.list[[i]]), ncol=2, byrow=TRUE)
      }
      print.list <- list()
      for(i in seq_along(index1)){
        print.list[[i]]<-gsub(".*IF\\s*","",mat.list[[i]])
      }
      lh <- list() # left handed display = categorical attribute decision rules
      for(i in seq_along(index1)){
        lh[[i]]<-print.list[[i]][,1]
      }
      rh <- list() # right handed display = class predictions based on decision rules
      for(i in seq_along(index1)){
        rh[[i]]<-print.list[[i]][,2]
      }
      rc <- list() # class prediction for each decision rule on the left
      for(i in seq_along(index1)){
        rc[[i]]<-as.numeric(gsub(".*=","",rh[[i]]))
      }
      pred <- rc
      cut <- list()  # decision rule on left hand side taking the first one
      for(i in seq_along(index1)){
        cut[[i]]<-lh[[i]]
        cut[[i]]<-gsub(".*=","",cut[[i]])
        cut[[i]]<-gsub(".*<","",cut[[i]])
        cut[[i]]<-gsub(".[a-zA-Z]{1}([0-9]{5}|[0-9]{4}|[0-9]{3}|[0-9]{2}|[0-9]{1})","",cut[[i]])
        cut[[i]]<-as.numeric(cut[[i]])
        cut[[i]]<-cut[[i]][!is.na(cut[[i]])]
      }

      data <- read.csv(runfile)
      header <- names(data)
      c.header <- seq_len(ncol(data))
      key <- data.frame(header, paste("v",c.header,sep=""))
      names(data) <- key[,2]
      cv <- data[unique(class)]
      data[data==-9] <- NA
      data <- data[attribute]

      for(i in n.model){
        stats[, c(paste0("model.",n.model[i]))] <- cbind(data_raw[(index3[i]:index4[i]),])
      }
      ### Return ODA Performance Index###
      ssumm[[thisrun]]<-stats

      df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
      for(i in n.model){
        pos<-setNames(data.frame(cut[[i]],pred[[i]]),c(names(data[i]),"class"))
        if(length(cut[[i]])>1){
          df[, c(paste0("pv.",n.model[i]))] <- as.vector(pos[match(unlist(data[i]),unlist(pos[1])),2])
        }else{
          df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[[i]], pred[[i]][2], pred[[i]][1]))
        }
      }

      predictions <- df
      df_list <- lapply(df, function(x){
        dfs <- list()
        for(j in seq_along(cv))
        {
          dfs[[j]] <- data.frame(ifelse(x == cv[j] & x==max(cv[j]), 1, 0),
                                 ifelse(x == cv[j] & x==min(cv[j]), 1, 0),
                                 ifelse(x != cv[j] & cv[j] < x, 1, 0),
                                 ifelse(x != cv[j] & cv[j] > x, 1, 0))
        }
        setNames(do.call("cbind", dfs),
                 paste0(c("tp.", "tn.", "fp.", "fn."), rep(seq_along(cv), each = 4)))
      })
      ###Create predictions for each model and store for SDA###
      index6 <- seq_along(cv)*4*nrow(data)
      sda <- lapply(df_list, function(x) as.numeric(unlist(x)))
      if(length(sda) > 1){
        sda.mat <- do.call(cbind, sda)
        res <- c()
        for(i in seq_along(index6)){
          res[[i]] <- sda.mat[seq((index6[i]-min(index6)+1),index6[i]),]
          rownames(res[[i]]) <- paste0(c(rep("tp.",each=nrow(data)),rep("tn.",each=nrow(data)),rep("fp.",each=nrow(data)),rep("fn.",each=nrow(data))),rep(seq(1,nrow(data),by=1),times=4))
        }
        sda.list <- do.call(rbind, res)
      }else{
        sda.list <- sda
      }
      sda.frame <- as.data.frame(sda.list)
      sda.frame$id <- rep(seq(1,nrow(data),by=1),times=4)
      sda.frame$outcome <- rep(seq_along(cv),each=4*nrow(data))
      sda.frame$classification <- c(rep("tp",each=nrow(data)),rep("tn",each=nrow(data)),rep("fp",each=nrow(data)),rep("fn",each=nrow(data)))
      ### Return SDA report for ODA Model ###
      sdsum[[thisrun]] <- sda.frame

      df <- predictions
      cc_list <- lapply(df, function(x){
        cct <- list()
        for(j in seq_along(cv))
        {
          cct[[j]] <- table(cbind(cv[j],x)
          )
        }
        setNames(do.call("list", cct), names(cv))
      })
      ### Return List of ODA Cross Class Tables###
      lsumm[[thisrun]] <- cc_list

      temp <- lapply(df_list,sapply,sum,na.rm=T)

      temp2 <- as.data.frame(do.call(rbind, temp))

      acc.n <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+1)])))

      acc.d <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+3)])))

      tp <- temp2[,seq(1, ncol(temp2), 4)]        # TP
      tn <- temp2[,seq(2, ncol(temp2), 4)]        # TN
      fp <- temp2[,seq(3, ncol(temp2), 4)]        # FP
      fn <- temp2[,seq(4, ncol(temp2), 4)]        # FN

      n.obs.class1 <- (tn+fp)
      n.obs.class2 <- (tp+fn)
      n.pred.class1 <- (tn+fn)
      n.pred.class2 <- (tp+fp)

      pac <- round(acc.n/acc.d*100,digits=1)      # Overall accuracy in classification
      names(pac) <- paste0(rep("PAC.",length(cv)), seq_along(cv))

      sens <- round(tp/(tp+fn)*100, digits=2)     # Sens
      names(sens) <- paste0(rep("Sens.",length(cv)), seq_along(cv))

      spec <- round(tn/(tn+fp)*100, digits=2)     # Spec
      names(spec) <- paste0(rep("Spec.",length(cv)), seq_along(cv))

      ppv <- round(tp/(tp+fp)*100, digits=2)      # PPV
      names(ppv) <- paste0(rep("PPV.",length(cv)), seq_along(cv))

      npv <- round(tn/(tn+fn)*100, digits=2)      # NPV
      names(npv) <- paste0(rep("NPV.",length(cv)), seq_along(cv))

      mpv <- round((ppv+npv)/2, digits=2)         # Mean predictive value in classification
      names(mpv) <- paste0(rep("MPV.",length(cv)), seq_along(cv))

      mpac <- round((sens+spec)/2, digits=2)      # Mean percent accuracy in classification
      names(mpac) <- paste0(rep("MPAC.",length(cv)), seq_along(cv))

      ess.m <- round(100*(mpac-50)/50, digits=2)  # Effect strength for sensitivity
      names(ess.m) <- paste0(rep("ESS.",length(cv)), seq_along(cv))

      esp.m <- round(100*(mpv-50)/50, digits=2)   # Effect strength for Positivity
      names(esp.m) <- paste0(rep("ESP.",length(cv)), seq_along(cv))

      for(i in seq_along(n.model)){
        if(min(tp[i],tn[i],fp[i],fn[i])==0){
          tp[i] <- tp[i]+0.5
          tn[i] <- tn[i]+0.5
          fp[i] <- fp[i]+0.5
          fn[i] <- fn[i]+0.5
          cat(paste0("An observed cell count of zero for model: ",i," in run: ",thisrun," was identified.\n"))
        } else{
          tp[i] <- tp[i]
          tn[i] <- tn[i]
          fp[i] <- fp[i]
          fn[i] <- fn[i]
        }
      }

      OR <- round((tp*tn)/(fp*fn), digits=2)      # OR
      names(OR) <- paste0(rep("OR.",length(cv)), seq_along(cv))

      SE.OR <- sqrt((1/tp)+(1/fp)+(1/fn)+(1/tn))        #Parametric SE for OR

      LL.OR <- round(OR * exp(-1.96 * SE.OR), digits=2)        #95% CI lower limit
      names(LL.OR) <- paste0(rep("LL.OR.",length(cv)), seq_along(cv))

      UL.OR <- round(OR * exp(1.96 * SE.OR), digits=2)         #95% CI upper limit
      names(UL.OR) <- paste0(rep("UL.OR.",length(cv)), seq_along(cv))

      perf <- cbind(n.obs.class1,n.pred.class1,n.obs.class2,n.pred.class2,pac,mpac,sens,spec,ess.m,mpv,ppv,npv,esp.m,OR,LL.OR,UL.OR)

      index7 <- seq_along(cv)*(nrow(perf)/length(cv))

      res <- c()
      for(i in seq_along(index7)){
        res[[i]] <- perf[(index7[i]-min(index7)+1):index7[i],seq(i,ncol(perf),by=length(index7))]
        names(res[[i]]) <- sub("*\\.+\\d", "", names(res[[i]]))
      }
      perf.list <- do.call(rbind, res)
      p.list <- c()
      for(i in n.model){
        p.list$Class_variable <- c(paste0("V",class))
        p.list$Class_name[[i]] <- header[class[i]]
        p.list$Attribute[[i]] <- c(paste0("V",attribute[i]))
        p.list$Attribute_name[[i]] <- header[attribute[i]]
      }
      ###Return ODA model performance object###
      perf_list <- as.data.frame(do.call("cbind",c(p.list,perf.list)))
      psumm[[thisrun]] <- perf_list
      ###Return data key with new variable names###
      a.key <- as.matrix(key[attribute,1])
      c.key <- as.matrix(key[class,1])
      oda.key <- data.frame(a.key,names(data),c.key,names(cv))
      names(oda.key) <- c("Attribute Label","Attribute","Class Label","Class")
      ksumm[[thisrun]] <- oda.key
      ###Return ODA data used for analysis###
      data$class <- cv[,1]
      data <- cbind(data,predictions)
      dsumm[[thisrun]] <- data
    }
    #### Method for GEN ODA Models: ####
    else{
      if(length(n.model)>1){
        stop(cat("Error: Multiple attributes are not supported with GEN models currently.\nUpdate ODArun() to include only one attribute for the GEN model.\n"))
      }
      gen.ind <- grep("^Results for group",file)
      gen.val <- data_raw[gen.ind,]
      gen.val <- gsub("Results for group ","",gen.val)
      gen.val <- unique(gen.val)
      gen.val <- c("Overall",gen.val)
      models <- c()
      classes <- c()
      for(i in n.model) {
        models[[i]] <- cbind(data_raw[(index1[i]:index2[i]),])
        classes[[i]] <- gsub(".*THEN *","",models[[i]])
        models[[i]] <- gsub(".*IF\\s*|THEN.*","",models[[i]])
      }
      models <- as.data.frame(matrix(models,ncol=1,byrow=T,dimnames=list(NULL,c("models"))),stringsAsFactors = T)
      classes <- as.data.frame(matrix(classes,ncol=1,byrow=T,dimnames=list(NULL,c("classes"))),stringsAsFactors = T)
      p.start <- grep("^Estimated p:",file)
      p.val <- data_raw[p.start,]
      p.val <- stringr::str_extract(string = p.val, pattern = "[\\d+]?\\.*\\d*$")
      if(length(wt.check>0)){
        if(length(loo.check)>0 | length(loo.cat.check)>0){
          index4 <- grep("^*Summary for Class",file)+20
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 21))
          e.start <- grep("^Monte Carlo summary ", file)
          if(length(cat.check)>0 | length(loo.cat.check)>0){
            linestart <- 40
            linenum <- c(linestart,rep(linestart,each=length(index2)-1)+1)
            ess <- data_raw[e.start+linenum,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
            linestart <- 42
            linenum <- c(linestart,rep(linestart,each=length(index2)-1)+1)
            wt.ess <- data_raw[e.start+linenum,]
            wt.ess <- strsplit(wt.ess, "      ")
            wt.ess <- matrix(unlist(wt.ess), ncol=2, byrow=TRUE)
            wt.ess <- gsub("Effect strength wtd PAC ","",wt.ess)
            wt.ess <- gsub("Effect strength wtd PV ","",wt.ess)
          }else{
            ess <- data_raw[e.start+40,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
            wt.ess <- data_raw[e.start+42,]
            wt.ess <- strsplit(wt.ess, "      ")
            wt.ess <- matrix(unlist(wt.ess), ncol=2, byrow=TRUE)
            wt.ess <- gsub("Effect strength wtd PAC ","",wt.ess)
            wt.ess <- gsub("Effect strength wtd PV ","",wt.ess)
          }
          c.lab <- as.vector(c("Overall ESS:","Overall ESP:","Weighted ESS:","Weighted ESP:"))
          c.model <- setNames(data.frame(paste0(n.model),gen.val, models, classes, ess,wt.ess,p.val),
                              c("Model:","GEN Group:","IF Attributes:", "THEN Predict:",c.lab,"MC P-value:"))
        }else{
          index4 <- grep("^*Summary for Class",file)+20
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 21))
          e.start <- grep("^Monte Carlo summary ", file)
          ess <- data_raw[e.start+40,]
          ess <- strsplit(ess, "      ")
          ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
          ess <- gsub("Effect strength PAC ","",ess)
          ess <- gsub("Effect strength PV ","",ess)
          wt.ess <- data_raw[e.start+42,]
          wt.ess <- strsplit(wt.ess, "      ")
          wt.ess <- matrix(unlist(wt.ess), ncol=2, byrow=TRUE)
          wt.ess <- gsub("Effect strength wtd PAC ","",wt.ess)
          wt.ess <- gsub("Effect strength wtd PV ","",wt.ess)
          loo.start <- grep("^Fisher's exact test \\(directional",file)
          ess.loo <- data_raw[loo.start+26,]
          ess.loo <- strsplit(ess.loo, "      ")
          ess.loo <- matrix(unlist(ess.loo), ncol=2, byrow=TRUE)
          ess.loo <- gsub("Effect strength PAC ","",ess.loo)
          ess.loo <- gsub("Effect strength PV ","",ess.loo)
          wt.ess.loo <- data_raw[loo.start+28,]
          wt.ess.loo <- strsplit(wt.ess.loo, "      ")
          wt.ess.loo <- matrix(unlist(wt.ess.loo), ncol=2, byrow=TRUE)
          wt.ess.loo <- gsub("Effect strength wtd PAC ","",wt.ess.loo)
          wt.ess.loo <- gsub("Effect strength wtd PV ","",wt.ess.loo)
          fp.start <- grep("^Fisher's exact test \\(directional",file)
          fp.val <- data_raw[fp.start,]
          if(length(fp.val)>0){
            fp.val <- strsplit(fp.val, "=")
            fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
            fp.val <- fp.val[,2]
            c.lab <- as.vector(c("Overall ESS:","Overall ESP:","Weighted ESS:","Weighted ESP:","MC P-value:","LOO ESS:","LOO ESP:","Weighted LOO ESS:","Weighted LOO ESP:","LOO P-value:"))
            c.model <- setNames(data.frame(paste0(n.model), gen.val, models, classes, ess, wt.ess, p.val, ess.loo, wt.ess.loo, fp.val),
                                c("Model:","GEN Group:", "IF Attributes:", "THEN Predict:",c.lab))
          }else{
            c.lab <- as.vector(c("Overall ESS","Overall ESP:","Weighted ESS:","Weighted ESP:","MC P-value:","LOO ESS:","LOO ESP:","Weighted LOO ESS:","Weighted LOO ESP:"))
            c.model <- setNames(data.frame(paste0(n.model), gen.val, models, classes, ess, wt.ess, p.val, ess.loo, wt.ess.loo),
                                c("Model","GEN Group:", "IF Attributes:", "THEN Predict:",c.lab))
          }
        }
      }
      else{
        if(length(loo.check)>0 | length(loo.cat.check)>0){
          index4 <- grep("^*Summary for Class",file)+12
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
          e.start <- grep("^Monte Carlo summary ", file)
          if(length(cat.check)>0 | length(loo.cat.check)>0){
            linestart <- 36
            linenum <- c(linestart,rep(linestart,each=length(index2)-1)+1)
            ess <- data_raw[e.start+linenum,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
          }else{
            ess <- data_raw[e.start+35,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
          }
          c.lab <- as.vector(c("Overall ESS:","Overall ESP:"))
          c.model <- setNames(data.frame(paste0(n.model),gen.val, models, classes, ess,p.val),
                              c("Model:","GEN Group:", "IF Attributes:", "THEN Predict:",c.lab,"MC P-value:"))
        }else{
          index4 <- grep("^*Summary for Class",file)+12
          stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
          e.start <- grep("^Monte Carlo summary ", file)
          if(length(cat.check)>0 | length(loo.cat.check)>0 ){
            linestart <- 36
            linenum <- c(linestart,rep(linestart,each=length(index2)-1)+1)
            ess <- data_raw[e.start+linenum,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
          }else{
            ess <- data_raw[e.start+35,]
            ess <- strsplit(ess, "      ")
            ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
            ess <- gsub("Effect strength PAC ","",ess)
            ess <- gsub("Effect strength PV ","",ess)
          }
          loo.start <- grep("^Fisher's exact test \\(directional",file)
          ess.loo <- data_raw[loo.start+21,]
          ess.loo <- strsplit(ess.loo, "      ")
          ess.loo <- matrix(unlist(ess.loo), ncol=2, byrow=TRUE)
          ess.loo <- gsub("Effect strength PAC ","",ess.loo)
          ess.loo <- gsub("Effect strength PV ","",ess.loo)
          fp.start <- grep("^Fisher's exact test \\(directional",file)
          fp.val <- data_raw[fp.start,]
          if(length(fp.val)>0){
            fp.val <- strsplit(fp.val, "=")
            fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
            fp.val <- fp.val[,2]
            c.lab <- as.vector(c("Overall ESS:","Overall ESP:","MC P-value:","LOO ESS:","LOO ESP:","LOO P-value:"))
            c.model <- setNames(data.frame(paste0(n.model), gen.val, models, classes, ess, p.val, ess.loo, fp.val),
                                c("Model:","GEN Group:","IF Attributes:", "THEN Predict:",c.lab))
          }else{
            c.lab <- as.vector(c("Overall ESS","Overall ESP:","MC P-value:","LOO ESS:","LOO ESP:"))
            c.model <- setNames(data.frame(paste0(n.model), gen.val, models, classes, ess, p.val, ess.loo),
                                c("Model","GEN Group:","IF Attributes:", "THEN Predict:",c.lab))
          }
        }
      }
      ### Return ODA model summary ###
      msumm[[thisrun]] <- c.model

      l.list <- list()
      for(i in seq_along(index1)){
        l.list[[i]]<-data_raw[c(index1[i]:index2[i]),]
      }
      string.list <- list()
      for(i in seq_along(index1)){
        string.list[[i]]<-strsplit(l.list[[i]], "THEN")
      }
      mat.list <- list()
      for(i in seq_along(index1)){
        mat.list[[i]]<-matrix(unlist(string.list[[i]]), ncol=2, byrow=TRUE)
      }
      print.list <- list()
      for(i in seq_along(index1)){
        print.list[[i]]<-gsub(".*IF\\s*","",mat.list[[i]])
      }
      lh <- list() # left handed display = categorical attribute decision rules
      for(i in seq_along(index1)){
        lh[[i]]<-print.list[[i]][,1]
      }
      rh <- list() # right handed display = class predictions based on decision rules
      for(i in seq_along(index1)){
        rh[[i]]<-print.list[[i]][,2]
      }
      rc <- list() # class prediction for each decision rule on the left
      for(i in seq_along(index1)){
        rc[[i]]<-as.numeric(gsub(".*=","",rh[[i]]))
      }
      pred <- rc
      cut <- list()  # decision rule on left hand side taking the first one
      for(i in seq_along(index1)){
        cut[[i]]<-lh[[i]]
        cut[[i]]<-gsub(".*=","",cut[[i]])
        cut[[i]]<-gsub(".*<","",cut[[i]])
        cut[[i]]<-gsub(".[a-zA-Z]{1}([0-9]{5}|[0-9]{4}|[0-9]{3}|[0-9]{2}|[0-9]{1})","",cut[[i]])
        cut[[i]]<-as.numeric(cut[[i]])
        cut[[i]]<-cut[[i]][!is.na(cut[[i]])]
      }

      data <- read.csv(runfile)
      header <- names(data)
      c.header <- seq_len(ncol(data))
      key <- data.frame(header, paste("v",c.header,sep=""))
      names(data) <- key[,2]
      cv <- data[unique(class)]
      data[data==-9] <- NA
      data <- data[attribute]

      for(i in n.model){
        stats[, c(paste0("model.",n.model[i]))] <- cbind(data_raw[(index3[i]:index4[i]),])
      }
      ### Return ODA Performance Index###
      ssumm[[thisrun]] <- stats

      df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
      for(i in n.model){
        pos<-setNames(data.frame(cut[[i]],pred[[i]]),c(names(data[i]),"class"))
        if(length(cut[[i]])>1){
          df[, c(paste0("pv.",n.model[i]))] <- as.vector(pos[match(unlist(data[i]),unlist(pos[1])),2])
        }else{
          df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[[i]], pred[[i]][2], pred[[i]][1]))
        }
      }

      predictions <- df
      df_list <- lapply(df, function(x){
        dfs <- list()
        for(j in seq_along(cv))
        {
          dfs[[j]] <- data.frame(ifelse(x == cv[j] & x==max(cv[j]), 1, 0),
                                 ifelse(x == cv[j] & x==min(cv[j]), 1, 0),
                                 ifelse(x != cv[j] & cv[j] < x, 1, 0),
                                 ifelse(x != cv[j] & cv[j] > x, 1, 0))
        }
        setNames(do.call("cbind", dfs),
                 paste0(c("tp.", "tn.", "fp.", "fn."), rep(seq_along(cv), each = 4)))
      })
      ###Create predictions for each model and store for SDA###
      index6 <- seq_along(cv)*4*nrow(data)
      sda <- lapply(df_list, function(x) as.numeric(unlist(x)))
      if(length(sda) > 1){
        sda.mat <- do.call(cbind, sda)
        res <- c()
        for(i in seq_along(index6)){
          res[[i]] <- sda.mat[seq((index6[i]-min(index6)+1),index6[i]),]
          rownames(res[[i]]) <- paste0(c(rep("tp.",each=nrow(data)),rep("tn.",each=nrow(data)),rep("fp.",each=nrow(data)),rep("fn.",each=nrow(data))),rep(seq(1,nrow(data),by=1),times=4))
        }
        sda.list <- do.call(rbind, res)
      }else{
        sda.list <- sda
      }
      sda.frame <- as.data.frame(sda.list)
      sda.frame$id <- rep(seq(1,nrow(data),by=1),times=4)
      sda.frame$outcome <- rep(seq_along(cv),each=4*nrow(data))
      sda.frame$classification <- c(rep("tp",each=nrow(data)),rep("tn",each=nrow(data)),rep("fp",each=nrow(data)),rep("fn",each=nrow(data)))
      ### Return SDA report for ODA Model ###
      sdsum[[thisrun]] <- sda.frame

      df <- predictions
      cc_list <- lapply(df, function(x){
        cct <- list()
        for(j in seq_along(cv))
        {
          cct[[j]] <- table(cbind(cv[j],x)
          )
        }
        setNames(do.call("list", cct), names(cv))
      })
      ### Extract List of ODA Cross Class Tables###
      lsumm[[thisrun]] <- cc_list

      temp <- lapply(df_list,sapply,sum,na.rm=T)

      temp2 <- as.data.frame(do.call(rbind, temp))

      acc.n <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+1)])))

      acc.d <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+3)])))

      tp <- temp2[,seq(1, ncol(temp2), 4)]        # TP
      tn <- temp2[,seq(2, ncol(temp2), 4)]        # TN
      fp <- temp2[,seq(3, ncol(temp2), 4)]        # FP
      fn <- temp2[,seq(4, ncol(temp2), 4)]        # FN

      n.obs.class1 <- (tn+fp)
      n.obs.class2 <- (tp+fn)
      n.pred.class1 <- (tn+fn)
      n.pred.class2 <- (tp+fp)

      pac <- round(acc.n/acc.d*100,digits=1)      # Overall accuracy in classification
      names(pac) <- paste0(rep("PAC.",length(cv)), seq_along(cv))

      sens <- round(tp/(tp+fn)*100, digits=2)     # Sens
      names(sens) <- paste0(rep("Sens.",length(cv)), seq_along(cv))

      spec <- round(tn/(tn+fp)*100, digits=2)     # Spec
      names(spec) <- paste0(rep("Spec.",length(cv)), seq_along(cv))

      ppv <- round(tp/(tp+fp)*100, digits=2)      # PPV
      names(ppv) <- paste0(rep("PPV.",length(cv)), seq_along(cv))

      npv <- round(tn/(tn+fn)*100, digits=2)      # NPV
      names(npv) <- paste0(rep("NPV.",length(cv)), seq_along(cv))

      mpv <- round((ppv+npv)/2, digits=2)         # Mean predictive value in classification
      names(mpv) <- paste0(rep("MPV.",length(cv)), seq_along(cv))

      mpac <- round((sens+spec)/2, digits=2)      # Mean percent accuracy in classification
      names(mpac) <- paste0(rep("MPAC.",length(cv)), seq_along(cv))

      ess.m <- round(100*(mpac-50)/50, digits=2)  # Effect strength for sensitivity
      names(ess.m) <- paste0(rep("ESS.",length(cv)), seq_along(cv))

      esp.m <- round(100*(mpv-50)/50, digits=2)   # Effect strength for Positivity
      names(esp.m) <- paste0(rep("ESP.",length(cv)), seq_along(cv))

      for(i in seq_along(n.model)){
        if(min(tp[i],tn[i],fp[i],fn[i])==0){
          tp[i] <- tp[i]+0.5
          tn[i] <- tn[i]+0.5
          fp[i] <- fp[i]+0.5
          fn[i] <- fn[i]+0.5
          cat(paste0("An observed cell count of zero for model: ",i," in run: ",thisrun," was identified.\n"))
        } else{
          tp[i] <- tp[i]
          tn[i] <- tn[i]
          fp[i] <- fp[i]
          fn[i] <- fn[i]
        }
      }

      OR <- round((tp*tn)/(fp*fn), digits=2)      # OR
      names(OR) <- paste0(rep("OR.",length(cv)), seq_along(cv))

      SE.OR <- sqrt((1/tp)+(1/fp)+(1/fn)+(1/tn))        #Parametric SE for OR

      LL.OR <- round(OR * exp(-1.96 * SE.OR), digits=2)        #95% CI lower limit
      names(LL.OR) <- paste0(rep("LL.OR.",length(cv)), seq_along(cv))

      UL.OR <- round(OR * exp(1.96 * SE.OR), digits=2)         #95% CI upper limit
      names(UL.OR) <- paste0(rep("UL.OR.",length(cv)), seq_along(cv))

      perf <- cbind(n.obs.class1,n.pred.class1,n.obs.class2,n.pred.class2,pac,mpac,sens,spec,ess.m,mpv,ppv,npv,esp.m,OR,LL.OR,UL.OR)

      index7 <- seq_along(cv)*(nrow(perf)/length(cv))

      res <- c()
      for(i in seq_along(index7)){
        res[[i]] <- perf[(index7[i]-min(index7)+1):index7[i],seq(i,ncol(perf),by=length(index7))]
        names(res[[i]]) <- sub("*\\.+\\d", "", names(res[[i]]))
      }
      perf.list <- do.call(rbind, res)
      p.list <- c()
      for(i in n.model){
        p.list$Class_variable <- c(paste0("V",class))
        p.list$Class_name[[i]] <- header[class[i]]
        p.list$Attribute[[i]] <- c(paste0("V",attribute[i]))
        p.list$Attribute_name[[i]] <- header[attribute[i]]
      }
      ###Return ODA model performance object###
      perf_list <- as.data.frame(do.call("cbind",c(p.list,perf.list)))
      psumm[[thisrun]] <- perf_list
      ###Return data key with new variable names###
      a.key <- as.matrix(key[attribute,1])
      c.key <- as.matrix(key[class,1])
      oda.key <- data.frame(a.key,names(data),c.key,names(cv))
      names(oda.key) <- c("Attribute Label","Attribute","Class Label","Class")
      ksumm[[thisrun]] <- oda.key
      ###Return ODA data used for analysis###
      data$class <- cv[,1]
      data <- cbind(data,predictions)
      dsumm[[thisrun]] <- data
      assign(paste0("oda.data.",thisrun), data, pos = parent.frame())
    }
  }
  for (thisrun in allruns){
    assign(paste0("oda.model.",thisrun), msumm[[thisrun]], pos = parent.frame())
    assign(paste0("oda.perf.",thisrun), psumm[[thisrun]], pos = parent.frame())
    assign(paste0("oda.data.",thisrun), dsumm[[thisrun]], pos = parent.frame())
    assign(paste0("oda.key.",thisrun), ksumm[[thisrun]], pos = parent.frame())
    assign(paste0("oda.list.",thisrun), lsumm[[thisrun]], pos = parent.frame())
    assign(paste0("oda.sda.",thisrun), sdsum[[thisrun]], pos = parent.frame())
    assign(paste0("oda.stats.",thisrun), ssumm[[thisrun]], pos = parent.frame())
  }
}

