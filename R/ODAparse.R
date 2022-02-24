#' @title Parse ODA output files.
#'
#' @description Parses model details, model predictions, and loads objects.
#'
#' @param run The numerical value of the run folder in which the data file is
#'   stored
#' @param path The working directory of the project stored as \code{path} which
#'   should be set above the level of the Runs folder
#' @param out The character name of the output file, when not specified it is
#'   assumed to be MODEL.OUT and must be the same name as was specified in
#'   \code{\link{ODArun}}
#'
#' @details When run, \code{ODAparse} will return model performance metrics and
#'   data loaded within the global environement, which can be further evaluated.
#'
#'   The path to the current project must be stored as \code{path} and the files
#'   to be loaded must be located in the \code{Run} folder.
#'
#'   Categorical ODA models are not fully supported. If a categorical model is
#'   detected, a model summary and statistics will be returned and loaded.
#'
#'   Multisample generalizablity (GEN) analyses are partially supported. If GEN
#'   models are detected, the user will be warned.
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
#'   whether a GEN, CAT, or univariate ODA model is detected, and if LOO
#'   jackknife is performed, the exact \code{oda.model} output will vary.
#'
#'   The user is referred to the ODA User guide \code{\link{ODAmanual}} and is
#'   encouraged to review the data contained within the MODEL.OUT file for more
#'   information.
#'
#' @return The following objects with the run number appended are returned for
#'   non categorical ODA models: \item{oda.data}{Data frame based on the
#'   data.csv file from specified \code{run} folder.} \item{oda.key}{Data frame
#'   containing 2 columns: the variable names from the \code{oda.data} and an
#'   ODA friendly alias e.g., v1 v2 for each attribute variable included in the
#'   ODA model.} \item{oda.list}{Data frame containing the confusion matrix for
#'   each ODA model contained in the model.out file. This output can be used to
#'   evaluate the reproducablity of the model results using a Novometric
#'   bootstrap analysis, see \code{\link{NOVOboot}}.} \item{oda.model}{Data
#'   frame containing the parsed model output and the ESS, ESP, and significance
#'   for ODA models.} \item{oda.perf}{Data frame containing the overall
#'   accuracy, sensitivity, specificity, positive predictive value, and negative
#'   predictive value for each attribute included in the ODA model. Univariate
#'   OR and 95\% CI are also presented, but caution is needed with observed cell
#'   counts of zero.} \item{oda.stats}{Data frame containing the classification
#'   summary for each ODA model contained in the model.out file.}
#'   \item{oda.sda}{Data frame containing predictions based on the model
#'   attributes and observed classifications. Correct and incorrect
#'   classifications, e.g., False positive and false negative status, are
#'   captured for each observation in the dataset. Structural decomposition can
#'   be completed using these data in subsequent ODA models.}
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
ODAparse <- function(run="",path=getwd(),out="") {
  if (out=="") {
    cat("Message: The default output file name of MODEL.OUT was applied.\n")
    out <- "MODEL.OUT"
  }
  ####Extract the ODA models, variables, and class summaries from the OUT file####
  rundir <- paste(path,"Runs",run,sep="/")
  runfile <- paste(rundir,list.files(rundir,pattern="*.csv"),sep="/")
  outfile <- paste(path,"Runs",run,out,sep="/")
  if(!file.exists(outfile)){
    stop(cat("Error: No valid OUT file found in the directory ~/Run",run,".\n"))
  }
  file <- readLines(outfile)
  data_raw <- data.frame(file,stringsAsFactors = FALSE)
  index1 <- grep("^*ODA model:",file) + 2
  index2 <- grep("^*Monte Carlo summary",file) - 3
  index3 <- grep("^*Summary for Class",file)
  index4 <- grep("^*Summary for Class",file)+12
  index5 <- grep("^*No solution found for this problem",file)-10
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
  loo.check <- grep("^LOO OFF;",file)
  loo.cat.check <- grep("^Warning  7",file)
  ####Warning if one or more attributes had no solutions found.####
  if(length(index5) > 0){
    cat("Warning: One or more attributes had no solutions found. Review the oda.model object for more information.\n")
  }
  ####CAT ODA Model: Warning if categorical model denoted where indicies differ by > 1 line.####
  if (max(index2-index1)>1 & length(index1)==length(index2)){
    cat("Warning: A categorical model was detected. The MODEL.OUT results have been truncated.\n")
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
    if(length(loo.check)>0 | length(loo.cat.check)>0){
      e.start <- grep("^Classification performance summary:", file)
      ess <- data_raw[e.start-8,]
      ess <- strsplit(ess, "      ")
      ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
      ess <- gsub("Effect strength PAC ","",ess)
      ess <- gsub("Effect strength PV ","",ess)
      ess <- ess[-seq_along(nrow(ess)),]
      esp <- ess[2]
      ess <- ess[1]
      c.lab <- as.vector(c("Overall ESS(%)","Overall ESP(%)"))
      c.model <- setNames(data.frame(paste0(n.model),models, classes, ess,esp,p.val),
                          c("Model","Attributes", "Predictions",c.lab,"MC P-value"))
    }else{
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
      fp.start <- grep("^Fisher's exact test (directional)",file)
      fp.val <- data_raw[fp.start,]
    if(length(fp.val)>0){
      fp.val <- strsplit(fp.val, "=")
      fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
      fp.val <- fp.val[,2]
      c.lab <- as.vector(c("Overall ESS(%)","Overall ESP(%)","MC P-value","LOO ESS(%)","LOO ESP(%)","LOO P-value"))
      c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, p.val, ess.loo, fp.val),
                          c("Model","Attributes", "Predictions",c.lab))
      }else{
        c.lab <- as.vector(c("Overall ESS(%)","Overall ESP(%)","MC P-value","LOO ESS(%)","LOO ESP(%)"))
        c.model <- setNames(data.frame(paste0(n.model), models, classes, ess, p.val, ess.loo),
                            c("Model","Attributes", "Predictions",c.lab))
        }
      }
    assign(paste0("oda.model.",run), c.model, pos = parent.frame())
    stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
    for(i in n.model){
      stats[, c(paste0("model.",n.model[i]))] <- cbind(data_raw[(index3[i]:index4[i]),])
    }
    assign(paste0("oda.stats.",run), stats[-13,], pos = parent.frame())
    cat("Categorical ODA Model summary:\n")
    return(c.model)
  }
  ####GEN ODA Model: Warning: A multisample generalizability model was detected. The MODEL.OUT results have been truncated####
  if(length(index1)<length(index2)){
    cat("Warning: A multisample generalizability model was detected. The MODEL.OUT results have been truncated.\n")
    gen.val <- grep("^Results for group",file)
    gen.val <- data_raw[gen.val,]
    gen.val <- gsub("Results for group ","",gen.val)
    gen.val <- unique(gen.val)
    Left <- data_raw[index1,]
    lh <- gsub(".*IF\\s*|THEN.*","",Left)
    lh <- gsub("D","E",lh)
    lc <- gsub(".*THEN *","",Left)
    Right <- data_raw[index2,]
    rh <- gsub(".*IF\\s*|THEN.*","",Right)
    rh <- gsub("----------------------","",rh)
    rh <- list(rh)
    rh <- as.vector(lapply(rh, function(x) x[!x %in% ""]))
    rh <- unlist(rh)
    rc <- gsub(".*THEN *","",Right)
    rc <- gsub("----------------------","",rc)
    rc <- list(rc)
    rc <- as.vector(lapply(rc, function(x) x[!x %in% ""]))
    rc <- unlist(rc)
    p.start <- grep("^Estimated p:",file)
    p.val <- data_raw[p.start,]
    p.val <- p.val[seq(1,length(p.val),3)]
    p.val <- stringr::str_extract(string = p.val, pattern = "[\\d+]?\\.*\\d*$")
    if(length(loo.check)>0 | length(loo.cat.check)>0){
      m.pac.start <- grep("^Mean PAC across groups",file)
      m.pac <- data_raw[m.pac.start,]
      m.pac <- strsplit(m.pac, "=")
      m.pac <- matrix(unlist(m.pac), ncol=2, byrow=TRUE)
      m.pac <- gsub("Mean PAC across groups ","",m.pac)
      m.pac <- list(m.pac)
      m.pac <- as.data.frame(lapply(m.pac, function(x) x[!x %in% ""]))
      e.start <- grep("l      -----------------", file)
      ess <- data_raw[e.start+4,]
      ess <- strsplit(ess, "      ")
      ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
      ess <- gsub("Effect strength PAC ","",ess)
      ess <- gsub("Effect strength PV ","",ess)
      gen.lab <- as.vector(c("ESS (%)","ESP(%)"))
      gen.model <- setNames(data.frame(paste0(n.model),lh, lc, rh, rc, m.pac, ess, p.val),
                            c("Model","IF ","THEN","IF ","THEN","Mean PAC",gen.lab,"MC P-value"))
    }else{
      m.pac.start <- grep("^Mean PAC across groups",file)
      m.pac <- data_raw[m.pac.start,]
      m.pac <- strsplit(m.pac, "=")
      m.pac <- matrix(unlist(m.pac), ncol=2, byrow=TRUE)
      m.pac <- gsub("Mean PAC across groups ","",m.pac)
      m.pac <- list(m.pac)
      m.pac <- as.data.frame(lapply(m.pac, function(x) x[!x %in% ""]))
      m.pac <- as.vector(m.pac[seq(1,length(attribute)*2,2),])
      ess.gen.start <- grep("^Results for group",file)
      ess.gen <- data_raw[ess.gen.start-4,]
      ess.gen <- strsplit(ess.gen, "      ")
      ess.gen <- matrix(unlist(ess.gen), ncol=2*length(gen.val), byrow=TRUE)
      ess.gen <- gsub("Effect strength PAC ","",ess.gen)
      ess.gen <- gsub("Effect strength PV ","",ess.gen)
      ess.gen <- as.data.frame(ess.gen)
      ess.gen <- ess.gen[1:2]
      ess.all <- ess.gen[seq(1,nrow(ess.gen),(length(gen.val))),]
      ess.loo <- ess.gen[seq(2,nrow(ess.gen),(length(gen.val))),]
      fp.start <- grep("^Fisher's exact test",file)
      fp.val <- data_raw[fp.start,]
      fp.val <- strsplit(fp.val, "=")
      fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
      fp.val <- fp.val[,2]
      fp.val <- fp.val[seq(1,length(fp.val),(length(gen.val)+1))]
      gen.lab <- as.vector(c("ESS (%)","ESP(%)","LOO ESS (%)","LOO ESP (%)"))
      gen.model <- setNames(data.frame(paste0(n.model),lh, lc, rh, rc, m.pac, p.val, ess.all, ess.loo, fp.val),
                            c("Model","IF ","THEN","IF ","THEN","PAC","MC P-value",gen.lab,"LOO P-value"))
    }

    ###Extract ODA Model Summary###
    assign(paste0("oda.model.",run), gen.model, pos = parent.frame())
    lc.pred <- as.numeric(gsub(".*=","",lc))
    rc.pred <- as.numeric(gsub(".*=","",rc))
    cut <- as.numeric(gsub(".*=","",lh))
    data <- read.csv(runfile)
    header <- names(data)
    c.header <- seq_len(ncol(data))
    key <- data.frame(header, paste("v",c.header,sep=""))
    names(data) <- key[,2]
    cv <- data[unique(class)]
    data <- data[attribute]

    stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
    for(i in n.model){
      stats[, c(paste0("model.",n.model[i]))] <- cbind(data_raw[(index3[i]:index4[i]),])
    }
    ### Extract ODA Performance Stats###
    assign(paste0("oda.stats.",run), stats[-13,], pos = parent.frame())

    df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
    for(i in n.model){
      df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[i], rc.pred[i], lc.pred[i]))
    }
    predictions <- df
    df_list <- lapply(df, function(x){
      dfs <- list()
      for(j in seq_along(cv))
      {
        dfs[[j]] <- data.frame(ifelse(x == 1 & cv[j] == 1, 1, 0),
                               ifelse(x == 0 & cv[j] == 0, 1, 0),
                               ifelse(x == 1 & cv[j] == 0, 1, 0),
                               ifelse(x == 0 & cv[j] == 1, 1, 0))
      }
      setNames(do.call("cbind", dfs),
               paste0(c("tp.", "tn.", "fp.", "fn."), rep(seq_along(cv), each = 4)))
    })
    df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
    for(i in n.model){
      df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[i], rc.pred[i], lc.pred[i]))
    }
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
    assign(paste0("oda.list.",run), cc_list, pos = parent.frame())

    temp <- lapply(df_list,sapply,sum, na.rm=T)

    temp2 <- as.data.frame(do.call(rbind, temp))

    acc.n <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+1)])))

    acc.d <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+3)])))

    tp <- temp2[,seq(1, ncol(temp2), 4)]        # TP
    tn <- temp2[,seq(2, ncol(temp2), 4)]        # TN
    fp <- temp2[,seq(3, ncol(temp2), 4)]        # FP
    fn <- temp2[,seq(4, ncol(temp2), 4)]        # FN

    pac <- round(acc.n/acc.d*100,digits=1)      # Percent accuracy in classfication
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

    OR <- round((tp*tn)/(fp*fn), digits=2)      # OR
    names(OR) <- paste0(rep("OR.",length(cv)), seq_along(cv))

    SE.OR <- sqrt((1/tp+1/fp+1/fn+1/tn))        #Parametric SE for OR

    LL.OR <- round(OR * exp(-1.96 * SE.OR), digits=2)        #95% CI upper limit
    names(LL.OR) <- paste0(rep("LL.OR.",length(cv)), seq_along(cv))

    UL.OR <- round(OR * exp(1.96 * SE.OR), digits=2)         #95% CI lower limit
    names(UL.OR) <- paste0(rep("UL.OR.",length(cv)), seq_along(cv))

    perf <- cbind(pac,mpac,sens,spec,ess.m,mpv,ppv,npv,esp.m,OR,LL.OR,UL.OR)

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
    perf_list <- as.data.frame(do.call("cbind",c(p.list,perf.list)))
    assign(paste0("oda.perf.",run), perf_list, pos = parent.frame())
    #Regenerate data key with new variables
    a.key <- as.matrix(key[attribute,1])
    c.key <- as.matrix(key[class,1])
    oda.key <- data.frame(a.key,names(data),c.key,names(cv))
    names(oda.key) <- c("Attribute Label","Attribute","Class Label","Class")
    assign(paste0("oda.key.",run), oda.key, pos = parent.frame())
    #Clean up and add back the class variable called class
    data$class <- cv[,1]
    data <- cbind(data,predictions)
    assign(paste0("oda.data.",run), data, pos = parent.frame())
    #Return model summary
    cat("ODA Model summary:\n")
    return(gen.model)
  }
  ####UNI ODA Model: No warnings because this is the standard model format####
  else{
    Left <- data_raw[index1,]
    lh <- gsub(".*IF\\s*|THEN.*","",Left)
    lh <- gsub("D","E",lh)
    lc <- gsub(".*THEN *","",Left)
    Right <- data_raw[index2,]
    rh <- gsub(".*IF\\s*|THEN.*","",Right)
    rc <- gsub(".*THEN *","",Right)
    p.start <- grep("^Estimated p:",file)
    p.val <- data_raw[p.start,]
    p.val <- stringr::str_extract(string = p.val, pattern = "[\\d+]?\\.*\\d*$")
    if(length(loo.check)>0 | length(loo.cat.check)>0){
      e.start <- grep("l      -----------------", file)
      ess <- data_raw[e.start+4,]
      ess <- strsplit(ess, "      ")
      ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
      ess <- gsub("Effect strength PAC ","",ess)
      ess <- gsub("Effect strength PV ","",ess)
      lab <- as.vector(c("Overall ESS(%)","Overall ESP(%)"))
      model <- setNames(data.frame(paste0(n.model),lh, lc, rh, rc, ess,p.val),
                        c("Model","IF ","THEN","IF ","THEN",lab,"MC P-value"))
    }else{
      e.start <- grep("^Results of leave-one-out analysis", file)
      ess <- data_raw[e.start-4,]
      ess <- strsplit(ess, "      ")
      ess <- matrix(unlist(ess), ncol=2, byrow=TRUE)
      ess <- gsub("Effect strength PAC ","",ess)
      ess <- gsub("Effect strength PV ","",ess)
      ess.loo <- data_raw[e.start+25,]
      ess.loo <- strsplit(ess.loo, "      ")
      ess.loo <- matrix(unlist(ess.loo), ncol=2, byrow=TRUE)
      ess.loo <- gsub("Effect strength PAC ","",ess.loo)
      ess.loo <- gsub("Effect strength PV ","",ess.loo)
      fp.start <- grep("^Fisher's exact test",file)
      fp.val <- data_raw[fp.start,]
      fp.val <- strsplit(fp.val, "=")
      fp.val <- matrix(unlist(fp.val), ncol=2, byrow=TRUE)
      fp.val <- fp.val[,2]
      lab <- as.vector(c("Overall ESS(%)","Overall ESP(%)","MC P-value","LOO ESS(%)","LOO ESP(%)","LOO P-value"))
      model <- setNames(data.frame(paste0(n.model),lh, lc, rh, rc, ess,p.val,ess.loo,fp.val),
                        c("Model","IF ","THEN","IF ","THEN",lab))
    }
    ###Extract ODA Model Summary###
    assign(paste0("oda.model.",run), model, pos = parent.frame())
    lc.pred <- as.numeric(gsub(".*=","",lc))
    rc.pred <- as.numeric(gsub(".*=","",rc))
    cut <- as.numeric(gsub(".*=","",lh))
    data <- read.csv(runfile)
    header <- names(data)
    c.header <- seq_len(ncol(data))
    key <- data.frame(header, paste("v",c.header,sep=""))
    names(data) <- key[,2]
    cv <- data[unique(class)]
    data <- data[attribute]

    stats <- as.data.frame(matrix(0, ncol = 0, nrow = 13))
    for(i in n.model){
      stats[, c(paste0("model.",n.model[i]))] <- cbind(data_raw[(index3[i]:index4[i]),])
    }
    ### Extract ODA Performance Stats###
    assign(paste0("oda.stats.",run), stats[-13,], pos = parent.frame())
    df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
    for(i in n.model){
      df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[i], rc.pred[i], lc.pred[i]))
    }
    predictions <- df
    df_list <- lapply(df, function(x){
      dfs <- list()
      for(j in seq_along(cv))
      {
        dfs[[j]] <- data.frame(ifelse(x == 1 & cv[j] == 1, 1, 0),
                               ifelse(x == 0 & cv[j] == 0, 1, 0),
                               ifelse(x == 1 & cv[j] == 0, 1, 0),
                               ifelse(x == 0 & cv[j] == 1, 1, 0))
      }
      setNames(do.call("cbind", dfs),
               paste0(c("tp.", "tn.", "fp.", "fn."), rep(seq_along(cv), each = 4)))
    })
    #Extract the classification for each model and store for SDA
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
    assign(paste0("oda.sda.",run), sda.frame, pos = parent.frame())
    df <- as.data.frame(matrix(0, ncol = 0, nrow = nrow(data)))
    for(i in n.model){
      df[, c(paste0("pv.",n.model[i]))] <- as.vector(ifelse(data[i] > cut[i], rc.pred[i], lc.pred[i]))
    }
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
    assign(paste0("oda.list.",run), cc_list, pos = parent.frame())

    temp <- lapply(df_list,sapply,sum,na.rm=T)

    temp2 <- as.data.frame(do.call(rbind, temp))

    acc.n <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+1)])))

    acc.d <- as.data.frame(sapply(seq(1,length(temp2),by=4),function(i) rowSums(temp2[,i:(i+3)])))

    tp <- temp2[,seq(1, ncol(temp2), 4)]        # TP
    tn <- temp2[,seq(2, ncol(temp2), 4)]        # TN
    fp <- temp2[,seq(3, ncol(temp2), 4)]        # FP
    fn <- temp2[,seq(4, ncol(temp2), 4)]        # FN

    pac <- round(acc.n/acc.d*100,digits=1)      # Percent accuracy in classfication
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

    OR <- round((tp*tn)/(fp*fn), digits=2)      # OR
    names(OR) <- paste0(rep("OR.",length(cv)), seq_along(cv))

    SE.OR <- sqrt((1/tp+1/fp+1/fn+1/tn))        #Parametric SE for OR

    LL.OR <- round(OR * exp(-1.96 * SE.OR), digits=2)        #95% CI upper limit
    names(LL.OR) <- paste0(rep("LL.OR.",length(cv)), seq_along(cv))

    UL.OR <- round(OR * exp(1.96 * SE.OR), digits=2)         #95% CI lower limit
    names(UL.OR) <- paste0(rep("UL.OR.",length(cv)), seq_along(cv))

    perf <- cbind(pac,mpac,sens,spec,ess.m,mpv,ppv,npv,esp.m,OR,LL.OR,UL.OR)

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
    perf_list <- as.data.frame(do.call("cbind",c(p.list,perf.list)))
    assign(paste0("oda.perf.",run), perf_list, pos = parent.frame())
    #Regenerate data key with new variables
    a.key <- as.matrix(key[attribute,1])
    c.key <- as.matrix(key[class,1])
    oda.key <- data.frame(a.key,names(data),c.key,names(cv))
    names(oda.key) <- c("Attribute Label","Attribute","Class Label","Class")
    assign(paste0("oda.key.",run), oda.key, pos = parent.frame())
    #Clean up and add back the class variable called class
    data$class <- cv[,1]
    data <- cbind(data,predictions)
    assign(paste0("oda.data.",run), data, pos = parent.frame())
    #Return Model summary
    cat("ODA Model summary:\n")
    return(model)
  }
}
