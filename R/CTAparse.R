#' @title Parse CTA output files.
#'
#' @description Parses CTA model details, model predictions, and loads objects.
#'
#' @param run The numeric value of the folder where the model output is saved
#' @param mod The numeric value of the model number. Additional models can be
#'   specified in '...'
#' @param type A character string containing one of the following tree model
#'   solution types: 'Unpruned','Pruned','Enumerated'. The default value is
#'   'Enumerated'. See \code{\link{CTArun}}
#' @param weight A logical value specifying whether the models being processed
#'   by \code{CTAparse} are weighted tree models.
#' @param ... Additional model numbers specified as a list
#'
#' @details When executed, \code{CTAparse} will return model performance metrics
#'   and data loaded within the global environment, which can be further
#'   evaluated.
#'
#'   The working directory must be directed toward the project, the input files
#'   must be located within the input folder, and model output files must be
#'   located within the output folder.
#'
#'   For each CTA model, \code{CTAparse} will return the \strong{Effect Strength
#'   for Sensitivity} or ESS.
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
#'   Unlike mean PAC, ESS is normed against chance (Yarnold \emph{et al}. 2005
#'   and 2016) providing an intuitive scaling of model accuracy within the ODA
#'   paradigm.
#'
#'   The significance values reported are from 1. Monte Carlo simulations for
#'   all observations and 2. Fisher's Exact tests for LOO analysis.
#'
#'   The user is referred to the ODA User guide \code{\link{ODAmanual}} and is
#'   encouraged to review the data contained within the MODEL.OUT file for more
#'   information.
#'
#' @return The following objects with the run number appended are returned for
#'   CTA models: \item{cta.denom}{Data frame containing the denominator values
#'   for the models contained within each output file in the \code{run} folder.}
#'   \item{cta.list}{Data frame containing the confusion matrix for each CTA
#'   model contained in the output file. This output can be used to evaluate the
#'   reproducibility of the model results using a Novometric bootstrap analysis,
#'   see \code{\link{NOVOboot}}.} \item{cta.model}{Data frame containing the
#'   parsed model output and significance for CTA models.} \item{cta.sum}{Data
#'   frame containing the overall accuracy, sensitivity, specificity, positive
#'   predictive value, and negative predictive value for each CTA model.}
#'
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @importFrom stringr str_extract
#' @importFrom tidyr separate
#' @importFrom utils read.csv
#'
#' @export
#'
#' @author Nathaniel J. Rhodes
#'
#' @seealso \code{\link{ODAmanual}} \code{\link{ODAclean}} \code{\link{ODAload}}
#'   \code{\link{CTArun}} \code{\link{NOVOboot}}
#'
#' @examples
#' # Not run:
#' # CTAparse(run=1, mod=1, type="Enumerated",weight=F)
#' # CTAparse(1,1)
#'
#' @references Yarnold P.R. and Soltysik R.C. (2005). \emph{Optimal data
#'   analysis: Guidebook with software for Windows}. APA Books.
#'
#'   Yarnold, P.R. and Soltysik, R.C. (2016). \emph{Maximizing Predictive
#'   Accuracy}. ODA Books. DOI: 10.13140/RG.2.1.1368.3286.
#'
#'
CTAparse <- function(run="",mod="",type="",weight="",...) {
  if(run==""){
    stop(
      print("Message: User must specify a single folder to parse.\n")
    )
  }
  if(mod==""){
    stop(
      print("Message: User must specify the model number from CTArun.\n")
    )
  }
  if (type == ""){
    type <- "Enumerated"
  }

  if (type != "" & (!type %in% list("Unpruned","Pruned","Enumerated"))) {
    stop(cat("Message: A model output type of 'Enumerated' 'Pruned' or 'Unpruned' must be entered.\n"))
  }

  type.list <- c("Unpruned","Pruned","Enumerated")

  m.type <- which(type == type.list)[[1]]

  if (weight == "T" | weight == "t" | weight == "TRUE"){
    weight <- TRUE
  } else{weight <- FALSE}

  addlmods <- list(...)

  if (length(addlmods) > 0) {
    allmods <- c(mod, unlist(addlmods))
  }else {
    allmods <- mod
  }

  mosumm <- list()
  csumm <- list()
  dsumm <- list()
  lsumm <- list()
  misumm <- list()

  for (thismod in allmods){

  #### Extract the CTA models, variables, and class summaries from the output file ####
  out <- paste0("MODEL",thismod,".TXT")
  rundir <- paste(getwd(),"CTA",run,"inputs",sep="/")
  runfile <- paste(rundir,list.files(rundir,pattern="*.csv"),sep="/")
  outfile <- paste(getwd(),"CTA",run,"outputs",out,sep="/")
  if(!file.exists(outfile)){
    stop(cat("Error: No valid output file found in CTA model",run,"directory .\n"))
  }
  file <- readLines(outfile)
  data_raw <- data.frame(file,stringsAsFactors = FALSE)
  index1 <- grep(paste0("^*",type),file)

  if(weight == TRUE){
    index2 <- grep("^*OVERALL ESS",file) - 3 # End of readout for each weighted model
    index3 <- grep("^*OVERALL ESS",file)     # Overall ESS for each model
    index4 <- grep("^*WEIGHTED ESS",file)    # Weighted ESS for each model
  } else{
    index2 <- grep("^*OVERALL ESS",file) - 2  # End of readout for each unweighted model
    index3 <- grep("^*OVERALL ESS",file)      # Overall ESS for each model
  }
  ## Print error message if all tree models have no solution and stop ##
  tryCatch({
    if((length(index2) | length(index3)) == 0 ){
      stop(cat("Error: no tree solutions found for any model(s)"))
    }
  },error=function(e){})

  index5 <- grep("^* No tree found.",file) - 2  # Class for which no model is found

  mindenom <- grep("MINDENOM",file)
  mindenom <- data_raw[mindenom,]
  mindenom <- gsub("MINDENOM ","",mindenom)
  mindenom <- gsub(";","",mindenom)
  mindenom <- as.numeric(as.character(mindenom))

  vars <- grep("*Class",file)
  vars <- vars[!vars %in% index5]
  vars <- data_raw[vars,]
  dt <- as.data.frame(vars)
  dt <- dt %>% separate(vars, c("Result", "Class"), "-")
  dt$Class <- gsub("Class ","",dt$Class)
  dt$Result <- gsub(" Tree Results ","",dt$Result)

  n.model <- seq_along(vars)
  n.model <- n.model[!vars %in% index5]

  ess <- data_raw[index3,]
  ess <- as.data.frame(ess)
  ess <- ess %>% separate(ess, c("Text", "Overall ESS"), "=")
  ess <- ess[-1]
  # remove the percentage sign and convert to numeric
  ess$`Overall ESS` <- as.numeric(sub("%", "", ess$`Overall ESS`))

  #### Extract and load model elements for weighted CTA models ####
  if(weight==TRUE){
    wess <- data_raw[index4,]
    wess <- as.data.frame(wess)
    wess <- wess %>% separate(wess, c("Text", "Weighted ESS"), "=")
    wess <- wess[-1]
    # remove the percentage sign and convert to numeric
    wess$`Weighted ESS` <- as.numeric(sub("%", "", wess$`Weighted ESS`))

    ### Extract List of CTA cross class tables ###
    index6 <- grep("^*        -----   -----",file)
    index6 <- index6[seq(1, length(index6)-1, by = 2)]-1
    index7 <- index6+6

    p.mat <- c()
    p.stat <- c()
    for(i in n.model){
      p.mat[[i]] <- cbind(data_raw[(index6[i]:index7[i]),])
    }

    p.mat[] <- lapply(p.mat, function(x){x[c(1,2,5)] <- ""
    return(x)})
    p.mat[] <- lapply(p.mat, gsub, pattern = "|", replacement = " ", fixed = TRUE)
    p.mat[] <- lapply(p.mat, gsub, pattern = "\\s+", replacement = " ")
    p.mat[] <- lapply(p.mat, function(z){ z[!is.na(z) & z != ""]})
    p.stat  <- lapply(p.mat,"[",3:4,drop=FALSE)
    p.stat[]<- lapply(p.stat, strsplit, split = " ")
    p.mat[] <- lapply(p.mat,"[",c(1:2),drop=FALSE)
    p.mat[] <- lapply(p.mat, strsplit, split = " ")
    p.stat[]<- lapply(p.stat, function(z){ z[!is.na(z) & z != ""]})


    s.list <- lapply(p.stat,as.data.frame)
    s.list <- lapply(s.list,"[",2:3,,drop=FALSE)
    s.list <- lapply(s.list,function(x) {colnames(x) <- c("PV","Weighted PV"); x})
    s.list <- lapply(s.list,function(x) {rownames(x) <- c(1,2); x})

    p.list <- lapply(p.mat,as.data.frame)
    p.list <- lapply(p.list,t)
    p.list <- lapply(p.list,as.data.frame)
    p.list <- lapply(p.list,"[",,c(2:6),drop=FALSE)
    p.list <- lapply(p.list,function(x) {rownames(x) <- c(1,2); x})
    p.class <- lapply(p.list,"[",,c(1),drop=FALSE)
    p.label <- as.numeric(unlist(p.class[[1]]))
    p.label <- c("Class",
                 paste0("Pred ",p.label[1]),
                 paste0("Pred ",p.label[2]),
                 "Sens",
                 "Weighted Sens")
    p.list <- lapply(p.list,function(x) {colnames(x) <- p.label; x})

    c.names <- as.numeric(unlist(p.class[[1]]))

    s.list <- lapply(s.list,function(x) within(x,"Class" <- c.names))

    c.list <- lapply(p.list,"[",,c(2:3),drop=FALSE)
    c.list <- lapply(c.list,function(x) {colnames(x) <- c.names; x})
    c.list <- lapply(c.list,function(x) {rownames(x) <- c.names; x})
    stat.list <- do.call(rbind, Map(merge,p.list,s.list,by="Class"))
    stat.list <- split(stat.list,rep(1:length(n.model),each=2))
    stat_num <- seq(m.type, length(n.model), m.type)
    cta.list <- stat.list[stat_num]
    names(cta.list) <- paste0("model.", stat_num)
    cta.list <- lapply(cta.list,function(x) {rownames(x) <- NULL; x})
    lsumm[[thismod]] <- cta.list

    ### Extract CTA Models and Performance Metrics ###
    index8 <- grep("^*     ATTRIBUTE",file)+2
    index9 <- index6-2

    mods <- list()
    for(i in seq_along(n.model)){
      mods[[i]] <- cbind(data_raw[(index8[i]:index9[i]),])
    }

    mods[] <- lapply(mods, gsub, pattern = "*\\s+", replacement = " ")
    mods[] <- lapply(mods, gsub, pattern = " V", replacement = "V", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = ";", replacement = "", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = "-->", replacement = " ", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = ",", replacement = " ", fixed = TRUE)
    mods[] <- lapply(mods, strsplit, split = " ")

    mod.list <- lapply(mods,as.data.frame)
    mod.list <- lapply(mod.list,t)
    mod.list <- lapply(mod.list,as.data.frame)
    mod.list <- lapply(mod.list,function(x) {rownames(x) <- NULL; x})

    mod_names <- c("ATTRIBUTE","NODE","LEVEL","OBS",
                   "P","WESS","LOO","WESSL","LOOp","TYPE",
                   "IF","THEN","N1","PERCENT1",
                   "IF","THEN","N2","PERCENT2"
    )
    mod.list <- lapply(mod.list,function(x) {colnames(x) <- mod_names; x})
    mod_num <- seq(m.type, length(n.model), m.type)
    cta.model <- mod.list[mod_num]
    names(cta.model) <- paste0("model.", mod_num)
    mosumm[[thismod]] <- cta.model

    ### Extract denominator for MDSA method ###
    denom.list <- list()
    for(i in seq_along(cta.model)) {
      # extract data.frame from the list
      df <- cta.model[[i]]
      # check if 'N1' and 'N2' columns are present in the data.frame
      if('N1' %in% names(df) && 'N2' %in% names(df)) {
        # iterate over each row of the data.frame
        for(j in 1:nrow(df)) {
          # split the 'N1' column values on "/"
          n1_split <- strsplit(as.character(df$N1[j]), "/")[[1]]
          # keep the left-hand side number
          df$N1[j] <- n1_split[2]
          # split the 'N2' column values on "/"
          n2_split <- strsplit(as.character(df$N2[j]), "/")[[1]]
          # keep the left-hand side number
          df$N2[j] <- n2_split[2]
        }
        # pull the modified data.frame back to the list
        denom.list[[i]] <- data.frame(as.numeric(df$N1[j]), as.numeric(df$N2[j]))
      }
    }

    ### Identify the minimum denominator across models within a run ###
    # create a vector to store the minimum values from each data.frame
    min_values <- c()
    # create a vector to store the indices with the smallest minimum value
    min_indices <- c()

    for(i in seq_along(denom.list)) {
      # extract data.frame from the list
      df <- denom.list[[i]]
      # find the minimum value in the data.frame
      min_value <- min(as.numeric(df))
      # add the minimum value to the 'min_values' vector
      min_values <- c(min_values, min_value)
      # check if the current minimum value is smaller than the previous minimum value
      if(length(min_indices) == 0 || min_value < min_values[min_indices]) {
        # if yes, update the 'min_indices' variable to the current index
        min_indices <- c(i)
      } else if (all(min_value == min_values[min_indices])) {
        # if tie, add the current index to the 'min_indices' variable
        min_indices <- c(min_indices, i)
      }
    }

    d.list <- data.frame(tree.num=mod_num,run=rep(run,length(mod_num)),min.denom=min_values)
    dsumm[[thismod]] <- d.list

    ### Extract predictive values for the weighted models ###
    m.strata <- c()
    for(i in seq_along(mod.list)){
      m.strata[[i]] <- length(grep("\\*",unlist(mod.list[i][[1]])))
    }

    m.strata <- unlist(m.strata)

    df <- c()
    for(i in seq_along(c.list)){
      df[[i]] <- matrix(as.numeric(as.character(unlist(c.list[[i]]))), nrow=2, ncol = 2, byrow = F)
    }

    tn <- sapply(df,'[[',1)

    fn <- sapply(df,'[[',2)

    fp <- sapply(df,'[[',3)

    tp <- sapply(df,'[[',4)

    m.sens <- tp/(tp+fn)

    m.spec <- tn/(tn+fp)

    m.ppv <- tp/(tp+fp)

    m.npv <- tn/(tn+fn)

    m.acc <- (m.sens+m.spec)/2

    m.ess <- (m.acc-0.5)/0.5

    m.ess <- round(m.ess*100,2)

    m.d <- round(100/(m.ess/m.strata)-m.strata,2)

    n.class1 <- tp+fn

    n.class0 <- tn+fp

    cta.sum <- setNames(
      data.frame(n.model,
                 rep(mindenom,each=length(n.model)),
                 n.class0,
                 n.class1,
                 round(100*m.sens,2),
                 round(100*m.spec,2),
                 round(100*m.ppv,2),
                 round(100*m.npv,2),
                 round(100*m.acc,2),
                 m.ess,
                 m.strata,
                 m.d),
      c("tree.num",
        "mindenom",
        paste0("n.class.",c.names[1]),
        paste0("n.class.",c.names[2]),
        "Sensitivity",
        "Specificiy",
        "PPV",
        "NPV",
        "Mean PAC",
        "ESS",
        "Strata",
        "D"))
    cta.sum <- data.frame(dt,cta.sum)
    cta.sum <- cta.sum[,c(3,1,2,4,5,6,7,8,9,10,11,12,13,14)]

  } else{
    #### Extract and load model elements for un-weighted CTA models####

    ### Extract List of CTA cross class tables ###
    index6 <- grep("^*        -----   -----",file)
    index6 <- index6[seq(1, length(index6)-1, by = 2)]-1
    index7 <- index6+5

    p.mat <- c()
    p.stat <- c()
    for(i in n.model){
      p.mat[[i]] <- cbind(data_raw[(index6[i]:index7[i]),])
    }

    p.mat[] <- lapply(p.mat, function(x){x[c(1,2,5)] <- ""
    return(x)})
    p.mat[] <- lapply(p.mat, gsub, pattern = "|", replacement = " ", fixed = TRUE)
    p.mat[] <- lapply(p.mat, gsub, pattern = "\\s+", replacement = " ")
    p.mat[] <- lapply(p.mat, function(z){ z[!is.na(z) & z != ""]})
    p.stat  <- lapply(p.mat,"[",3,drop=FALSE)
    p.stat[]<- lapply(p.stat, strsplit, split = " ")
    p.mat[] <- lapply(p.mat,"[",c(1:2),drop=FALSE)
    p.mat[] <- lapply(p.mat, strsplit, split = " ")
    p.stat[]<- lapply(p.stat, function(z){ z[!is.na(z) & z != ""]})

    s.list <- lapply(p.stat,as.data.frame)
    s.list <- lapply(s.list,"[",2:3,,drop=FALSE)
    s.list <- lapply(s.list,function(x) {colnames(x) <- c("PV"); x})
    s.list <- lapply(s.list,function(x) {rownames(x) <- c(1,2); x})


    p.list <- lapply(p.mat,as.data.frame)
    p.list <- lapply(p.list,t)
    p.list <- lapply(p.list,as.data.frame)
    p.list <- lapply(p.list,"[",,c(2:5),drop=FALSE)
    p.list <- lapply(p.list,function(x) {rownames(x) <- c(1,2); x})
    p.class <- lapply(p.list,"[",,c(1),drop=FALSE)
    p.label <- as.numeric(unlist(p.class[[1]]))
    p.label <- c("Class",
                 paste0("Pred ",p.label[1]),
                 paste0("Pred ",p.label[2]),
                 "Sens")
    p.list <- lapply(p.list,function(x) {colnames(x) <- p.label; x})

    c.names <- as.numeric(unlist(p.class[[1]]))

    s.list <- lapply(s.list,function(x) within(x,"Class" <- c.names))

    c.list <- lapply(p.list,"[",,c(2:3),drop=FALSE)
    c.list <- lapply(c.list,function(x) {colnames(x) <- c.names; x})
    c.list <- lapply(c.list,function(x) {rownames(x) <- c.names; x})
    stat.list <- do.call(rbind, Map(merge,p.list,s.list,by="Class"))
    stat.list <- split(stat.list,rep(1:length(n.model),each=2))
    stat_num <- seq(m.type, length(n.model), m.type)
    cta.list <- stat.list[stat_num]
    names(cta.list) <- paste0("model.", stat_num)
    cta.list <- lapply(cta.list,function(x) {rownames(x) <- NULL; x})
    lsumm[[thismod]] <- cta.list


    ### Extract CTA Models and Performance Metrics ###
    index8 <- grep("^*     ATTRIBUTE",file)+2
    index9 <- index6-2

    mods <- list()
    for(i in seq_along(n.model)){
      mods[[i]] <- cbind(data_raw[(index8[i]:index9[i]),])
    }

    mods[] <- lapply(mods, gsub, pattern = "*\\s+", replacement = " ")
    mods[] <- lapply(mods, gsub, pattern = " V", replacement = "V", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = ";", replacement = "", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = "-->", replacement = " ", fixed = TRUE)
    mods[] <- lapply(mods, gsub, pattern = ",", replacement = " ", fixed = TRUE)
    mods[] <- lapply(mods, strsplit, split = " ")

    mod.list <- lapply(mods,as.data.frame)

    mod.list <- lapply(mod.list,t)

    mod.list <- lapply(mod.list,as.data.frame)

    mod.list <- lapply(mod.list,function(x) {rownames(x) <- NULL; x})

    mod_names <- c("ATTRIBUTE","NODE","LEVEL","OBS",
                   "P","ESS","LOO","ESSL","LOOp","TYPE",
                   "IF","THEN","N1","PERCENT1",
                   "IF","THEN","N2","PERCENT2"
    )
    mod.list <- lapply(mod.list,function(x) {colnames(x) <- mod_names; x})
    mod_num <- seq(m.type, length(n.model), m.type)
    cta.model <- mod.list[mod_num]
    names(cta.model) <- paste0("model.", mod_num)
    mosumm[[thismod]] <- cta.model

    ### Extract denominator for MDSA method ###
    denom.list <- list()
    for(i in seq_along(cta.model)) {
      # extract data.frame from the list
      df <- cta.model[[i]]
      # check if 'N1' and 'N2' columns are present in the data.frame
      if('N1' %in% names(df) && 'N2' %in% names(df)) {
        # iterate over each row of the data.frame
        for(j in 1:nrow(df)) {
          # split the 'N1' column values on "/"
          n1_split <- strsplit(as.character(df$N1[j]), "/")[[1]]
          # keep the left-hand side number
          df$N1[j] <- n1_split[2]
          # split the 'N2' column values on "/"
          n2_split <- strsplit(as.character(df$N2[j]), "/")[[1]]
          # keep the left-hand side number
          df$N2[j] <- n2_split[2]
        }
        # pull the modified data.frame back to the list
        denom.list[[i]] <- data.frame(as.numeric(df$N1[j]), as.numeric(df$N2[j]))
      }
    }

    ### Identify the minimum denominator across models within a run ###
    # create a vector to store the minimum values from each data.frame
    min_values <- c()
    # create a vector to store the indices with the smallest minimum value
    min_indices <- c()

    for(i in seq_along(denom.list)) {
      # extract data.frame from the list
      df <- denom.list[[i]]
      # find the minimum value in the data.frame
      min_value <- min(as.numeric(df))
      # add the minimum value to the 'min_values' vector
      min_values <- c(min_values, min_value)
      # check if the current minimum value is smaller than the previous minimum value
      if(length(min_indices) == 0 || min_value < min_values[min_indices]) {
        # if yes, update the 'min_indices' variable to the current index
        min_indices <- c(i)
      } else if (all(min_value == min_values[min_indices])) {
        # if tie, add the current index to the 'min_indices' variable
        min_indices <- c(min_indices, i)
      }
    }

    d.list <- data.frame(tree.num=mod_num,run=rep(run,length(mod_num)),min.denom=min_values)
    dsumm[[thismod]] <- d.list

    ### Extract the summary of predictive values for the models ###
    m.strata <- c()
    for(i in seq_along(mod.list)){
      m.strata[[i]] <- length(grep("\\*",unlist(mod.list[i][[1]])))
    }

    m.strata <- unlist(m.strata)

    df <- c()
    for(i in seq_along(c.list)){
      df[[i]] <- matrix(as.numeric(as.character(unlist(c.list[[i]]))), nrow=2, ncol = 2, byrow = F)
    }

    tn <- sapply(df,'[[',1)

    fn <- sapply(df,'[[',2)

    fp <- sapply(df,'[[',3)

    tp <- sapply(df,'[[',4)

    m.sens <- tp/(tp+fn)

    m.spec <- tn/(tn+fp)

    m.ppv <- tp/(tp+fp)

    m.npv <- tn/(tn+fn)

    m.acc <- (m.sens+m.spec)/2

    m.ess <- (m.acc-0.5)/0.5

    m.ess <- round(m.ess*100,2)

    m.d <- round(100/(m.ess/m.strata)-m.strata,2)

    n.class1 <- tp+fn

    n.class0 <- tn+fp

    cta.sum <- setNames(
      data.frame(n.model,
                 rep(mindenom,each=length(n.model)),
                 n.class0,
                 n.class1,
                 round(100*m.sens,2),
                 round(100*m.spec,2),
                 round(100*m.ppv,2),
                 round(100*m.npv,2),
                 round(100*m.acc,2),
                 m.ess,
                 m.strata,
                 m.d),
      c("tree.num",
        "mindenom",
        paste0("n.class.",c.names[1]),
        paste0("n.class.",c.names[2]),
        "Sensitivity",
        "Specificiy",
        "PPV",
        "NPV",
        "Mean PAC",
        "ESS",
        "Strata",
        "D"))
    cta.sum <- data.frame(dt,cta.sum)
    cta.sum <- cta.sum[,c(3,1,2,4,5,6,7,8,9,10,11,12,13,14)]
    }

  if(weight==T){
    cta.sum <- data.frame(cta.sum,wess)

    ### Extract weighted CTA Model Summary ###
    cta.sum <- cta.sum[seq(m.type, nrow(cta.sum), m.type), ]
    cta.sum$mindenom <- min_values # replace placeholder setting with obs mindenom
    cta.sum <- as.data.frame(lapply(cta.sum,function(x) {rownames(x) <- NULL; x}))
    cta.sum$wD <- 100/(cta.sum$Weighted.ESS/cta.sum$Strata)-cta.sum$Strata
    min_wD_value <- min(cta.sum$wD)
    min_wD_row <- which(cta.sum$wD==min_wD_value)
    cat("The smallest observed wD statistic was:", min_wD_value, "\n")
    cat("Model(s) with the smallest observed wD statistic:", cta.sum[min_wD_row,]$tree.num, "\n")
    csumm[[thismod]] <- cta.sum
  } else{

    ### Extract non-weighted CTA Model Summary ###
    cta.sum <- cta.sum
    cta.sum <- cta.sum[seq(m.type, nrow(cta.sum), m.type), ]
    cta.sum$mindenom <- min_values # replace placeholder setting with obs mindenom
    cta.sum <- as.data.frame(lapply(cta.sum,function(x) {rownames(x) <- NULL; x}))
    min_D_value <- min(cta.sum$D)
    min_D_row <- which(cta.sum$D==min_D_value)
    cat("The smallest observed D statistic was:", min_D_value, "\n")
    cat("Model(s) with the smallest D statistic is:", cta.sum[min_D_row,]$tree.num, "\n")
    csumm[[thismod]] <- cta.sum
  }
  cat("The minimum observed denominator value was:",min(min_values),"\n")
  cat("Model(s) with the minimum observed denominator value:", mod_num[min_indices], "\n")
  #### Warning if one or more attributes had no solutions found. ####
  if(length(index5) > 0){
    # class variables with no solutions identified
    miss.tree <- data_raw[index5,][seq(m.type,length(index5),by=m.type)]
    miss.tree <- as.data.frame(miss.tree)
    miss.tree <- miss.tree %>% separate(miss.tree, c("Type", "Class"), "-")
    miss.type   <- gsub(" Tree Results ","",miss.tree$Type)
    miss.class  <- gsub("Class ","",miss.tree$Class)
    miss.result <-  data.frame(Tree=miss.type,Class=miss.class,Result="No tree found")
    cta.miss <- miss.result
    cat("Warning: One or more models yielded no solutions.\n")
    ### Extract List of CTA cross class tables###
    misumm[[thismod]] <- cta.miss
  }
  }
  for (thismod in allmods){
    assign(paste0("cta.model.",run,".",thismod), mosumm[[thismod]], pos = parent.frame())
    assign(paste0("cta.sum.",run,".",thismod), csumm[[thismod]], pos = parent.frame())
    assign(paste0("cta.denom.",run,".",thismod), dsumm[[thismod]], pos = parent.frame())
    assign(paste0("cta.list.",run,".",thismod), lsumm[[thismod]], pos = parent.frame())
    if(length(index5) > 0){
      assign(paste0("cta.miss.",run,".",thismod), misumm[[thismod]], pos = parent.frame())
    }
  }
}
