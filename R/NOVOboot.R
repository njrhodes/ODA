#' @title Perform novometric bootstrap analysis using a classification model
#'
#' @param data The name of a valid object created by \code{\link{ODAparse}} that
#'   begins with \code{oda.list.x} where \code{x} is the \code{run}.
#' @param run The run number of the ODA model created using \code{\link{ODArun}}
#'   and parsed using \code{\link{ODAparse}}.
#' @param predictor   The model number within the \code{\link{ODArun}} which is
#'   read using \code{\link{ODAparse}}. This is the model that is compared to
#'   chance.
#' @param outcome The outcome number, when more than one outcome was evaluated.
#'   For example, if three outcomes were evaluated simultaneously and the impact
#'   of outcome one was of interest, then enter 1.
#' @param nboot The number of bootstrap replicates. Both model and chance will
#'   be evaluated using this number of replicates using a 50\% resampling with
#'   replacement. The default value is 25,000 replicates.
#' @param seed The seed number passed to \code{\link{set.seed}} that serves as
#'   the origin of the pseudorandom numbers generated for the bootstrap
#'   resampling. The default seed number is the current system time.
#' @param alternative Specification of a two-tailed test as \code{two.sided} is
#'   assumed but alternative specifications are \code{greater} or or \code{less}
#'
#' @return An array of percentiles from 0\% to 100\% capturing the resampled
#'   model and chance performance metrics. The simulated bootstraps are stored
#'   within the object \code{novo.boot.x} which is appended with the current
#'   \code{run} number \code{x}. The model and chance quantile summary is stored
#'   within the object \code{sum.boot.x} which is appended with the current
#'   \code{run} number \code{x}.
#'
#' @details The first axiom of novometric theory states that, for a random
#'   statistical sample "S" consisting of a class variable, one or more
#'   attributes, and a weight, the corresponding exact discrete confidence
#'   intervals, CIs, for model and for chance do NOT overlap. That is, a
#'   signficant model exists. If the CIs overlap, the effect is not
#'   statistically significant. However, if the CIs do not overlap, then the
#'   effect strength of the model is statistically significant at the confidence
#'   level selected by the user.
#'
#'   \code{NOVOboot} reports the Effect Strength for Sensitivity (ESS), which is
#'   the mean Percent Accuracy in Classification (mean PAC) corrected for
#'   chance, in quantiles from 0\% to 100\% in the first slot of the
#'   \code{sum.boot.x}. For a 2 x 2 matrix, mean PAC is equivalent to the ROC
#'   area. \code{NOVOboot} also reports the distribution of mean PAC in
#'   quantiles from 0\% to 100\% in the second slot of \code{sum.boot.x}.
#'   Because a binary classification matrix can also expressed as an odds ratio
#'   (OR) or as a risk ratio (RR), \code{NOVOboot} also reports estimates of the
#'   OR and RR for the model and chance effect distributions in the third and
#'   fourth slots of the \code{sum.boot.x} object, respectively. Likewise, one
#'   may wish to consider the corresponding distribution of p values from
#'   Fisher's Exact tests conducted for each bootstrap replicate. These
#'   distributions for both model and chance are reported in the fifth slot of
#'   the \code{sum.boot.x} object.
#'
#'   The results of this non-parametric bootstrap analysis provide a mechanism
#'   to evaluate the precision of point estimate of an effect provided by an ODA
#'   model. The same methodology can be applied to any classifcation model
#'   wherein a two class and two attribute confusion matrix can be formulated.
#'   It is axiomatic that the exact discrete confidence intervals for model and
#'   chance define the boundry between "signal" and "noise". For effects wherein
#'   the distributions of model and chance overlap, it is concluded that the
#'   effect was not significantly different vs. the distribution of chance given
#'   the data.
#'
#' @export
#'
#' @importFrom stats quantile
#' @importFrom epitools expand.table
#' @importFrom stats fisher.test
#'
#' @seealso \code{\link{ODArun}} \code{\link{ODAparse}} \code{\link{set.seed}}
#'
#' @examples
#' ## Not run
#' ## NOVOboot(data=oda.list.1,run=1,predictor=1,outcome=1,seed=1234)
#'
#' ## Example of a moderate effect size (ESS) exact discrete 95% confidence interval
#' ess <- ((((14/14)+(20/64))/2)-0.5)/.5        # 31.25% ESS, a moderate effect
#' data <- matrix(c(20,0,44,14),ncol=2,nrow=2,dimnames=list(c(0,1),c(0,1)))
#' data.raw <- epitools::expand.table(data)
#' data.tab <- list(table(cbind(data.raw[1],data.raw[2]),dnn=c("v1","x")))
#' oda.list.1 <- list()                         # supply list formatted for NOVOboot
#' oda.list.1[[1]] <- do.call("list",data.tab)  # supply data for NOVOboot
#'
#' # NOVOboot(data=oda.list.1,run=1,predictor=1,outcome=1,seed=1234, nboot=25000)
#'
#' # boot.1 <- novo.boot.1 # added after NOVOboot() run
#'
#' # print(sum.boot.1[[1]]) # displays the quantile summary of Model vs. Chance ESS
#'
#' # boot.list <- setNames(data.frame(boot.1$ess.model,boot.1$ess.chance),c("Model","Chance"))
#'
#' # df <- stack(boot.list,select=c("Model","Chance"))
#'
#' # library(ggplot2)
#' # ggplot(df,aes(x=values)) +
#' # geom_histogram(data=subset(df, ind== 'Chance'),fill="skyblue",colour="black",binwidth = 2) +
#' # geom_histogram(data=subset(df, ind== 'Model'), fill="pink", colour="black",binwidth=2) +
#' # xlab("Effect strength for sensitivity (ESS %)") +
#' # ylab("Frequency of bootstrap replicates") +
#' # geom_vline(aes(xintercept = quantile(boot.1$ess.model,probs=0.025, na.rm=T), color="LB"), linetype="dashed") +
#' # geom_vline(aes(xintercept = quantile(boot.1$ess.chance,probs=0.975, na.rm=T), color="UB"), linetype="dashed") +
#' # labs(title = "Bootstrap 95% Interval for ESS from Final Model vs. 95% Interval for Chance",
#' # subtitle = "ESS Distribution for Chance (Blue) vs. Model (Red) resamples (n=25,000)") +
#' # scale_color_manual(name = "95% PI", values = c(UB = "Blue", LB = "Red")) +
#' # theme_bw()
#'
#' @references Yarnold, P.R. (2020). Reformulating the First Axiom of Novometric
#'   Theory: Assessing Minimum Sample Size in Experimental Desig \emph{Optimal
#'   Data Analysis} \bold{9}, 7-8.
#'   \url{https://odajournal.files.wordpress.com/2020/01/v9a2.pdf}
#'
#'   Yarnold, P.R. and Soltysik, R.C. (2016). \emph{Maximizing Predictive
#'   Accuracy}. ODA Books. DOI: 10.13140/RG.2.1.1368.3286
#'
NOVOboot <- function(data="",run="",predictor="",outcome="",nboot="",seed="",alternative=""){
  if(!exists(paste0("oda.list.",run))){
    cat("It is recommended to run ODAparse() prior to NOVOboot() or supply a confusion matrix.\n")
  }
  if(length(data)==0){
    stop(cat("User must supply the oda.list object with associated run number to bootstrap.\n"))
  }
  if(missing(predictor)){
    stop(cat("User must supply the predictor variable (i.e., pv) number from the ODA model to bootstrap.\n"))
  }
  if(missing(outcome)){
    stop(cat("User must supply the outocme of the ODA model to bootstrap.\n"))
  }
  if(missing(nboot)){
    cat("Default of 25,000 replicates assumed.\n")
    reps <- 25000
  }
  else(reps<-nboot)
  if(missing(alternative)){
    cat("Default two-sided test assumed.\n")
    alt <- "two.sided"
  }
  else{alt<-alternative}
  if(missing(seed)){
    set.seed(as.numeric(Sys.time()))
  }
  else(set.seed(paste0(seed)))

  dat1 <- data.frame(data[[predictor]][outcome])

  dat2 <- dat1[rep(1:nrow(dat1), unlist(dat1[3])), 1:2]

  rows <- sample(nrow(dat2))

  dat2 <- dat2[rows,]

  # Chance bootstrap for 95% CI for chance
  dat <- list()
  for(i in 1:reps) {
    dat[[i]] <- lapply(dat2, function(x) {sample(x, round(0.5*length(x)),replace=T)})
  }

  tab <- list()
  for(i in 1:reps) {
    tab[[i]] <- table(dat[[i]])
  }

  sens <- list()
  spec <- list()
  mpac <- list()
  ess  <- list()
  p    <- list()
  or   <- list()
  rr   <- list()
  for(i in 1:reps) {
    sens[[i]] <- tab[[i]][2,2]/sum(tab[[i]][2,2],tab[[i]][2,1])
    spec[[i]] <- tab[[i]][1,1]/sum(tab[[i]][1,1],tab[[i]][1,2])
    mpac[[i]] <- (sens[[i]]+spec[[i]])/2
    ess[[i]]  <- (mpac[[i]]-0.5)/0.5
    p[[i]]    <- fisher.test(tab[[i]],alternative = alt)[1]
    or[[i]]   <- (tab[[i]][2,2] / tab[[i]][1,2]) / (tab[[i]][2,1] / tab[[i]][1,1])
    rr[[i]]   <- (tab[[i]][2,2] / sum(tab[[i]][2,2],tab[[i]][1,2])) / (tab[[i]][2,1] / sum(tab[[i]][2,1],tab[[i]][1,1]))
  }

  metrics <- data.frame(cbind(sens,spec,mpac,ess,p,or,rr))
  result  <- as.data.frame(sapply(metrics, unlist))
  result[1:4]  <- round(result[1:4]*100,digits=2)
  result[6:7]  <- round(result[6:7], digits=3)
  chance  <- result

  # Model bootstap for 95% CI for model
  dat   <- list()
  index <- list()
  for(i in 1:reps) {
    index[[i]] <- sample(round(0.5*nrow(dat2)),replace=T)
    dat[[i]]   <- dat2[index[[i]],]
  }

  tab <- list()
  for(i in 1:reps) {
    tab[[i]] <- table(dat[[i]])
  }

  sens <- list()
  spec <- list()
  mpac <- list()
  ess  <- list()
  p    <- list()
  or   <- list()
  rr   <- list()
  for(i in 1:reps) {
    sens[[i]] <- tab[[i]][2,2]/sum(tab[[i]][2,2],tab[[i]][2,1])
    spec[[i]] <- tab[[i]][1,1]/sum(tab[[i]][1,1],tab[[i]][1,2])
    mpac[[i]] <- (sens[[i]]+spec[[i]])/2
    ess[[i]]  <- (mpac[[i]]-0.5)/0.5
    p[[i]]    <- fisher.test(tab[[i]],alternative = alt)[1]
    or[[i]]   <- (tab[[i]][2,2] / tab[[i]][1,2]) / (tab[[i]][2,1] / tab[[i]][1,1])
    rr[[i]]   <- (tab[[i]][2,2] / sum(tab[[i]][2,2],tab[[i]][1,2])) / (tab[[i]][2,1] / sum(tab[[i]][2,1],tab[[i]][1,1]))
  }

  metrics <- data.frame(cbind(sens,spec,mpac,ess,p,or,rr))
  result  <- as.data.frame(sapply(metrics, unlist))
  result[1:4]  <- round(result[1:4]*100,digits=2)
  result[6:7]  <- round(result[6:7], digits=3)
  model   <- result

  boot.out <- setNames(data.frame(model,chance),paste0(rep(c("sens.","spec.","mpac.","ess.","p.","or.","rr."),times=2),rep(c("model","chance"),each=7)))

  out <- setNames(data.frame(
    quantile(
      model$ess, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      chance$ess, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      model$mpac, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      chance$mpac, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      model$or, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      chance$or, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      model$rr, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      chance$rr, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      model$p, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),
    quantile(
      chance$p, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T)),
    c("ESS(%) Model","ESS(%) Chance","MPAC(%) Model","MPAC(%) Chance","OR Model","OR Chance","RR Model","RR Chance","P-value Model","P-value Chance"))

  boot <- list()

  boot[[1]] <- out[1:2]

  boot[[2]] <- out[3:4]

  boot[[3]] <- out[5:6]

  boot[[4]] <- out[7:8]

  boot[[5]] <- out[9:10]

  assign(paste0("novo.boot.",run), boot.out, pos = parent.frame())
  assign(paste0("sum.boot.",run), boot, pos = parent.frame())
  return(boot[[1]])
}
