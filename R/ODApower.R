#' @title Estimate power for an ODA model.
#'
#' @description Statistical power is estimated for a unit weighted binary
#'   application with balanced samples in each of 2 groups
#'
#' @param n1 A numeric vector that contains the number of subjects in group 1
#' @param n2 A numeric vector that contains the number of subjects in group 2
#' @param p1 A numeric value that is the proportion of group 1 with the outcome
#' @param p2 A numeric value that is the proportion of group 2 with the outcome
#' @param comp An integer value specifying the number of experiment wise
#'   comparions for a Sidak type adjustment to \code{alpha}
#' @param alpha Numeric value specifying the a priori level of signifanced
#'   assumed
#' @param nsim An integer value specifying the number of Fisher's Exact Tests to
#'   simulate.
#'
#' @return An array of power estimates with \code{nrows} of length \code{n1} and
#'   \code{ncol} of length \code{comp}
#' @export
#'
#' @details A default of 10,000 Monte Carlo Fisher's Exact tests are simulated
#'   and compared to \code{alpha} to estimate power.
#'
#'   The resulting power estimate represents a "worst case scenario" for the
#'   lowest level of measurement accuracy, i.e., a two class and two attribute
#'   problem, see Rhodes 2020.
#'
#'   For unit weighted applications, a Fisher's Exact test is isomorphic to the
#'   power of an ODA model, see Yarnold \emph{et al.} 2005.
#'
#'   The \emph{a priori} signifiance level \code{alpha} is adjusted based on
#'   number of comparisons (\code{comp}) as follows: \eqn{alpha_{adjusted} =
#'   1-(1-alpha)^{1/comp}}.
#'
#' @importFrom statmod power.fisher.test
#'
#' @seealso \code{\link{fisher.test}} \code{\link{power.fisher.test}}
#'
#' @author Nathaniel J. Rhodes
#'
#' @examples
#' n1 <- seq(15,50,5)
#' n2 <- seq(15,50,5)
#' p1 <- 0.74
#' p2 <- 0.26
#' alpha <- 0.05
#' comp <- 1
#' nsim <- 100
#' #Power for an analysis with an ESS of 48% (a moderate effect)
#' ess <- 100*(((0.74+0.74)/2)-0.5)/0.5
#' ODApower(n1=n1,n2=n2,comp=comp,p1=p1,p2=p2,alpha=alpha,nsim=nsim)
#'
#' @references Rhodes N.J. (2020). Statistical Power Analysis in ODA, CTA and
#'   Novometrics. \emph{Optimal Data Analysis} \bold{9}, 21-25.
#'   \url{https://odajournal.files.wordpress.com/2020/02/v9a5.pdf}
#'
#'   Yarnold P.R. and Soltysik R.C. (2005). \emph{Optimal data analysis:
#'   Guidebook with software for Windows}. APA Books.
#'
ODApower <-function(n1,n2,p1,p2,comp,alpha,nsim){
  if (!missing(n1)) {
    n1 <- n1}
  if (missing(n1)) {
    stop("Please input the sample size for group 1")
  }
  if (missing(n2)) {
    n2 <- n1
    print("Equal sample within each binary class is assumed")
  }
  else {n2 <- n2}
  if (missing(p1)) {
    stop("Please provide a proportion (decimal) for group 1")
  }
  if  (missing(p2)) {
    stop("Please provide a proportion (decimal) for group 2")
  }
  if (missing(comp)) {
    comp <- 1
    print("Single comparison assumed (e.g., 2x2 analysis)")
  }
  else {comp <- comp}
  if (missing(alpha)) {
    stop("Please input the alpha value for group comparison")
  }
  else {
    if(length(comp) > 1) {
      a.adj <- 1-(1-alpha)**(1/(comp)) # alpha inflation adjustment for experimentwise error
    }
    else {
      a.adj <- alpha
    }
  }
  if (missing(nsim)) {
    nsim <- 10000
    print("Standard of 10,000 simulations assumed")
  }
  else {nsim <- nsim}
  power <- matrix(NA,nrow=length(n1),ncol=length(a.adj),dimnames=list(n1,paste0(comp,"-comparison(s)")))
  for(i in 1:length(n1)) {
    for(j in 1:length(a.adj)){
      set.seed(12345)
      power[i,j] <-  power.fisher.test(p1=p1, p2=p2, n1=n1[i], n2=n2[i], nsim=nsim, alpha=a.adj[j])
    }
  }
  return(power)
}
