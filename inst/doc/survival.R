## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=6
)
knitr::opts_knit$set(root.dir= "myeloma")


# Check and install required packages
required_packages <- c("survival", "survminer", "tidyverse", "fastDummies", "devtools")

# Function to check if a package is installed
is_package_installed <- function(package_name) {
  return(package_name %in% installed.packages())
}

# Check and install required packages
for (package in required_packages) {
  if (!is_package_installed(package)) {
    install.packages(package, dependencies = TRUE)
  }
  
  # Load the package
  library(package, character.only = TRUE)
}

# Check if ODA package is installed
if (!is_package_installed("ODA")) {
  # Install ODA package from GitHub
  devtools::install_github("njrhodes/ODA", ref = "main")
}

# Load ODA package
library(ODA)


## -----------------------------------------------------------------------------
# Replace "~" with your own directory path
project_dir <- "~/Workshop" 

# Set the working directory to "project_dir"

# Myeloma data extracted from publicly available gene expression data (GEO Id: GSE4581)
data("myeloma")

glimpse(myeloma)

myeloma %>%
  filter(time==0) # exclude time=0 (v2=0) event from survival analysis

# Create dummy coded variables for char and factor variables
myeloma.oda <- dummy_cols(myeloma, select_columns = c("molecular_group", "chr1q21_status"))

glimpse(myeloma.oda)

# remove non-compatable char and factor variables from analysis
myeloma.oda <- myeloma.oda[-c(1:3)]

# create ODA project in current directory
#ODAtree("myeloma") # already run

# Set the working directory to "myeloma"


## -----------------------------------------------------------------------------
# write the data file to the project directory
#write.csv(file = "/ODA/data.csv", myeloma.oda, row.names = FALSE)

# running this pre-processing code will create a cleaned (NA removed, replaced with -9) data.txt file
#ODAclean(data="data.csv",output = 1, type="ODA") # already run

#review the variable names and aliases to be used in command syntax
ODAload(1,type = "oda")

oda.key.1

# Create ODA command syntax file within the run directory, then execute the batch file
# ODArun(run=1, # this will take about 15 minutes to run to completion
#        vstart="v1",
#        vend="v19",
#        class="v1",
#        attribute="v3 to v8 v9 v11 v12 v14 v15 v16 v17 v18 v19", # exclude v10 and v13, no solutions found
#        miss="-9",
#        exclude = "v2=0",
#        weight="v2") # time weighted ODA model

# once batch completes, the resulting model file will be moved to outputs folder

# the resulting model can then be parsed and reviewed
ODAparse(run=1,mod=1)

oda.model.1.1

oda.list.1.1[10] # there is one v2=0 event at time zero, must subtract from table

sn <-18/(18+51)
sp <-160/(160+26)

100*(((sn+sp)/2) - 0.5) / 0.5

oda.model.1.1[10,]

ODAsummary(1.1)

oda.summary.1.1 %>%
  filter(Weighted.LOO.ESS.==Weighted.ESS.) %>%
  select(Model.,IF.Attributes.,THEN.Predict.,LOO.P.value. ) # v4 v9 v11 v12 v14 to v19


## -----------------------------------------------------------------------------
#write.csv(file="CTA/data.csv",myeloma.oda,row.names = F)

#ODAclean(data="data.csv",output = 1, type="CTA")

ODAload(1,type = "cta")

#review the variable labels and aliases
cta.key.1

# Create CTA command syntax file within the run directory, then execute the batch file
# CTArun(run=1,
#        vstart="v1",
#        vend="v19",
#        class="v1",
#        attribute="v4 v9 v11 v12 v14 to v19", # wess stable in training and loo
#        miss="-9",
#        exclude = "v2=0",
#        weight="v2",
#        mindenom = 1,
#        enumerate = T)

# once batch completes, the resulting model file will be moved to outputs folder

# the resulting model can then be parsed and reviewed
# note syntax below differs from ODAparse() as it includes type and weight
CTAparse(run=1,mod=1,type="Enumerated",weight=T)

cta.sum.1.1


## -----------------------------------------------------------------------------

#write.csv(file="CTA/data.csv",myeloma.oda,row.names = F)

#ODAclean(data="data.csv",output = 2, type="CTA")

ODAload(2,type = "cta")

#review the variable labels and aliases
cta.key.2

# CTArun(run=2,
#        vstart="v1",
#        vend="v19",
#        class="v1",
#        attribute="v4 v9 v11 v12 v14 to v19", # wess stable in training and loo
#        miss="-9",
#        exclude = "v2=0",
#        weight="v2",
#        mindenom = 30, #  mindenom from Run 1 was 29, so Run 2 mindenom = 30
#        enumerate = T)

CTAparse(run=2,mod=1,type="Enumerated",weight=T)

cta.sum.2.1


## -----------------------------------------------------------------------------
#write.csv(file="CTA/data.csv",myeloma.oda,row.names = F)

#ODAclean(data="data.csv",output = 3, type="CTA")

ODAload(3,type = "cta")

#review the variable labels and aliases
cta.key.3

# CTArun(run=3,
#        vstart="v1",
#        vend="v19",
#        class="v1",
#        attribute="v4 v9 v11 v12 v14 to v19", # wess stable in training and loo
#        miss="-9",
#        exclude = "v2=0",
#        weight="v2",
#        mindenom = 56, #  mindenom from Run 2 was 55, so Run 3 mindenom = 56
#        enumerate = T)

#CTAparse(run=3,mod=1,type="Enumerated",weight=T) # Error: no tree found


## -----------------------------------------------------------------------------
cta.sum.1.1
cta.model.1.1
cta.list.1.1
cta.key.1

cta.sum.2.1
cta.model.2.1
cta.list.2.1

## -----------------------------------------------------------------------------
# variable coded names
cta.key.1[14,]
cta.key.1[15,]

## -----------------------------------------------------------------------------
# make a copy of the data to update with CTA predictions
myeloma.cta <- myeloma.oda

myeloma.cta$CTA.predicted.event <- with(myeloma.cta, ifelse(
  time != 0,
  ifelse(molecular_group_MMSET > 0.5, 1,
         ifelse(molecular_group_Proliferation > 0.5, 1, 0)),
  NA
))

table(myeloma.cta$event,myeloma.cta$CTA.predicted.event)

# compare to CTA model summary

cta.list.1.1

## -----------------------------------------------------------------------------

#write.csv(file="ODA/data2.csv",myeloma.cta,row.names = F)

#ODAclean(data="data2.csv",output = 2, type="ODA")

ODAload(2,type = "oda")

oda.key.2

# ODArun(run=2, 
#        vstart="v1", # takes about 10 seconds to run
#        vend="v20",
#        class="v1",
#        attribute="v20", # include only v20 (CTA predictions)
#        miss="-9",
#        exclude = "v2=0",
#        weight="v2") # time weighted ODA model

ODAparse(2,1)

oda.model.2.1

cta.sum.1.1


## -----------------------------------------------------------------------------
# Fit a KM to predictions
fit0 <- survfit(Surv(time, event) ~CTA.predicted.event, data = myeloma.cta)

# Fit a Cox model to predictions
fit1 <- coxph(Surv(time, event) ~CTA.predicted.event, data = myeloma.cta)
summary(fit1) # this is a parametric cox model fit
AIC(fit1)
loglik1 <-logLik(fit1)
loglik1
cox.zph(fit1)

# plot the KM curves for fit0
ggsurvplot(fit0, risk.table = TRUE, risk.table.col = "strata", pval = TRUE,
           surv.median.line = "hv", ggtheme = theme_bw(), palette="npg")

# plot the stratified cox model for fit1 by variable
ggadjustedcurves(fit1, data = myeloma.cta, method = "average", variable = "CTA.predicted.event",
                 ggtheme = theme_bw(), palette="npg")

## -----------------------------------------------------------------------------
### Use Run 1 GO CTA model strata for predicting survival over time ####
cta.model.1.1

myeloma.cta$group <- with(myeloma.cta, ifelse(
  time != 0,
  ifelse(molecular_group_MMSET <= 0.5,
         ifelse(molecular_group_Proliferation > 0.5, "MMSET=0 & Prolif=1", "MMSET=0 & Prolif=0"),
         "MMSET=1"),
  NA
))

# fit KM to CTA strata 
fit2 <- survfit(Surv(time, event) ~group, data = myeloma.cta)

#fit a Cox model to CTA strata
fit3 <- coxph(Surv(time, event) ~group, data = myeloma.cta)
summary(fit3) # this is a parametric cox model fit
AIC(fit3)
loglik3 <-logLik(fit3)
loglik3
cox.zph(fit3)

# plot the KM curves for fit2
ggsurvplot(fit2, risk.table = TRUE, risk.table.col = "strata", pval = TRUE,
           surv.median.line = "hv", ggtheme = theme_bw(), palette="npg")

# plot the stratified cox model for fit3 by variable
ggadjustedcurves(fit3, data = myeloma.cta, method = "average", variable = "group",
                 ggtheme = theme_bw(), palette="npg")

## -----------------------------------------------------------------------------
#vignette(package="ODA","Example-1")
#vignette(package="ODA","Example-2")
#vignette(package="ODA","Example-3")
#vignette(package="ODA","Example-4")

