## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir= "data/Example 4")

## -----------------------------------------------------------------------------
library(epitools)

#Summary data, expanded for writing to file, then presented as a cross class table
data <- matrix(c(13,9,4,2,1,1,3,0,5,13,6,1,2,3,3,1),
               ncol=2,nrow=8,dimnames=list(c(0,1,2,3,4,5,6,7),c(0,1)))

data.raw <- setNames(expand.table(data),c("Attacks","Treatment 2"))

data.tab <- table(cbind(data.raw[1],data.raw[2]))

## Table 1: Data for 67 Patients from a Clinical Trial of Two Migraine Treatments
print(data.tab)


## -----------------------------------------------------------------------------
library(ODA)

#Not run-already exists
#ODAtree("Example 4")

#setwd("~/vignettes/data/Example 4/Runs")

#write.csv(data.raw,file="data3.csv",row.names=F)

#setwd("~/vignettes/data/Example 4")

#ODAclean(data="data4.csv",output=1)

## Load data.csv file and obtain recoded variable names
ODAload(1)
# Recoded variable (v) names to be used in ODA analysis
print(key.1)

## Run MegaODA via ODArun() specifying binary class and ordered attribute variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v2",attribute="v1", 
#                                   miss=-9,mcarlo=T,iter=25000)

## Load ODA model into active environment and evaluate output
#ODAparse(1) 
#Message: The default output file name of MODEL.OUT was applied.
#ODA Model summary:
#  Model        IF    THEN       IF    THEN Overall ESS(%) Overall ESP(%) MC P-value 
#      1 V1 <= 0.5  V2 = 0 0.5 < V1  V2 = 1         24.69%         31.41%   0.086760 
#  LOO ESS(%) LOO ESP(%) LOO P-value
#      24.69%     31.41%     .021822

## Display model performance metrics
#print(oda.perf.1)
#Class_variable  Class_name Attribute Attribute_name  PAC  mpac  sens  spec ess.m 
#            V2 Treatment.2        V1        Attacks 62.7 62.34 85.29 39.39 24.68 
# mpv   ppv   npv esp.m   OR  LL.OR UL.OR
# 65.7 59.18 72.22  31.4 3.77  1.16 12.25

## Table 2. Confusion Matrix for a Binary Class, Ordered Attribute, ODA Model
#print(oda.list.1)
#   x
#v2   0  1
#  0 13 20
#  1  5 29

# Here, v2=Observed Treatment 2  status (1 or 0)
# and x=ODA predicted Treatment 2 status (1 or 0)

#Sens for Treatment 1 = 39.4%
round(13/(13+20),3)*100

#Sens for Treatment 2 = 85.3%
round(29/(29+5),3)*100

#PV for Treatment 1 = 72.2%
round(13/(13+5),3)*100

#PV for Treatment 2 = 59.2%
round(29/(29+20),3)*100


