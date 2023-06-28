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

#setwd("Example 4")

#write.csv(data.raw,file="ODA/data.csv",row.names=F)

#ODAclean(data="data.csv",output=1,type="ODA")

## Load data.csv file and obtain recoded variable names
ODAload(1,type = "oda")

# Recoded variable (v) names to be used in ODA analysis
print(oda.key.1)

## Run MegaODA via ODArun() specifying binary class and ordered attribute variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v2",attribute="v1")

## Load ODA model into active environment and evaluate output
ODAparse(1,1) 
print(oda.model.1.1)

## Display model performance metrics
print(oda.perf.1.1)

## Table 2. Confusion Matrix for a Binary Class, Ordered Attribute, ODA Model
print(oda.list.1.1)

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


