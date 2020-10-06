## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir= "data/Example 1")

## -----------------------------------------------------------------------------
require(epitools)

#Summary data, expanded for writing to file, then presented as a cross class table
data <- matrix(c(118,34,78,177),ncol=2,nrow=2,dimnames=list(c(0,1),c(0,1)))

data.raw <- setNames(expand.table(data),c("Pro","Democrat"))

data.tab <- table(cbind(data.raw[1],data.raw[2]))

## Table 1: Voting Behavior on the Refugee Act by Political Affiliation
print(data.tab)


## -----------------------------------------------------------------------------
library(ODA)

#Not run-already exists
#ODAtree("Example 1")

#setwd("~/vignettes/data/Example 1/Runs")

#write.csv(data.raw,file="data1.csv",row.names=F)

#setwd("~/vignettes/data/Example 1")

#ODAclean(data="data1.csv",output=1)

## Load data.csv file and obtain recoded variable names
ODAload(1)
# Recoded variable (v) names to be used in ODA analysis
print(key.1)

## Run MegaODA via ODArun() specifying class and attribute variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v1",
#                                     attribute="v2", miss=-9,mcarlo=T,iter=25000)

## Load ODA model into active environment and evaluate output
#ODAparse(1) 
#Message: The default output file name of MODEL.OUT was applied.
#ODA Model summary:
#  Model        IF    THEN       IF    THEN Overall ESS(%) Overall ESP(%) MC P-value
#      1 V2 <= 0.5  V1 = 0 0.5 < V2  V1 = 1         44.09%         47.04%   0.000000
#  LOO ESS(%)   LOO ESP(%)   LOO P-value
#      44.09%       47.04%    .922E-0020

## Table 2. Confusion Matrix for ODA Model, for Training and LOO Analysis
#print(oda.list.1) 
#   x
#v1    0   1
#  0 118  78
#  1  34 177

# Here, v1=observed votes (Pro = 1; Con = 0) 
# and x=predicted votes (Pro = 1; Con = 0)

#Sens for Pro = 83.9%
round(177/(177+34),3)*100

#Sens for Con = 60.2%
round(118/(118+78),3)*100

#PV for Pro = 69.4%
round(177/(177+78),3)*100

#PV for Con = 77.6%
round(118/(118+34),3)*100

