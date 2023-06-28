## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir= "data/Example 2")

## -----------------------------------------------------------------------------
require(epitools)

#Summary data, expanded for writing to file, then presented as a cross class table
data <- matrix(c(85,65,172,45,173,170,10,0),
               ncol=2,nrow=4,dimnames=list(c(1,2,3,4),c(0,1)))

data.raw <- setNames(expand.table(data),c("Adjustment","Community"))

data.tab <- table(cbind(data.raw[1],data.raw[2]))

## Table 1: Individual- (0) vs. Community-Led (1) Adjustments in Response to Gully Erosion
print(data.tab)

#Where Adjustment was coded as follows: 1=Use of Ridges,
# 2=Shifting Habitation, 3=Relocation, and 4=Intensified Cultivation


## -----------------------------------------------------------------------------
library(ODA)

#Not run-already exists
#ODAtree("Example 2")

#setwd("Example 2")

#write.csv(data.raw,file="ODA/data.csv",row.names=F)

#ODAclean(data="data.csv",output=1,type="ODA")

## Load data.csv file and obtain recoded variable names
ODAload(1,type = "oda")

# Recoded variable (v) names to be used in ODA analysis
print(oda.key.1)

## Run MegaODA via ODArun() specifying class and attribute and categorical variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v2",attribute="v1", categorical = "v1")

## Load ODA model into active environment and evaluate output
ODAparse(1,1) 
print(oda.model.1.1)

## Table 2. Confusion Matrix for a Binary Class, Categorical Attribute, ODA Model
print(oda.list.1.1)

# Here, v2=observed motivation (Community = 1; Individual = 0) 
# and x=predicted motivation (Community = 1; Individual = 0)

#Sens for Community = 97.2%
round(343/(343+10),3)*100

#Sens for Individual = 59.1%
round(217/(217+150),3)*100

#PV for Community = 69.6%
round(343/(343+150),3)*100

#PV for Individual = 95.6%
round(217/(217+10),3)*100

