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

#Where Adjustement was coded as follows: 1=Use of Ridges,
# 2=Shifting Habitation, 3=Relocation, and 4=Intensified Cultivation


## -----------------------------------------------------------------------------
library(ODA)

#Not run-already exists
#ODAtree("Example 2")

#setwd("~/vignettes/data/Example 2/Runs")

#write.csv(data.raw,file="data2.csv",row.names=F)

#setwd("~/vignettes/data/Example 2")

#ODAclean(data="data2.csv",output=1)

## Load data.csv file and obtain recoded variable names
ODAload(1)
# Recoded variable (v) names to be used in ODA analysis
print(key.1)

## Run MegaODA via ODArun() specifying class and attribute and categorical variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v2",attribute="v1", 
#                                   categorical = "v1", miss=-9,mcarlo=T,iter=25000)

## Load ODA model into active environment and evaluate output
#ODAparse(1) 
#Message: The default output file name of MODEL.OUT was applied.
#Warning: A categorical model was detected. The MODEL.OUT results have been truncated.
#Categorical ODA Model summary:
#  Model                         Attributes                    Predictions 
#      1 V1 = 1 , V1 = 2 , V1 = 3 , V1 = 4  V2 = 1, V2 = 1, V2 = 0, V2 = 0
#  Overall ESS(%) Overall ESP(%) MC P-value  LOO ESS(%) LOO ESP(%) LOO P-value
#          56.30%         65.17%   0.000000      56.30%     65.17%  .816E-0068

## Generate multicategorical model predictions (not automated currently)
data.1$Predict <- ifelse(data.1$Adjustment==1 | data.1$Adjustment==2,1,0)

## Table 2. Confusion Matrix for a Binary Class, Multi-categorical Attribute, ODA Model
print(table(cbind(data.1[2],data.1[3])))

# Here, Community=observed motivation (Community = 1; Individual = 0) 
# and Predict=predicted motivation (Community = 1; Individual = 0)

#Sens for Community = 97.2%
round(343/(343+10),3)*100

#Sens for Individual = 59.1%
round(217/(217+150),3)*100

#PV for Community = 69.6%
round(343/(343+150),3)*100

#PV for Individual = 95.6%
round(217/(217+10),3)*100

