## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir= "data/Example 3")

## -----------------------------------------------------------------------------
library(epitools)

#Summary data, expanded for writing to file, then presented as a cross class table
data <- matrix(c(98,13,6,7,16,50,4,19,5,2,23,14,3,8,12,45),
               ncol=4,nrow=4,dimnames=list(c(1,2,3,4),c(1,2,3,4)))

data.raw <- setNames(expand.table(data),c("Biological","Amino Acid"))

data.tab <- table(cbind(data.raw[1],data.raw[2]))

## Table 1: Type of Protein Assessed by Biological Characteristics and Amino Acid Composition Types (1-4)
print(data.tab)


## -----------------------------------------------------------------------------
library(ODA)

#Not run-already exists
#ODAtree("Example 3")

#setwd("~/vignettes/data/Example 3/Runs")

#write.csv(data.raw,file="data3.csv",row.names=F)

#setwd("~/vignettes/data/Example 3")

#ODAclean(data="data3.csv",output=1)

## Load data.csv file and obtain recoded variable names
ODAload(1)
# Recoded variable (v) names to be used in ODA analysis
print(key.1)

## Run MegaODA via ODArun() specifying class and attribute and categorical variables
#Not run-already exists
#ODArun(run=1,vstart="v1",vend="v2",class="v1",attribute="v2", 
#                                   categorical = "v2", miss=-9,mcarlo=T,iter=25000)

## Load ODA model into active environment and evaluate output
#ODAparse(1) 
#Message: The default output file name of MODEL.OUT was applied.
#Warning: A categorical model was detected. The MODEL.OUT results have been truncated.
#Categorical ODA Model summary:
#  Model                         Attributes                    Predictions 
#1     1 V2 = 1 , V2 = 2 , V2 = 3 , V2 = 4  V1 = 1, V1 = 2, V1 = 3, V1 = 4
#  Overall ESS(%) Overall ESP(%) MC P-value LOO ESS(%) LOO ESP(%)
#          50.96%         51.22%   0.000000     50.96%     51.22%

## Generate multicategorical model predictions (not automated currently)
data.1$Predict <- ifelse(data.1$Amino.Acid==1 ,1,
                         ifelse(data.1$Amino.Acid==2,2,
                                ifelse(data.1$Amino.Acid==3,3,
                                       4)))

## Table 2. Confusion Matrix for a Muticategorical Class, Multi-categorical Attribute, ODA Model
print(table(cbind(data.1[1],data.1[3])))

# Here, Biological=observed Biological protein type (1-4) 
# and Predict=predicted Biological protein type (1-4)

#Sens for Type 1 Proteins = 80.3%
round(98/(122),3)*100

#Sens for Type 2 Proteins = 68.5%
round(50/(73),3)*100

#Sens for Type 3 Proteins = 51.1%
round(23/(45),3)*100

#Sens for Type 4 Proteins = 52.9%
round(45/(85),3)*100

#PV for Type 1 Proteins = 79.0%
round(98/(124),3)*100

#PV for Type 2 Proteins = 56.2%
round(50/(89),3)*100

#PV for Type 3 Proteins = 52.3%
round(23/(44),3)*100

#PV for Type 4 Proteins = 66.2%
round(45/(68),3)*100

