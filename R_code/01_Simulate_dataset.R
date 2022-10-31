##################################################
## Title: "Simulate clustered RCT data"
## Author: Isabel Onate
## Contact: isabel.onate@northwestern.edu
## Date: Oct 2021
##################################################

# This code tests model stability by changing the seed for different models and comparing predictions

########
## SETUP
########

rm(list=ls()) 
packages <- c('simstudy', 'data.table', 'ggplot2', 'clusterPower', 'parallel',
              'lmerTest')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
rm(list='package.check') 

RNGkind("L'Ecuyer-CMRG") # enables seed for parallel process
set.seed(11397)

clusteredData <- function(k, m, d1, d2) {
  
  dc <- genData(k, d1)
  dc <- trtAssign(dc, grpName = "treatment")
  
  di <- genCluster(dc, "cluster", m, level1ID = "id")
  di <- addColumns(d2, di)
  di[]
  
}
y0 <- defData(varname = "y0", formula = 0, 
                variance = 1, id = "cluster", dist = "normal")

y1 <- defDataAdd(varname = "y1", formula = "y0 + 0.1 * treatment", 
                    variance = 1, dist = "normal")
dt <- clusteredData(k = 100, m = 500, y0, y1)

setcolorder(dt, c('id', 'cluster', 'treatment', 'y0', 'y1'))

dt[treatment==1, y.obs := y1]

dt[, y.obs := fcase( treatment == 0, y0,
                     treatment == 1, y1)]

dt[, y.mis := fcase( treatment == 0, y1,
                     treatment == 1, y0)]

var.true <- (0.1^2)*var(dt$treatment)
var.sample <- var(dt$y1 - dt$y0)












