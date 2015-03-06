################################################################################
#                                                                              #
# Quantitative Biodiversity Functions Source Code                              #
#                                                                              #
# Written by: Jason Walsman                                                    #
#                                                                              #
# Last update: 2015/03/06                                                      #
#                                                                              #
# Notes: This file contains functions to calculate metrics of taxonomic and    #
# phylogenetic diversity                                                       #
#                                                                              #
################################################################################

require("vegan")||install.packages("vegan");require("vegan")

#Calculates niche breadth based on a table of growth of species 
#(each species is a row) on different resources (each resource is a column)
umax<-(apply(p.growth,1,max))
levins<-function(p_xi = ""){
  p=0
  for (i in p_xi){
    p=p+i^2
  }
  nb=1/(length(p_xi)*p)
  return(nb)
}
nb<-as.matrix(levins(p.growth.std))
rownames(nb)<-row.names(p.growth)
colnames(nb)<-c("NB")

#Calculates Whitaker's Beta-Diversity between two sites
beta.w <- function(site1 = "", site2 = ""){
  site1 = subset(site1, select = site1 > 0)
  site2 = subset(site2, select = site2 > 0)
  gamma = union(colnames(site1), colnames(site2))
  s = length(gamma)
  a.bar = mean(c(specnumber(site1), specnumber(site2)))
  b.w = round(s/a.bar - 1, 3)
  return(b.w)
}