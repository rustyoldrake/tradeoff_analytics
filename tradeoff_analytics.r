######################################################
### Experimental Code.  Pareto Optimization - R. Anderson
### Focus: Exploring Alternatives to Pareto Optimization in R
### https://www.ibm.com/watson/developercloud/doc/tradeoff-analytics/science.html
### Code is experimental / Please use with caution!
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(splitstackshape)


######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Project_Pareto")  # i'm on a mac
getwd()

URL_TA = 'https://gateway.watsonplatform.net/tradeoff-analytics/api'
username_TA ='dcf2f879-xxxx-xxxx-be91-774f3afaeab3'
password_TA ='xxxxxxxx'


filename = "drugcandidates99.json"

### FUNCTION DECLARE
query_TA <- function(filename){
  
  response <- POST(url=paste(
    URL_TA,
    "/v1/dilemmas",
    "?find_preferable_options=true",
    "&generate_visualization=false",
    sep=""),
    authenticate(username_TA,password_TA),
    add_headers("Content-Type"="application/json"),
    body = (file = upload_file(filename)) 
  )

  #response
  #response$headers
  #content(response)
  
  data <- content(response)
  data
  
  print(paste("ANALYSIS RESPONSE"))
  print(paste("FILENAME: ",filename))
  print(paste("PREFERABLE SOLUTION(S): ",data$resolution$preferable_solutions$solution_refs))
  print(paste("SCORE: ",data$resolution$preferable_solutions$score))
  #print(data$resolution$preferable_solutions$solution_refs)
  
  # winner! (first one, there could be multiple returned)
  winner = paste(data$resolution$preferable_solutions$solution_refs[1]) # could be more

}

# Interpretation https://www.ibm.com/watson/developercloud/doc/tradeoff-analytics/output.html 
query_TA("drugcandidates1.json")
query_TA("drugcandidates2.json")
query_TA("drugcandidates3.json")
query_TA("drugcandidates4.json")
query_TA("drugcandidates5.json")


query_TA("auto6.json") # price, engine size, average rating, reviews count, power, MPG (all factors)
# include the well balanced Toyota Prius

query_TA("auto3.json") #  engine size, average rating, power (Muscle Car lovers)
# Obvious Choice: "key": 200744626, "Chevrolet Corvette", 6.2L 8cyl S/C 7M)", power": 650hp


# References (TA)
# Self organizing maps for visualizing an objective space (US 9104963 B2)   https://www.google.com/patents/US9104963 
# Automated multi-objective solution selection (US 20150363703 A1) https://www.google.com/patents/US20150363703 




##############
library(kohonen)
autodata <- read.csv2("auto.csv", header = TRUE, sep = ",")
autodata <- autodata[4:9]
#autodata <- data.frame(autodata)
head(autodata)
 
# take the long way
autodata$price <- as.numeric(autodata$price)
autodata$engineSize <- as.numeric(autodata$engineSize)
autodata$averageRating <- as.numeric(autodata$averageRating)
autodata$reviewsCount <- as.numeric(autodata$reviewsCount)
autodata$power <- as.numeric(autodata$power)
autodata$MPGCombined <- as.numeric(autodata$MPGCombined)

## Kohonen Self Organizing Map
autodata.sc <- scale(autodata) # numbers only
set.seed(7)
autodata.som <- som(autodata.sc, grid = somgrid(10, 10, "hexagonal"))
   
   # Color Palette 
   coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=5/6, alpha=alpha)[n:1] }

plot(autodata.som, main = "Automotive Decision Support \n Kohonen Self Organizing Map",palette.name=coolBlueHotRed)


####   EXPLORING RPREF
# rPref is a package (CRAN) for the statistical computing language R for Skyline computation 
# and some slight generalizations of it. The Skyline calculation in rPref is done very efficiently
# as all performance critical algorithms are written in C++.   The Skyline of a data set selects tuples 
# which are Pareto-optimal with respect to given optimization goals. Only those tuples are returned which 
# are not dominated by any other tuple. A tuple dominates another tuple if it is better in all relevant 
# dimensions and strictly better in at least one dimension. Hence, the computation of the Skyline is a 
# powerful tool for pre- filtering large data sets under given optimization goals. A typical example from
# economics is the search for products with low price and high quality. In this case one typically assumes 
# that products which are worse in both dimensions (price and quality) are not interesting. 
# Thus, a Pareto query optimizing for low price and high quality only returns the potentially interesting products.

library(rPref)
# Skyline and top-k/at-least skyline
psel(mtcars, low(mpg) * low(hp))
psel(mtcars, low(mpg) * low(hp), top = 5)
psel(mtcars, low(mpg) * low(hp), at_least = 5)
# preference with associated data frame and evaluation
p <- low(mpg, df = mtcars) * (high(cyl) & high(gear))
peval(p)
# visualize the skyline in a plot
sky1 <- psel(mtcars, high(mpg) * high(hp))
plot(mtcars$mpg, mtcars$hp)
points(sky1$mpg, sky1$hp, lwd=3)
# grouped preference with dplyr
library(dplyr)
psel(group_by(mtcars, cyl), low(mpg))
# return size of each maxima group
summarise(psel(group_by(mtcars, cyl), low(mpg)), n())
