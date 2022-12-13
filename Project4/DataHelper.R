library(tidyverse)

readData <- function(){

  tab <- read_csv("cleanData_Airbnb.csv")
  
  return(tibble(data.frame(tab)))
}