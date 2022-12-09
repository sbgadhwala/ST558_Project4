library(tidyverse)

readData <- function(){
  #tab <- read_csv("C:\\Users\\sbgad\\Desktop\\airbnb\\cleanData_Airbnb.csv")
  
  #For Github render
  tab <- read_csv("cleanData_Airbnb.csv")
  
  return(tibble(data.frame(tab)))
}