library(tidyverse)

readData <- function(){
  tab <- read_csv("C:\\Users\\sbgad\\Desktop\\airbnb\\cleanData_Airbnb.csv")
  return(tibble(data.frame(tab)))
}