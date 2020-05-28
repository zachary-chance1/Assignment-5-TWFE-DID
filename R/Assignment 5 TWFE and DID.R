################################################################
#name: Assignment 5 TWFE and DID
#author: zachary chance (baylor university)
#description: completes assignment 5 and performs analysis
#             on the assigned data under TWFE and DID scenarios
#date: may 27, 2020
#################################################################


WORK.DIR = "C:/Users/Owner/Desktop/Assignment-5-TWFE-DID"

# set working directory
setwd(WORK.DIR)

library(AER)
library(haven)
library(tidyverse)


problem_1_data = read.csv("Data/hw5_ex1.csv")
