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

#make dummies

problem_1_data$is_unit_2 = 0
problem_1_data$is_unit_3 = 0
problem_1_data$is_unit_4 = 0
problem_1_data$is_unit_5 = 0

problem_1_data$is_time_2 = 0


attach(problem_1_data)

len = seq(1,dim(problem_1_data)[1])
for(i in len){
  if(Unit[i] == 2){
    problem_1_data$is_unit_2[i] = 1
  }
  if(Unit[i] == 3){
    problem_1_data$is_unit_3[i] = 1
  }
  if(Unit[i] == 4){
    problem_1_data$is_unit_4[i] = 1
  }
  if(Unit[i] == 5){
    problem_1_data$is_unit_5[i] = 1
  }
  
  if(Time[i] == 2){
    problem_1_data$is_time_2[i] = 1
  }
}


basic_reg = lm(Y~D+is_unit_2+is_unit_3+is_unit_4+is_unit_5+is_time_2, data = problem_1_data)
summary(basic_reg)



demeaned_reg = lm(Demeaned.Y~Demeaned.D+is_time_2, data = problem_1_data)
summary(demeaned_reg)



library(sjPlot)
library(sjmisc)
library(sjlabelled)


prob_1_table = tab_model(basic_reg, demeaned_reg, dv.labels = c("Standard OLS", "Demeaned OLS"), title = "Assignment 5 TWFE", show.r2 = FALSE, show.p = FALSE, p.style = "stars", show.intercept = FALSE, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, pred.labels = c("D", "Unit 2 FE", "Unit 3 FE", "Unit 4 FE", "Unit 5 FE", "Time 2 FE", "Demeaned D"))
prob_1_table


tab_model(basic_reg, demeaned_reg, dv.labels = c("Standard OLS", "Demeaned OLS"), file = "Tables/TWFE_Table.rtf", title = "Assignment 5 TWFE", show.r2 = FALSE, show.p = FALSE, p.style = "stars", show.intercept = FALSE, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, pred.labels = c("D", "Unit 2 FE", "Unit 3 FE", "Unit 4 FE", "Unit 5 FE", "Time 2 FE", "Demeaned D"))



