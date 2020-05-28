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

simdata = read_dta("Data/simulation.dta")


hist(simdata$te, freq = FALSE, breaks = 200, main = "Distribution of Treatment Effects", xlab = "Treatment Effect (TE)")

qje = summary(simdata$te)

te_dist = data.frame(as.numeric(qje[1]))
te_dist[2] = as.numeric(qje[2])
te_dist[3] = as.numeric(qje[3])
te_dist[4] = as.numeric(qje[4])
te_dist[5] = as.numeric(qje[5])
te_dist[6] = as.numeric(qje[6])
te_dist[7] = as.numeric(qje[7])
colnames(te_dist) = c("Min", "1st Qtl", "Median", "Mean", "3rd Qtl", "Max", "NAs")
te_dist = round(te_dist, digits = 4)

library(rtf)
rtffile <- RTF("Tables/DID_dist_te.rtf")
addText.RTF(this = rtffile, "Dist of TE\n\n", bold = TRUE)
addTable.RTF(this = rtffile, dat = te_dist)
done(rtffile)



reg_prob_5 = lm(y~treat+group+year, data = simdata)
summary(reg_prob_5)


library(lfe)

fe_reg = felm(y~treat|year+id , data = simdata)
summary(fe_reg)

tab_model(fe_reg, dv.labels = "Fixed Effects", file = "Tables/DID_Prob_5.rtf", title = "Assignment 5 DID Problem 5", show.r2 = FALSE, show.p = FALSE, p.style = "stars", show.intercept = FALSE, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE)


#Problem 7
fe_reg_2 = felm(y2~treat|year+id , data = simdata)
summary(fe_reg_2)

