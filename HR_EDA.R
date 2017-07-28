setwd("E:/RPI/Spring2017/Data Analytics/Project/human-resources-analytics")

HRData <- read.csv("HRData.csv", header = TRUE, sep = ",")

str(HRData)
summary(HRData)

HRData$left <- as.factor(HRData$left)

library("ggthemes")
library(ggplot2)

####### BARPLOTS #########
var.sales <- ggplot(HRData, aes(x =sales, fill = left)) +
  geom_bar() +
  labs(x = 'SALES') +
  theme_excel()

var.salary <- ggplot(HRData, aes(x =salary, fill = left)) +
  geom_bar() +
  labs(x = 'SALARY') +
  theme_excel()

var.project <- ggplot(HRData, aes(x =number_project, fill = left)) +
  geom_bar() +
  labs(x = 'NUMBER OF PROJECTS') +
  theme_excel()

var.years <- ggplot(HRData, aes(x =time_spend_company, fill = left)) +
  geom_bar() +
  labs(x = 'NUMBER OF YEARS WITH THE COMPANY') +
  theme_excel()

var.accident <- ggplot(HRData, aes(x =Work_accident, fill = left)) +
  geom_bar() +
  labs(x = 'ACCIDENTS DURING WORK') +
  theme_excel()

var.promotion <- ggplot(HRData, aes(x =promotion_last_5years, fill = left)) +
  geom_bar() +
  labs(x = 'PROMOTION IN LAST 5 YEARS') +
  theme_excel()

var.satisfaction <- ggplot(HRData, aes(x =satisfaction_level, fill = left)) +
  geom_bar() +
  labs(x = 'SATISFACTION LEVEL') +
  theme_excel()

var.avgmonthlyhours <- ggplot(HRData, aes(x =average_montly_hours, fill = left)) +
  geom_bar() +
  labs(x = 'AVG MONTHLY HOURS') +
  theme_excel()

library(grid)
library(gridExtra)
var.left <- grid.arrange(var.sales,var.accident,var.project,var.promotion,
                         var.salary,var.years,var.satisfaction,var.avgmonthlyhours, 
                         top=textGrob("BARPLOTS", gp=gpar(fontsize=15,font=1)))


#### BOXPLOTS ####
box.satisfaction <- ggplot(HRData, aes(x = "", y = satisfaction_level)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("SATISFACTION")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)

box.lastevaluation <- ggplot(HRData, aes(x = "", y = last_evaluation)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("LAST EVALUATION")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)

box.avgmonthlyhours <- ggplot(HRData, aes(x = "", y = average_montly_hours)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("AVG MONTHLY WORK HOURS")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)

box.time <- ggplot(HRData, aes(x = "", y = time_spend_company)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("TIME WITH THE COMPANY")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)

box.grid <- grid.arrange(box.satisfaction,box.lastevaluation,box.avgmonthlyhours,
                         box.time, 
                         top=textGrob("BOXPLOTS", gp=gpar(fontsize=15,font=1)))

######### QQ PLOTS ##############
qq_satisfactionlevel <- qqnorm(HRData$satisfaction_level, main = "Satisfaction Level")
qq_lastevaluation <- qqnorm(HRData$last_evaluation, main = "Last Evaluation")
qq_avgmnthhrs <- qqnorm(HRData$average_montly_hours, main = "Avg Monthly Hours")

####### VARIABLE PLOTS ###########
var1 <- ggplot(HRData, aes(x=satisfaction_level, y=average_montly_hours))+
  ggtitle("Satisfaction vs Avg Monthly Hours")+geom_line()

var2 <- ggplot(HRData, aes(x=satisfaction_level, y=time_spend_company))+
  ggtitle("Satisfaction vs Time Spent in Company")+geom_line()

var3 <- ggplot(HRData, aes(x=satisfaction_level, y=last_evaluation))+
  ggtitle("Satisfaction vs Last Evaluation")+geom_line()

var4 <- ggplot(HRData, aes(x=last_evaluation, y=average_montly_hours))+
  ggtitle("Last Evaluation vs Avg Monthly Hours")+geom_line()


var.grid1 <- grid.arrange(var1,var2,var3,var4, 
                         top=textGrob("PLOTS", gp=gpar(fontsize=15,font=1)))

########### HISTOGRAMS ##############
hist_satisfaction <- ggplot(data=HRData, aes(HRData$satisfaction_level)) + geom_histogram(breaks=seq(0, 1, by=0.1),col="red",fill="yellow")
hist_evaluation <- ggplot(data=HRData, aes(HRData$last_evaluation)) + geom_histogram(breaks=seq(0, 1, by=0.1),col="red",fill="orange")
hist_hours <- ggplot(data=HRData, aes(HRData$average_montly_hours)) + geom_histogram(breaks=seq(0, 320, by=10),col="red",fill="blue")
hist_time <- ggplot(data=HRData, aes(HRData$time_spend_company)) + geom_histogram(breaks=seq(0, 10, by=1),col="orange",fill="red")
hist_project <- ggplot(data=HRData, aes(HRData$number_project)) + geom_histogram(breaks=seq(1, 8, by=1),col="orange",fill="green")

hist.grid <- grid.arrange(hist_satisfaction,hist_evaluation,hist_hours,
                          hist_time,hist_project,
                          top=textGrob("HISTOGRAMS", gp=gpar(fontsize=15,font=1)))
