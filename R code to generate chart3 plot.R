# R code to make the chart3 graph:
install.packages("devtools")
require(devtools)
install_github('rCharts', 'ramnathv')(edited)
require(rCharts)

setwd("~/Desktop/github/EDAV_Project_1")
survey <- read.csv("SurveyResponse_updated.csv")
#generate data set

names(survey)
skills <- summary(survey[,14])
for (i in c(18,15:17)){
  x <- summary(survey[,i])
  skills <- c(skills, x)
}
skills <- as.data.frame(skills)
colnames(skills) <- "count"

cols<-factor(c("R, graphic basics","Github","R, advanced ","Reproducible documentation","Matlab"))
skills$skill <- rep(cols,each = 4)
level <- factor(c("A little","Confident","Expert","None"))
skills$level <- rep(level,5) 

#rearrange orders:
skills$level <- factor(skills$level,levels =c("Expert","Confident","A little","None"))
skills <- with(skills,skills[order(level),])

#generate D3 plot
n <- nPlot(count ~ skill, group = "level", data = skills, type = "multiBarChart")
n$print("chart3")
