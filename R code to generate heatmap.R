# R code to make the heatmap graph:
library(ggplot2)
library(scales)
library(plyr)
library(reshape)

setwd("~/Google Drive/Data Science/Courses/Spring 2016/Exploratory Data Analysis and Visualization/Assignments/EDAV_Project_1/")
survey <- read.csv("./SurveyResponse_updated.csv")
#generate data set

progSurvey = cbind(survey$X,survey[,grep("Programming", colnames(survey))])
names(progSurvey) = c("x", "R", "R Graphics", "R Advanced", "R Markdown", "Matlab", "Github")
progSurvey$x <- with(progSurvey, reorder(x, R))
progSurvey.m = melt(progSurvey, id.vars = "x")
names(progSurvey.m) <- c("Student", "Skill", "Level")
(p <- ggplot(progSurvey.m, aes(Skill, Student)) +  geom_tile(aes(fill = Level), colour = "white"))
p + scale_fill_manual(values = c("None" = "#eff3ff", "A little" = "#bdd7e7", "Confident" = "#6baed6", "Expert" = "#2171b5"))
