# R code to make the chart3 graph:
setwd("~/Desktop/github/EDAV_Project_1")
survey <- read.csv("SurveyResponse_updated.csv")

clean <- function(x,y,col){
  col[grep(x,col,ignore.case = TRUE)] <- y
  return(col)
}

#clean Program
summary(survey$Program)
survey$Program <- clean("QMSS","QMSS (master)",survey$Program)
survey$Program <- clean("Ms in ds","Data Science",survey$Program)
survey$Program <- clean("PhD","Ph.D.",survey$Program)
survey$Program <- clean("MSDS","Data Science",survey$Program)
survey$Program <- clean("IDSE","Data Science",survey$Program)
survey$Program <- droplevels(survey$Program)
#clean Favoriate Code
survey$What.code.text.editor.do.you.use.most.<- as.character(survey$What.code.text.editor.do.you.use.most.)
table(survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Sublime","Sublime Text",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most.<- clean("ipy","Ipython",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("textwrangler","TextWrangler",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Java","Eclipse", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("jupyter","Jupyter", survey$What.code.text.editor.do.you.use.most.)

#see the data
View(survey)


#Deal with the missing values in "Gender":
survey$What.is.your.preferred.gender.pronoun.[which(survey$What.is.your.preferred.gender.pronoun. == "")] <- "doesn't matter"
