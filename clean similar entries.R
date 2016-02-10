# R code to make the chart3 graph:
setwd("/Users/huisu/Documents/columbia 2016Spring/EDAV/project/EDAV_Project_1")
survey <- read.csv("Survey+Response.csv")

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
survey$Program <- clean("Applied Math","Other masters",survey$Program)
survey$Program <- droplevels(survey$Program)
#clean Favoriate Code
survey$What.code.text.editor.do.you.use.most.<- as.character(survey$What.code.text.editor.do.you.use.most.)
table(survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Sublime","Sublime Text",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most.<- clean("ipy","Ipython",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("textwrangler","TextWrangler",survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Java","Eclipse", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("python","Ipython", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("jupyter","Jupyter", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Eclipse ","Eclipse", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Text Wrangler","TextWrangler", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("haven't used any","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Webstorm, pycharm","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Stata","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("xcode","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("TextMate","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Jupyter","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Atom","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- clean("Eclipse","others", survey$What.code.text.editor.do.you.use.most.)
#survey$What.code.text.editor.do.you.use.most. <- clean("Ipython","others", survey$What.code.text.editor.do.you.use.most.)
#survey$What.code.text.editor.do.you.use.most. <- clean("TextWrangler","others", survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- droplevels(survey$What.code.text.editor.do.you.use.most.)
survey$What.code.text.editor.do.you.use.most. <- as.factor(survey$What.code.text.editor.do.you.use.most.)
levels(survey$What.code.text.editor.do.you.use.most.) <- levels(survey$What.code.text.editor.do.you.use.most.)[c(1,2,4:7,3)]
#see the data
View(survey)


#Deal with the missing values in "Gender":
survey$What.is.your.preferred.gender.pronoun.[which(survey$What.is.your.preferred.gender.pronoun. == "")] <- "doesn't matter"
