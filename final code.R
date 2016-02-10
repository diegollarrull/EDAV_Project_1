
#PACKAGES TO INSTALL IF NOT IN THE SYSTEM
#install.packages("devtools")
#install_github('rCharts', 'ramnathv')(edited)

library(ggplot2)
library(scales)
library(plyr)
library(reshape)
require(devtools)
require(rCharts)


# XIAOWEI'S' PATH
#setwd("~/Desktop/github/EDAV_Project_1")
# DIEGO'S PATH
setwd("~/Google Drive/Data Science/Courses/Spring 2016/Exploratory Data Analysis and Visualization/Assignments/EDAV_Project_1/")


##-------------------------
## DATA PROCESSING - DIEGO
##-------------------------


SurveyResponse <- read.csv("./Survey+Response.csv")

Exprs = c("html|css|basic js|jquery", "Matlab", "R",
          "knitr", "Python", "Github", "SQL")
Cols = c("Software.experience..the.smaller.list...Web.frontend..html.css.basic.js.jquery..",
         "Experiences.with.tools..Matlab.",
         "Software.experience..the.smaller.list...R..graphics..base..lattice..ggplot2..or.grid..",
         "Software.experience..the.smaller.list...Reproducible.research..sweave..knitr..ipnb..etc...",
         "Software.experience..the.smaller.list...Python.",   
         "Software.experience..the.smaller.list...Version.control..git..mercurial..subversion..etc...",
         "Software.experience..the.smaller.list...Databases..any..")

exprcols = cbind(Exprs,Cols)

for (j in 1:nrow(exprcols)){ 
  coll = names(SurveyResponse) %in% exprcols[j,2]
  for (i in 1:nrow(SurveyResponse)){
    Gr =  grep(exprcols[j,1], as.character(SurveyResponse$Experiences.with.tools[i]))
    if (length(Gr) > 0){
      answer = "yes"
    }
    else{
      answer = "no"
    }
    SurveyResponse[i, coll] <- as.character(SurveyResponse[i,coll])
    print(SurveyResponse[i,coll])
    SurveyResponse[i, coll] <- answer
    print(SurveyResponse[i,coll])
  }
}


# Remove unnecesary column
SurveyResponse$Software.experience..the.smaller.list...Serious.javascript..angular.ember.node.. <- NULL

write.csv(SurveyResponse, "./SurveyResponse_updated.csv")
survey <- read.csv("./SurveyResponse_updated.csv")


##-------------------------
# R code to clean similar entries - XIAOWEI
##-------------------------

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
survey$What.code.text.editor.do.ylou.use.most. <- clean("jupyter","Jupyter", survey$What.code.text.editor.do.you.use.most.)

#Deal with the missing values in "Gender":
survey$What.is.your.preferred.gender.pronoun.[which(survey$What.is.your.preferred.gender.pronoun. == "")] <- "doesn't matter"







##-------------------------
## HEATMAP - DIEGO
##-------------------------


# R code to make the heatmap graph:
progSurvey = cbind(survey$X,survey[,grep("Programming", colnames(survey))])
names(progSurvey) = c("x", "R", "R Graphics", "R Advanced", "R Markdown", "Matlab", "Github")
progSurvey$x <- with(progSurvey, reorder(x, R))
progSurvey.m = melt(progSurvey, id.vars = "x")
names(progSurvey.m) <- c("Student", "Skill", "Level")
(p <- ggplot(progSurvey.m, aes(Skill, Student)) +  geom_tile(aes(fill = Level), colour = "white"))
p + scale_fill_manual(values = c("None" = "#eff3ff", "A little" = "#bdd7e7", "Confident" = "#6baed6", "Expert" = "#2171b5"))


##-------------------------
# R code to make the chart3 graph - XIAOWEI
##-------------------------

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


##-------------------------
# Additional plots - VISHAL
##-------------------------


df = survey

clean <- function(x,y,col){
  col[grep(x,col,ignore.case = TRUE)] <- y
  return(col)
}

clean_values <- function(mdata){
  mdata$value[mdata["value"] == "yes"] = "Yes"
  mdata$value[mdata["value"] == "no"] = "No"
  return(mdata)
}

#clean Program
df$Program <- clean("QMSS","QMSS (master)",df$Program)
df$Program <- clean("Ms in ds","Data Science",df$Program)
df$Program <- clean("PhD","Ph.D.",df$Program)
df$Program <- clean("MSDS", "IDSE (master)", df$Program)
df$Program[df$Program == "Data Science"] = "IDSE (master)"

# assign names
names(df) = c("x", "wl", "program", "tools", "r_manipulation", "r_graphics", "r_research",
              "python", "vc", "databases", "frontend", "gender", "editor","pae_r_graphics", 
              "pae_r_advanced", "pae_rmarkdown" , "pae_vis" , "github")



### plot 1
yesno = c("wl", "r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[yesno]
mdata = melt(dfcat, id=c())
clean_values(mdata)

levels(mdata$variable) = c("Wait-List", "R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")
g = ggplot(mdata, aes(variable, fill=value)) + geom_bar() + facet_grid(~variable)
g = g + labs(title = "Skill Distribution") + theme(axis.text.x=element_blank())
g = g + theme(axis.ticks.x = element_blank())
g = g + labs(x = "Skills", y = "Count of Yes/No")
g


### plot 2
subset = c("wl", "program", "r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[subset]
mdata = melt(dfcat, id=c("program"))
levels(mdata$variable) = c("Wait-List", "R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")
mdata = clean_values(mdata)
g1 = ggplot(mdata, aes(variable, fill=value)) + geom_bar() + facet_wrap(~program, ncol=3)
g1 = g1 + labs(x = "Skills", y = "Count of Yes/No", title = "Facet is Department")
g1

### plot 3
g2 = ggplot(mdata, aes(program, fill=value)) + geom_bar() + facet_wrap(~variable, nrow=4)
g2 = g2 +labs(x = "Program", y = "Count of Yes/No", title="Facet is Skill")
g2


### plot 4

df$editor = as.character(df$editor)
# clean editors
df$editor = clean("sublime", "SublimeText", df$editor)
df$editor = as.character(df$editor)
df$editor = clean("sublime", "SublimeText", df$editor)
df$editor = clean("ipy", "Ipython", df$editor)
df$editor = clean("wrangler", "Text Wrangler", df$editor)
df$editor = clean("Java", "Eclipse", df$editor)
df$editor = clean("jupyter", "Jupyter", df$editor)
df$editor = clean("haven't", "None", df$editor)
df$editor[is.na(df$editor)] = "None"
df$editor = clean("Eclipse ", "Eclipse", df$editor)
df$editor = clean("pycharm", "Pycharm", df$editor)
df$editor = clean("python", "Ipython", df$editor)

t = table(df$editor)

g4 = ggplot(df, aes(x=reorder(editor, -table(editor)[editor]))) + geom_bar(fill="blue")
g4 = g4 + labs(x="Editor", y="Frequency", title="Historgram(editors)")
g4 = g4 + theme(axis.text.x=element_text(size=15, vjust=0.5))
g4 = g4 + theme(axis.title.y=element_text(size=15, vjust=0.5))
g4 = g4 + theme(plot.title = element_text(size=20))
g4

dfcat = df[c("editor", "program")]
# replace editors with freq 5 into "Others" category
t = table(dfcat$editor)
for (i in 1:nrow(t)) {
  if (as.numeric(t[i]) < 5) {
    dfcat$editor[dfcat$editor == names(t)[i]] = "Other"
  }
}

g5 = ggplot(dfcat, aes(x=reorder(program, -table(program)[program]), fill=editor)) + geom_bar()
g5 = g5 + labs(x="program", y="Frequency", title="Editor Preference with Program")
g5 = g5 + theme(axis.text.x = element_text(size=15, vjust=0.5))
g5 = g5 + theme(axis.title.y = element_text(size=15, vjust=0.5))
g5 = g5 + theme(plot.title = element_text(size=20))
g5 = g5 + guides(fill=guide_legend(title="Editor"))
g5 = g5 + theme(legend.title = element_text(size=15))
g5



  

# create a dataset "skills"
#is this a duplicate from the code in lines 118-139?
skills <- survey[,c(1,14:18)]
colnames(skills) <-c("id","R, graphic basics","Github","R, advanced ","Reproducible documentation","Matlab")
skills <- melt(skills,id = "id")
colnames(skills) <- c("id","Skill","level")
#generate the plot:
ggplot(skills, aes(x= Skill,fill=level))+
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = 4)+
  labs(title = "Programming and Analytical Experience level")

# create a dataset "software" 
software <- survey[,c(1,3,6:11)]
prog <- levels(software$Program)
softname <- c("R, graphics","Reproducible","Python","Version Control","Databases", "Web frontend")
colnames(software)<- c("id","program",softname)
software <- melt(software,id = c("id","program"))
colnames(software) <- c("id","Program","Software experience", "Y/N")
software <- software[software$`Y/N`=="yes",]

ggplot(software, aes(x= `Software experience`,fill= Program))+
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = 4)+
  labs(title = "Sofware experience distribution over different programs")

soft_back <- as.data.frame(matrix(data = NA, nrow = 7,ncol = 8))
colnames(soft_back) <- c("program",softname,"total")
soft_back$program <-prog
soft_back$total <- summary(survey$Program)
for (i in 1:7){
  soft_back$`R, graphics`[i] <- sum(software$Program == prog[i] & software$`Software experience` == softname[2])
  soft_back$Reproducible [i]<- sum(software$Program == prog[i] & software$`Software experience` == softname[2])
  soft_back$Python [i]<- sum(software$Program == prog[i] & software$`Software experience` == softname[3])
  soft_back$`Version Control`[i] <- sum(software$Program == prog[i] & software$`Software experience` == softname[4])
  soft_back$Databases[i] <- sum(software$Program == prog[i] & software$`Software experience` == softname[5])
  soft_back$`Web frontend`[i] <- sum(software$Program == prog[i] & software$`Software experience` == softname[6])
}

soft_back <- melt(soft_back,id = c("program","total"))
soft_back$percent <- round(soft_back$value/soft_back$total,2) 

ggplot(soft_back, aes(x= program))+
  geom_bar(aes(y = percent,fill= variable ),position = "dodge",stat = "identity")+
  scale_fill_brewer(palette = 4)



