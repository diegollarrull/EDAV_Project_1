
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
    SurveyResponse[i, coll] <- answer
  }
}


# Remove unnecesary column
SurveyResponse$Software.experience..the.smaller.list...Serious.javascript..angular.ember.node.. <- NULL
SurveyResponse$Programming.and.Analytical.Experiences..Serious.javascript..angular.ember.node.. <- NULL                                                       
SurveyResponse$Programming.and.Analytical.Experiences..Web.frontend..html.css.basic.js.jquery.. <- NULL                                                       


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
progSurvey = progSurvey[,1:7]
names(progSurvey) = c("x", "R", "R Graphics", "R Advanced", "R Markdown", "Matlab", "Github")
progSurvey$R <- factor(progSurvey$R,levels(progSurvey$R)[c(4,1,2,3)])
progSurvey$"R Graphics" <- factor(progSurvey$"R Graphics",levels(progSurvey$"R Graphics")[c(4,1,2,3)])
progSurvey$"R Advanced" <- factor(progSurvey$"R Advanced",levels(progSurvey$"R Advanced")[c(4,1,2,3)])
progSurvey$"R Markdown" <- factor(progSurvey$"R Markdown",levels(progSurvey$"R Markdown")[c(4,1,2,3)])
progSurvey$"Matlab" <- factor(progSurvey$"Matlab",levels(progSurvey$"Matlab")[c(4,1,2,3)])
progSurvey$"Github" <- factor(progSurvey$"Github",levels(progSurvey$"Github")[c(4,1,2,3)])
#progSurvey$R <- as.character(progSurvey$R)
progSurvey$x <- with(progSurvey, reorder(x, R))
progSurvey.m = melt(progSurvey, id.vars = "x")
names(progSurvey.m) <- c("Student", "Skill", "Level")
progSurvey.m <- progSurvey.m[order(progSurvey.m$Skill, progSurvey.m$Level),]
progSurvey.m$Student <- factor(progSurvey.m$Student,levels(progSurvey.m$Student)[progSurvey.m$Student[1:114]])
(p <- ggplot(progSurvey.m, aes(Skill, Student)) +  geom_tile(aes(fill = Level), colour = "white"))
p + theme(axis.text.x = element_text( size = rel(2.5)))
p + scale_fill_manual(values = c("None" = "#eff3ff", "A little" = "#bdd7e7", "Confident" = "#6baed6", "Expert" = "#2171b5"))


##-------------------------
## ORDERED HEATMAP - VISHAL
##-------------------------


# R code to make the heatmap graph:
progSurvey = cbind(survey$X,survey[,grep("Programming", colnames(survey))])
progSurvey = progSurvey[,1:7]
names(progSurvey) = c("x", "R", "R Graphics", "R Advanced", "R Markdown", "Matlab", "Github")
a_map = c("None"=0,"A little"=1, "Confident"=2, "Expert"=3)
A = function(x) a_map[as.character(x)]
n = cbind(apply(progSurvey[2:7], 2, A))
score = apply(n, 1, sum)
progSurvey$score = score
progSurvey$x <- with(progSurvey, reorder(x, -score))
#progSurvey <- progSurvey[order(score),]
progSurvey$score = NULL
progSurvey.m = melt(progSurvey, id.vars = "x")
names(progSurvey.m) <- c("Student", "Skill", "Level")
(p1 <- ggplot(progSurvey.m, aes(Skill, Student)) +  geom_tile(aes(fill = Level), colour = "white") + labs(title="Student_Skills_Heatmap"))
p1 + theme(axis.text.y = element_text( size = rel(2))) + theme(axis.text.x = element_text( size = rel(2))) + scale_fill_manual(values = c("None" = "#eff3ff", "A little" = "#bdd7e7", "Confident" = "#6baed6", "Expert" = "#2171b5"))




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
yesno = c("r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[yesno]
mdata = melt(dfcat, id=c())
mdata = clean_values(mdata)

levels(mdata$variable) = c("R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")
g = ggplot(mdata, aes(variable, fill=value)) + geom_bar(colour="black") + facet_grid(~variable)
g = g + labs(title = "Skill Distribution") + theme(axis.text.x=element_blank())
g = g + theme(axis.ticks.x = element_blank())
g = g + labs(x = "Skills", y = "Count of Yes/No")
g = g + theme(strip.text.x = element_text(size = 14))
#g = g + theme(axis.text.x = element_text(size=10, vjust=0.5))
g = g + theme(axis.title.y = element_text(size=15, vjust=0.5))
#g = g + scale_fill_manual(values=c("darkgoldenrod1", "darkolivegreen4"))
g = g + scale_fill_brewer()
g = g + theme(legend.title=element_blank())
g


### plot 2
subset = c("program", "r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[subset]
mdata = melt(dfcat, id=c("program"))
levels(mdata$variable) = c("R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")
mdata = clean_values(mdata)
g1 = ggplot(mdata, aes(variable, fill=value)) + geom_bar() + facet_wrap(~program, ncol=3)
g1 = g1 + labs(x = "Skills", y = "Count of Yes/No", title = "Department vise skill distribution")
g1 = g1 + theme(axis.text.x = element_text(size=10, vjust=0.5))
g1 = g1 + theme(axis.title.y = element_text(size=15, vjust=0.5))
g1 = g1 + theme(plot.title = element_text(size=20))
g1 = g1 + scale_fill_manual(values=c("darkgoldenrod1", "darkolivegreen4"))
g1 = g1 + theme(legend.title=element_blank())
g1

### plot 3
mdata$program = as.character(mdata$program)
mdata$program[mdata$program == "Data Science Certification"] = "DS Cert"
g2 = ggplot(mdata, aes(program, fill=value)) + geom_bar() + facet_wrap(~variable, nrow=4)
g2 = g2 +labs(x = "Program", y = "Count of Yes/No", title="Facet is Skill")
g2 = g2 + theme(axis.title.y = element_text(size=20, vjust=0.5))
g2 = g2 + theme(plot.title = element_text(size=50))
g2 = g2 + scale_fill_brewer(palette="Spectral") + theme_dark()
g2 = g2 + theme(legend.title=element_blank())
g2

## pie
g2 + coord_polar(theta = "y")
g2


### plot 4

df$editor = as.character(df$editor)
# clean editors
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

g4 = ggplot(df, aes(x=reorder(editor, -table(editor)[editor]))) + geom_bar(fill="gold")
g4 = g4 + labs(x="Editor", y="Frequency", title="Historgram(editors)")
g4 = g4 + theme(axis.text.x=element_text(size=15, vjust=0.5))
g4 = g4 + theme(axis.title.y=element_text(size=15, vjust=0.5))
g4 = g4 + theme(plot.title = element_text(size=20))
g4 = g4 + scale_color_brewer()

dfcat = df[c("editor", "program")]
# replace editors with freq 5 into "Others" category
t = table(dfcat$editor)
for (i in 1:nrow(t)) {
  if (as.numeric(t[i]) < 5) {
    dfcat$editor[dfcat$editor == names(t)[i]] = "Other"
  }
}

g5 = ggplot(dfcat, aes(x=reorder(program, -table(program)[program]), fill=editor)) + geom_bar() + scale_fill_brewer()
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

##-------------------------
## DATA PROCESSING 
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
    # print(SurveyResponse[i,coll])
    SurveyResponse[i, coll] <- answer
    # print(SurveyResponse[i,coll])
  }
}


# Remove unnecesary column 
SurveyResponse$Software.experience..the.smaller.list...Serious.javascript..angular.ember.node.. <- NULL
write.csv(SurveyResponse, "./SurveyResponse_updated.csv")
survey <- read.csv("./SurveyResponse_updated.csv")
survey <- survey[, !apply(is.na(survey), 2, all)] #remove columns will all "NA".

##-------------------------
# Data Cleanning
##-------------------------

#change colnames to shorter version:
colnames(survey) <- c("X","Waiting List", "Program","Tools", "pae-R.data manipulation","R graphics","Reproducible research", "Python","Version Control","Databases","Web Frontend","Gender Preferrence", "Favorite Editor","pae-R.graphic basics","pae-R.advanced","pae-R.Markdown","pae-Matlab","pae-Github","Matlab")

clean <- function(x,y,col){
  col[grep(x,col,ignore.case = TRUE)] <- y
  return(col)
}

#clean similar entries in "Program"
survey$Program <- as.character(survey$Program)
survey$Program <- clean("QMSS","QMSS (master)",survey$Program)
survey$Program <- clean("Ms in ds","Data Science",survey$Program)
survey$Program <- clean("PhD","Ph.D.",survey$Program)
survey$Program <- clean("MSDS","IDSE (master)",survey$Program)
survey$Program <- clean("Data Science","IDSE (master)",survey$Program)
survey$Program <- clean("Math","Other masters",survey$Program)      # only one person
survey$Program <- droplevels(as.factor(survey$Program))
#reorder from most people to least people:
p <-as.data.frame(summary(survey$Program))
sort <- sort.int(p$`summary(survey$Program)`,decreasing = TRUE,index.return = TRUE)
survey$Program <- factor(survey$Program,levels(survey$Program)[sort$ix])

#clean similar entries in "Favoriate Editor"
survey$`Favorite Editor` <- as.character(survey$`Favorite Editor`)
survey$`Favorite Editor` <- clean("Sublime","Sublime Text",survey$`Favorite Editor`)
survey$`Favorite Editor`<- clean("ipy","Ipython",survey$`Favorite Editor`)
survey$`Favorite Editor` <- clean("wrangler","Text Wrangler",survey$`Favorite Editor`)
survey$`Favorite Editor` <- clean("Java","Eclipse", survey$`Favorite Editor`)
survey$`Favorite Editor` <- clean("jupyter","Jupyter", survey$`Favorite Editor`)
survey$`Favorite Editor` = clean("haven't", "None", survey$`Favorite Editor`)
survey$`Favorite Editor`[is.na(survey$`Favorite Editor`)] = "None"
survey$`Favorite Editor` = clean("Eclipse ", "Eclipse", survey$`Favorite Editor`)
survey$`Favorite Editor` = clean("pycharm", "Pycharm", survey$`Favorite Editor`)
survey$`Favorite Editor` = clean("python", "Ipython", survey$`Favorite Editor`)

# clssify any "Editor" used by less than 3 people as others:
t = as.data.frame(table(survey$`Favorite Editor`))
others <- t[t$Freq<3,]
for (i in 1:nrow(others)){
  survey[survey$`Favorite Editor` == others$Var1[i],13] = "Others"
}
survey$`Favorite Editor` <- as.factor(survey$`Favorite Editor`)
survey$`Favorite Editor` <- droplevels(survey$`Favorite Editor`)

# reorder "Favoriate Editor":
e <-as.data.frame(summary(survey$`Favorite Editor`))
colnames(e) <- "Editor"
sort2 <- sort.int(e$Editor,decreasing = TRUE,index.return = TRUE)
survey$`Favorite Editor` <- factor(survey$`Favorite Editor`,levels(survey$`Favorite Editor`)[sort2$ix])

#Deal with the missing values in "Gender":
survey$`Gender Preferrence`[which(survey$`Gender Preferrence` == "")] <- "doesn't matter"
##-------------------------
## Plot 1.HEATMAP 
##-------------------------

# R code to make the heatmap graph:
progSurvey = cbind(survey$X,survey[,grep("pae", colnames(survey))])
names(progSurvey) = c("x", "R", "R Graphics", "R Advanced", "R Markdown", "Matlab", "Github")
progSurvey$R <- as.character(progSurvey$R)
progSurvey$x <- with(progSurvey, reorder(x, R))
progSurvey.m = melt(progSurvey, id.vars = "x")
names(progSurvey.m) <- c("Student", "Skill", "Level")
(p <- ggplot(progSurvey.m, aes(Skill, Student)) +  geom_tile(aes(fill = Level), colour = "white"))
p + scale_fill_manual(values = c("None" = "#eff3ff", "A little" = "#bdd7e7", "Confident" = "#6baed6", "Expert" = "#2171b5"))


##-------------------------
# Plot 2. software experience vs. waiting list
##-------------------------
df = survey
skill <- names(df)[c(6:11,19)]
p2 = df[,c(2,6:11,19)]
p2 = melt(p2, id=c("Waiting List"))
p2$value = factor(p2$value, levels(p2$value)[c(2,1)])
levels(p2$`Waiting List`) = c("Student Enrolled","On Waiting List")
levels(p2$variable) = skill
colnames(p2) <- c("Waiting List","Software Experience","Count Y/N")

#plot
ggplot(p2, aes(reorder(`Software Experience`,`Software Experience`,function(x)-length(x)), fill= `Count Y/N`)) +
  geom_bar()+ #+ facet_grid(~variable)+ 
  facet_grid(`Waiting List`~.,scales = "free_y")+
  scale_fill_manual(values  = c("no" = "#a6bddb", "yes" = "#2171b5"))+
  theme(axis.ticks.x = element_blank())+
  labs(x = "Skills", y = "Count of Yes/No",title = "Software Experience with Waiting List")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="With experience(Y/N)"))+
  theme(legend.title = element_text(size=10))


##-------------------------
# Plot 3. software experience vs. Program
##-------------------------
subset = names(df)[c(3,6:11)]
p3 = df[subset]
p3 = melt(p3, id=c("Program"))
levels(p3$variable) = skill
p3$value = factor(p3$value, levels(p3$value)[c(2,1)])

#plot
ggplot(p3, aes(reorder(variable,variable,function(x)-length(x)), fill=value)) + 
  geom_bar() +
  facet_grid(Program~.,scales = "free_y")+
  scale_fill_manual(values  = c("no" = "#a6bddb", "yes" = "#2171b5"))+
  labs(x = "Skills", y = "Count of Yes/No", title = "Software Experience with Programs")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="With experience(Y/N)"))+
  theme(legend.title = element_text(size=10))


##-------------------------
# Plot 4. Favorite Editor vs. Program
##-------------------------

p4 = df[c("Favorite Editor", "Program")]
p4$`Favorite Editor` <- as.character(p4$`Favorite Editor`)

#plot
ggplot(p4, aes(x=reorder(Program, -table(Program)[Program]), fill=`Favorite Editor`)) +
  geom_bar()+
  scale_fill_brewer(palette = 1,direction = -1)+ 
  labs(x="Program", y="Frequency", title="Editor Preference with Program")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="Favorite Editor"))+
  theme(legend.title = element_text(size=10))

##-------------------------
# Plot 6. Proportion of students with cerntain software experence in each program 
##-------------------------
p6 <- survey[,c(1,3,6:11,19)]
prog <- levels(p6$Program)
softname <- c("R, graphics","Reproducible","Python","Version Control","Databases", "Web frontend","Matlab")
colnames(p6)<- c("id","program",softname)
p6 <- melt(p6,id = c("id","program"))
colnames(p6) <- c("id","Program","Software experience", "Y/N")
p6 <- p6[p6$`Y/N`=="yes",]
soft_back <- as.data.frame(matrix(data = NA, nrow = length(prog),ncol = 9))
colnames(soft_back) <- c("program",softname,"total")
soft_back$program <-prog
soft_back$total <- summary(survey$Program)
for (i in 1:length(prog)){
  soft_back$`R, graphics`[i] <- sum(p6$Program == prog[i] & p6$`Software experience` == softname[1])
  soft_back$Reproducible[i]<- sum(p6$Program == prog[i] & p6$`Software experience` == softname[2])
  soft_back$Python[i]<- sum(p6$Program == prog[i] & p6$`Software experience` == softname[3])
  soft_back$`Version Control`[i] <- sum(p6$Program == prog[i] & p6$`Software experience` == softname[4])
  soft_back$Databases[i] <- sum(p6$Program == prog[i] & p6$`Software experience` == softname[5])
  soft_back$`Web frontend`[i] <- sum(p6$Program == prog[i] & p6$`Software experience` == softname[6])
  soft_back$Matlab[i] <- sum(p6$Program == prog[i] & p6$`Software experience` == softname[7])
}

soft_back <- melt(soft_back,id = c("program","total"))
soft_back$percent <- round(soft_back$value/soft_back$total,2) 

#plot
ggplot(soft_back, aes(x= program))+
  geom_bar(aes(y = percent,fill= variable ),position = "dodge",stat = "identity")+
  scale_fill_brewer(palette = 1)+
  labs(x = "Software Experience", y = "Proportion of Yes within each Program", title = "Proportion of students with cerntain software experence")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="Software Experience"))+
  theme(legend.title = element_text(size=10))


##-------------------------
# More basic graphs: 1. waiting list with program
##-------------------------
ggplot(survey, aes(Program, fill = `Waiting List`)) +
  geom_bar()+
  scale_fill_brewer(palette = 1,direction = -1)+
  labs(x = "Program", y = "count Y/N on waitingl list", title ="Number of students on Waiting list within each program")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="Wheather on Wait list"))+
  theme(legend.title = element_text(size=10))
##-------------------------
# More basic graphs: 2. gender preferrence with program
##-------------------------
ggplot(survey, aes(Program, fill = `Gender Preferrence`)) +
  geom_bar()+
  scale_fill_brewer(palette = 1,direction = -1)+
  labs(x = "Program", y = "count", title = "Gender Preferrence with Program")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="Gender Preferrence"))+
  theme(legend.title = element_text(size=10))

##-------------------------
# More basic graphs: 3. code editor preferrence with program
##-------------------------
ggplot(survey, aes(Program, fill = `Favorite Editor`)) +
  geom_bar()+
  scale_fill_brewer(palette = 1,direction = -1)+
  labs(x = "Program", y = "count", title = "Code Editor Preferrence with Program")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))+
  guides(fill=guide_legend(title="Code Editor"))+
  theme(legend.title = element_text(size=10))
##-------------------------
# More basic graphs: 4. code editor preferrence 
##-------------------------
ggplot(df, aes(x=reorder(`Favorite Editor`, -table(`Favorite Editor`)[`Favorite Editor`]))) + 
  geom_bar(fill="blue")+
  labs(x="Editor", y="Frequency", title="Historgram(editors)")+
  theme(axis.text.x = element_text(size=10, vjust=0.5))+
  theme(axis.title.y = element_text(size=10, vjust=0.5))+
  theme(plot.title = element_text(size=15))


