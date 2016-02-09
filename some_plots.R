df = read.csv("SurveyResponse_updated.csv")

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


