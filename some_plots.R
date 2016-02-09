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
df(df$Program)
df$Program <- clean("QMSS","QMSS (master)",df$Program)
df$Program <- clean("Ms in ds","Data Science",df$Program)
df$Program <- clean("PhD","Ph.D.",df$Program)

names(df) = c("x", "wl", "program", "tools", "r_manipulation", "r_graphics", "r_research",
              "python", "vc", "databases", "frontend", "gender", "editor","pae_r_graphics", 
              "pae_r_advanced", "pae_rmarkdown" , "pae_vis" , "github")


yesno = c("wl", "r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[yesno]
mdata = melt(dfcat, id=c())
clean_values(mdata)

levels(mdata$variable) = c("Wait-List", "R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")

### plot 1
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
mdata$program = clean("MSDS", "IDSE (master)", mdata$program)
mdata$program = clean("Data Science", "IDSE (master)", mdata$program)
mdata = clean_values(mdata)
g1 = ggplot(mdata, aes(variable, fill=value)) + geom_bar() + facet_wrap(~program, ncol=3)
g1 = g1 + labs(x = "Skills", y = "Count of Yes/No", title = "Facet is Department")
g1

### plot 3
g2 = ggplot(mdata, aes(program, fill=value)) + geom_bar() + facet_wrap(~variable, nrow=4)
g2 = g2 +labs(x = "Program", y = "Count of Yes/No", title="Facet is Skill")
g2
