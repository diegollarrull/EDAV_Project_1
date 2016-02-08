df = read.csv("SurveyResponse_updated.csv")

names(df) = c("x", "wl", "program", "tools", "r_manipulation", "r_graphics", "r_research",
              "python", "vc", "databases", "frontend", "gender", "editor","pae_r_graphics", 
              "pae_r_advanced", "pae_rmarkdown" , "pae_vis" , "github")


yesno = c("wl", "r_graphics", "r_research", "python", "vc", "databases", "frontend")
dfcat = df[yesno]
mdata = melt(dfcat, id=c())
mdata$value[mdata["value"] == "yes"] = "Yes"
mdata$value[mdata["value"] == "no"] = "No"

levels(mdata$variable) = c("Wait-List", "R_Graphics", "R_Research", "Python", "VersionControl"
                           ,"Databases", "Frontend")

g = ggplot(mdata, aes(variable, fill=value)) + geom_bar() + facet_grid(~variable)
g = g + labs(title = "Skill Distribution") + theme(axis.text.x=element_blank())
g = g + theme(axis.ticks.x = element_blank())
g = g + labs(x = "Skills", y = "Count of Yes/No")
g
