setwd("~/Desktop/github/EDAV_Project_1")
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
names(survey)

#remove repeat columns:
survey <- survey[,-c(11,28,29,31:36,38)]
names(survey)

#strip "Experience with tools into list"
survey$Experiences.with.tools <- as.character(survey$Experiences.with.tools)
survey$Experiences.with.tools <- strsplit(survey$Experiences.with.tools,",")

#Fill in columns:
for(i in 1:dim(survey)[1])
{
  tools <- array()
  #remove the head and tail space:
  tools<- stri_trim(survey$Experiences.with.tools[[i]])
  
  if(survey$Programming.and.Analytical.Experiences..R..graphic.basics..base..lattice..grid.etc.... [i]!= "None" || any(tools == "ggplot2")){
    survey$Software.experience..the.smaller.list...R..graphics..base..lattice..ggplot2..or.grid..[i] <- "Yes"
  } 
    
  if(any(tools == "Sweave/knitr")){    #Python == "ipnb"? 
    survey$Software.experience..the.smaller.list...Reproducible.research..sweave..knitr..ipnb..etc...[i] <- "Yes"
  }
  
  if(any(tools == "Python")){
    survey$Software.experience..the.smaller.list...Python.[i] <- "Yes"
  }
  
  if(any(tools == "Github")){
    survey$Software.experience..the.smaller.list...Version.control..git..mercurial..subversion..etc...[i] <- "Yes"
    survey$Experiences.with.tools..Github.[i] <- "Yes"
  }
  
  if(any(tools == "Excel"|| tools == "SQL")){
    survey$Software.experience..the.smaller.list...Databases..any..[i] <- "Yes"
  }
  
  if(any(tools == "XML"||tools == "Web: html css js")){
    survey$Software.experience..the.smaller.list...Web.frontend..html.css.basic.js.jquery.. [i] <- "Yes"
  }
  
  
  if(any(tools == "Matlab")){
    survey$Experiences.with.tools..Matlab.[i] <- "Yes"
  }
  
  if(survey$Programming.and.Analytical.Experiences..R..data.manipulation.and.modeling.[i] != "None"){
    survey$Experiences.with.tools..R..advanced..multivariate.data.analysis..e.g..spatiotemporal.data..visualization.and.modeling..[i] <- "Yes"
    survey$Software.experience..the.smaller.list...R..data.manipulation.and.modeling.[i] <- "Yes"
  }
  
  if(survey$Programming.and.Analytical.Experiences..Reproducible.documentation.with.R..e.g..R.Markdown..[i] != "None"){
    survey$Experiences.with.tools..Reproducible.documentation.with.R..e.g..R.Markdown..[i] <-"Yes"
  }
  
  if(survey$Programming.and.Analytical.Experiences..Matlab..data.manipulation..analysis..visualization.and.modeling.[i] != "None"){
    survey$Experiences.with.tools..Matlab..data.manipulation..analysis..visualization.and.modeling.[i] <- "Yes"
  }
  

  if(length(tools) != 0){
    survey$Experiences.with.tools[i] <- "Yes"
  }
  
}

# survey[is.na(survey)] <- "No"

#Deal with the missing values in "Gender":
survey$What.is.your.preferred.gender.pronoun.[which(survey$What.is.your.preferred.gender.pronoun. == "")] <- "doesn't matter"
