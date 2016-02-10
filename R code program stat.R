ggplot(`survey`, aes(factor(Program), fill = factor(Are.you.on.the.waiting.list.))) +geom_bar()
ggplot(`Survey+Response`, aes(factor(Program), fill = factor(What.is.your.preferred.gender.pronoun.))) +geom_bar()
ggplot(`Survey+Response`, aes(factor(Program), fill = factor(What.code.text.editor.do.you.use.most.))) +
  +     geom_bar()