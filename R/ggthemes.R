library(ggplot2)


heightcols <- c("low" = "red2", "medium" = "orange", "high" = "blue")


themepoints <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size=13, family = "serif"),
                     axis.text = element_text(size=10, family = "serif"),
                     legend.position = "top",
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     panel.background = element_blank(),
                     strip.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(size=.5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))
