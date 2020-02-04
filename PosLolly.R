PosLolly <-function(TweeSeizoenen){
  print(SpeelRonde)
  dat1 <- transform(TweeSeizoenen, min = pmin(`Position 18/19`, `Position 18/19`))
  dat2 <- transform(TweeSeizoenen, max = pmax(`Position 19/20`, `Position 19/20`))
  p <- ggplot(TweeSeizoenen) +
    
    geom_segment( aes(x=Team, xend=Team, y=`Position 18/19`, yend=`Position 19/20`), color=TweeSeizoenen$Color, size = 2.3,alpha = 0.5,arrow = arrow(length = unit(0.5, "cm"))) +
    geom_point( aes(x=Team, y=`Position 18/19`), color=TweeSeizoenen$Color, size=5 ) +
    geom_point( aes(x=Team, y=`Position 19/20`), color=TweeSeizoenen$Color, size=5, alpha = 1 ) + theme_light() + 
    theme(legend.position = "none", panel.border = element_blank()) +
    xlab("") +
    theme_fivethirtyeight() +
    ylab("Points") +
    scale_y_reverse(breaks = round(seq(min(dat1$min), max(dat2$max), by = 2),1)) +
    theme(axis.text.x = element_text( 
                                     size=10, angle=30,vjust = 0.75)) +
    
    labs(title = "Position difference with last season",
         subtitle = paste0("Match day: ",SpeelRondeEre),caption = "Made by @RobinWilhelmus in RStudio") +
    geom_text(aes(x=  TweeSeizoenen$`Position 19/20`, y = TweeSeizoenen$`Position 19/20`,label = TweeSeizoenen$`Position 19/20` ), color = "white", size = 3) +
    geom_text(aes(x=  TweeSeizoenen$Team, y = TweeSeizoenen$`Position 18/19`,label = TweeSeizoenen$`Position 18/19` ), color = "white", size = 3)
  p
  return(p)
}
