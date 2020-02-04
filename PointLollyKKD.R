PointLollyKKD <- function(TweeSeizoenen){
  
  TweeSeizoenen1 <- filter(TweeSeizoenen, Team %in% c("Ajax", "AZ Alkmaar", "Feyenoord","For Sittard", "Groningen", "PSV Eindhoven"))
  TweeSeizoenen$Team <- factor(TweeSeizoenen$Team, levels = TweeSeizoenen$Team[order(-TweeSeizoenen$`Points 19/20`)])
  TweeSeizoenen1$Team <- factor(TweeSeizoenen1$Team, levels = TweeSeizoenen1$Team[order(-TweeSeizoenen1$`Points 19/20`)])
  dat1 <- transform(TweeSeizoenen, min = pmin(`Points 19/20`, `Points 18/19`))
  dat2 <- transform(TweeSeizoenen, max = pmax(`Points 19/20`, `Points 18/19`))
  
  
  p <- ggplot(TweeSeizoenen) +
    
    geom_segment( aes(x=Team, xend=Team, y=`Points 18/19`, yend=`Points 19/20`), color=TweeSeizoenen$Color, size = 2.3,alpha = 0.5,arrow = arrow(length = unit(0.5, "cm"))) +
    geom_point( aes(x=Team, y=`Points 18/19`), color=TweeSeizoenen$Color, size=5 ) +
    geom_point( aes(x=Team, y=`Points 19/20`), color=TweeSeizoenen$Color, size=5, alpha = 1 ) + theme_light() + 
    theme(legend.position = "none", panel.border = element_blank()) +
    xlab("") +
    theme_fivethirtyeight() +
    
    
    theme(axis.text.x = element_text(
                                     size=10, angle=30, vjust=0.75)) +
    geom_text(aes(x=  TweeSeizoenen$Team, y = (TweeSeizoenen$`Points 19/20`),label = TweeSeizoenen$`Points 19/20` ), color = "white", size = 3) +
    geom_text(aes(x=  TweeSeizoenen$Team, y = (TweeSeizoenen$`Points 18/19`),label = TweeSeizoenen$`Points 18/19` ), color = "white", size = 3) +
    scale_y_continuous(breaks = round(seq(min(dat1$min), max(dat2$max), by = 2),1)) +
    ylab("Points") +  
    labs(title = "Points difference with last season",
         subtitle = paste0("Match day: ",SpeelRondeKKD),caption = "Made by @RobinWilhelmus in RStudio")  
  
  
  
  p#+
  
  #geom_errorbar(data = TweeSeizoenen1,aes(ymin=`Points 19/20`, ymax=`Points 19/20`+3,x=Team), width=.2,
  #                position=position_dodge(.9)) + 
  #geom_errorbar(data = TweeSeizoenen1,aes(ymin=`Points 19/20`, ymax=`Points 19/20`+1,x=Team), width=.2,
  #                                                             position=position_dodge(.9)) 
  
  return(p)
}

TweeSeizoenen$Team[1] = "Ajax"

