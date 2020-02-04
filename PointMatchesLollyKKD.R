PointMatchesLollyKKD <- function(ZelfdeWedstrijden){
  dat1 <- transform(ZelfdeWedstrijden, min = pmin(`Points1920`, `Points1819`))
  dat2 <- transform(ZelfdeWedstrijden, max = pmax(`Points1920`, `Points1819`))
  print(dat1)
  print(dat2)
  
  
p <- ggplot(ZelfdeWedstrijden) + 
  geom_point( aes(x=Team3, y=Points1819), color=ZelfdeWedstrijden$Color, size=5 ) +
  geom_segment( aes(x=Team3, xend=Team3, y=Points1819, yend=Points1920), color=ZelfdeWedstrijden$Color, size = 2.3,alpha = 0.5,arrow = arrow(length = unit(0.5, "cm"))) +
  
  geom_point( aes(x=Team3, y=Points1920), color=ZelfdeWedstrijden$Color, size=5, alpha = 1 ) + theme_light() + 
  theme(legend.position = "none", panel.border = element_blank()) +
  xlab("") +
  
  #theme_tufte() +
  #dark_theme_gray() +
 
  ylab("Points") + 
  theme_fivethirtyeight() +
  #geom_text(aes(x=  ZelfdeWedstrijden$Team3, y = (ZelfdeWedstrijden$Points1920 - (ZelfdeWedstrijden$Diff/2)),label = ZelfdeWedstrijden$Diff ), color = "black", size = 3) +
  geom_text(aes(x=  ZelfdeWedstrijden$Team3, y = (ZelfdeWedstrijden$Points1920),label = ZelfdeWedstrijden$Points1920 ), color = "white", size = 3) +
  geom_text(aes(x=  ZelfdeWedstrijden$Team3, y = ZelfdeWedstrijden$Points1819),label = ZelfdeWedstrijden$Points1819, color = "white", size = 3) +
  theme(axis.text.x = element_text( 
                                   size=10, angle=30,vjust=0.75)) +

  scale_y_continuous(breaks = round(seq(min(dat1$min), max(dat2$max), by = 2),1)) +
  labs(title = "Points difference with same matches last season",
       subtitle = paste0("Number of matches: ",SpeelRondeKKD),caption = "Made by @RobinWilhelmus in RStudio"
       #,
       #subtitle = paste0("Match day: ",SpeelRonde)
  ) 

p

return(p)
}
