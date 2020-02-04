PointSegment <-function(TweeSeizoenen){

p <-  ggplot(TweeSeizoenen) + geom_segment( aes(x=1, xend=2, y=`Points 18/19`, yend=`Points 19/20`, size=.05), color = TweeSeizoenen$Color, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.05) + 
  geom_vline(xintercept=2, linetype="dashed", size=.05) + 
  
  labs(x="", y="") + 
  
  
  geom_text_repel(aes(label=left_label, y=TweeSeizoenen$`Points 18/19`), x=1, hjust=0, size=3.5) +
  geom_text_repel(aes(label=right_label, y=TweeSeizoenen$`Points 19/20`), x=2, hjust=0, size=3.5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))  +
  
  scale_x_continuous(limits = c(0.9,2.1)) +
  geom_text(label=paste("After Match Day",SpeelRonde), x=1.5, y=12.5, vjust=1, size=4) +
  geom_text(label="18/19", x=1, y=12.5, vjust=1, size=4) +
  geom_text(label="19/20", x=2, y=12.5, vjust=1, size=4) + annotate(geom="text", x=2, y=0, label="Made by @RobinWilhelmus in RStudio",
                                                                    color="black", size = 3)

return(p)
}
