PosSegmentKKD <- function(TweeSeizoenen){
  TweeSeizoenen$Team <- gsub("\n", "/", TweeSeizoenen$Team)
  left_label <- paste(TweeSeizoenen$Team, TweeSeizoenen$`Position 18/19`)
  right_label <- paste(TweeSeizoenen$`Position 19/20`, TweeSeizoenen$Team)
  
  
p <- ggplot(TweeSeizoenen) + geom_segment( aes(x=1, xend=2, y=`Position 18/19`, yend=`Position 19/20`, size=.05), color = TweeSeizoenen$Color2, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.05) + 
  geom_vline(xintercept=2, linetype="dashed", size=.05) + 
  
  labs(x="", y="") + 
  
  
  geom_text(aes(label=left_label, y=TweeSeizoenen$`Position 18/19`), x=1, hjust=1, size=3.5, color = "black") +
  geom_text(aes(label=right_label, y=TweeSeizoenen$`Position 19/20`), x=2, hjust=0, size=3.5, color= "black") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))  + scale_y_reverse() +
  scale_x_continuous(limits = c(0.9,2.1)) +
  
  geom_text(label=paste("After Match Day",SpeelRondeKKD), x=1.5, y=2, vjust=8, size=4, color = "black") +
  geom_text(label="18/19", x=1, y=2, vjust=8, size=4, color = "black") +
  geom_text(label="19/20", x=2, y=2, vjust=8, size=4, color = "black") + annotate(geom="text", x=2, y=21, label="Made by @RobinWilhelmus in RStudio",
                                                                 color="black", size = 3)

print(p)
return(p)
}
