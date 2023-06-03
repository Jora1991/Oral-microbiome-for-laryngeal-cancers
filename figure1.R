# plots for alpha diversity
box.in <- read.table("00.rawdata/alpha_diversity_index.xls",sep="\t",header = T,row.names = 1)
ggplot(box.in, aes(Group, Abun, fill = Group)) +
    labs(x="",y="")+
    geom_violin() + geom_boxplot(col='black',size=1,width =0.1,outlier.colour = NA) + 
    theme(axis.text.x=element_text(colour = "black",size=7,angle = 45,hjust = 1), axis.text.y=element_text(colour = "black",size=7),axis.title.y=element_text(size=7),axis.title.x=element_text(size=7),legend.position = "none",axis.text = element_text(size=7)) +
    facet_wrap(vars(Factor), scales = "free",nrow = 1) +
    geom_signif(comparisons = comparedGroups,
                color="black",step_increase = 0.08,
                map_signif_level=function(p) if(p < 0.01){'+'}else{if(p < 0.05){'*'}else{sprintf("%.2f",p)}},
                test = wilcox.test)

#we dist  
we.dist <- read.table("bray_distance.txt",sep="\t",header = T,row.names = 1)
 dataTB <- as.dist(we.dist)
    adonis(dataTB~as.vector(group.info),permutations = 999)
    #PCOA
    PCOA <- pcoa(dataTB, correction="none", rn=NULL)
    result <-PCOA$values[,"Relative_eig"]
    pco1 = as.numeric(sprintf("%.3f",result[1]))*100
    pco2 = as.numeric(sprintf("%.3f",result[2]))*100
    pc = as.data.frame(PCOA$vectors)
    pc$names = rownames(pc)
    xlab=paste("PCoA1 (",pco1,"%)",sep="")
    ylab=paste("PCoA2 (",pco2,"%)",sep="")
    ggplot(pc, aes(Axis.1,Axis.2, fill=Group,color=Group,shape=Group)) +
      labs(x=xlab,y=ylab) +
      geom_hline(yintercept=0,linetype=4,color="grey") +
      geom_vline(xintercept=0,linetype=4,color="grey") +
      geom_point(size=4,alpha=0.7) +
      stat_ellipse(show.legend = F,level = 0.95)+
      theme(axis.text.x=element_text(colour = "black",angle=45,vjust=1,hjust=1,size = 7), axis.text.y=element_text(colour = "black",size = 7),panel.background = element_rect(fill="white",color="black",linetype=1,size=1),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.text = element_text(size=7))