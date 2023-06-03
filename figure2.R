dat1 <- read.delim('g_tax_rel.sort.screen.txt', row.names = 1, sep = '\t', head = TRUE, check.names = FALSE)[,group %>% dplyr::filter(Total == "Tumor_tissue") %>% rownames()]
dis_bray <- vegan::vegdist(t(dat1), method = 'bray')
tree1 <- hclust(dis_bray, method = 'average')
dat1.plot <- dat1 %>% tibble::rownames_to_column(var="Tax") %>% reshape2::melt(by="Tax") 
dat1.plot$Tax <- factor(dat1.plot$Tax,levels = rownames(dat1) %>% rev)  
ggplot(dat1.plot,aes(variable,weight=value,fill=Tax)) + geom_bar() + xlim(colnames(dat1)[tree1$order]) + theme_classic() + theme(axis.text.x = element_text(angle = 45,hjust = 1)) + xlab("Tumor_tissue") + ylab("Relative Abundance") + guides(fill = guide_legend( ncol = 2, byrow = F))