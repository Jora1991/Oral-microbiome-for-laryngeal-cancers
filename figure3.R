#wilcoxon test
data <- read.delim('g_tax_rel.sort.screen.txt', row.names = 1, sep = '\t', head = TRUE, check.names = FALSE)[,group %>% dplyr::filter(Total == "Tumor_tissue") %>% rownames()]
sample1=rownames(group[which(group[,"Total"]== "Tumor_tissue"),,drop=F])
sample2=rownames(group[which(group[,"Total"]== "Ctl_tissue"),,drop=F])
wilcoxon.P <- apply(data,1,function(x) {if(sum(x[c(sample1,sample2)]) == 0){return('NA')}else{test <- wilcox.test(x[sample1],x[sample2],conf.int = T);p <- test$p.value;return(p);}}) %>% as.numeric()

