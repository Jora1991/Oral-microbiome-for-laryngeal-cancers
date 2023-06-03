
  otu <- read.table("g_tax_rel.sort.screen.txt", sep = '\t', row.names = 1, header = TRUE)
  otu <- data.frame(t(otu))
  
  otu1 <- otu[group %>% dplyr::filter(Total %in% c("Tumor_tissue","Ctl_tissue")) %>% rownames(),gsub("^\\.","X.",gsub(" |\\[|\\]|-",".",otu.sign))]
  group.info <- group[rownames(otu1),"Total"]
  names(group.info) <- rownames(otu1)
  group.info <- as.vector(group.info)
  group.info[group.info=="Ctl_tissue"] <- 0
  group.info[group.info=="Tumor_tissue"] <- 1
  group.info <- as.numeric(group.info)
  group.info1 <- group.info
  otu_group1 <- cbind(otu1, group.info1)
  colnames(otu_group1)[ncol(otu_group1)] <- "group"
  otu_group1 <- data.frame(otu_group1)
  mark1 <- sample(nrow(otu_group1),nrow(otu_group1)*0.8,replace=FALSE)
  otu_train <- otu_group1[mark1,]
  otu_test <- otu_group1[-mark1,]
  test.group <- otu_test$group
  otu_train$group <- factor(otu_train$group,levels = c(0,1))
  
  #cross-validation
  otu_train.5_10 <- replicate(5, rfcv(otu_train[-ncol(otu_train)], otu_train$group, cv.fold = 10,step = 1.5), simplify = FALSE)

  #constuct RF modle based on selected variables
  model.last <- randomForest(group ~ ., data = otu_train, importance = TRUE,proximity=T,type="classification")
  importance_otu <- data.frame(importance(model.last)) %>% dplyr::arrange(-MeanDecreaseAccuracy)
  #ROC
  train.roc <- roc(otu_train$group,model.last$votes[,2],ci=TRUE,ci.alpha=0.9,plot=F)
  otu_test <- otu_test[,rownames(importance_otu)]
  test_predict <- predict(model.last, otu_test,type="prob")
  test.roc <- roc(test.group,test_predict[,2],ci=TRUE,ci.alpha=0.9,plot=F)