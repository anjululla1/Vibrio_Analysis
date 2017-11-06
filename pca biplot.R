library("DESeq2")
countdata<- read.table("/Users/alulla/Desktop/Data.txt", header = TRUE, row.names = 1)
countdata<- as.matrix(countdata)
head(countdata)
type<-factor(c(rep("HS",2),rep("ASW",2),rep("HS",2),rep("ASW",2),rep("HS",2),rep("ASW",2),rep("HS",2),rep("ASW",2)))
condition<-factor(c(rep("CMCP6",4),rep("YJ016",4),rep("JY1701",4),rep("JY1305",4)))
colData<- data.frame(row.names = colnames(countdata), condition, type)
colData
design(dds)<-formula(~type + condition)
dds<- DESeqDataSetFromMatrix(countData = countdata, colData = colData, design = ~type + condition)
dds
rld<- rlogTransformation(dds)
data <- plotPCA(rld, intgroup=c("condition", "type"), returnData=TRUE)
percentVar <- round(100 * attr(data, "percentVar"))
library("ggplot2")
ggplot(data, aes(PC1, PC2, color=condition, shape=type)) +
  geom_point(size=3) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance"))