library("pheatmap") 
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
select <- order(rowMeans(counts(dds,normalized=FALSE)),decreasing=TRUE)[1:50]
nt <- normTransform(dds) # defaults to log2(x+1)
log2.norm.counts <- assay(nt)[select,]
df <- as.data.frame(colData(dds)[,c("condition","type")])
pheatmap(assay(rld)[select,], cluster_rows=FALSE, show_rownames=TRUE,
         cluster_cols=FALSE, annotation_col=df)