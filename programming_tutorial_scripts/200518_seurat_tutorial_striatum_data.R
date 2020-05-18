

##### Seurat Tutorial/Walkthrough from: https://satijalab.org/seurat/v3.1/pbmc3k_tutorial.html
##### using Gocke 2016 striatum data


### start by clearing global workstation, installing and importing necessary packages, and setting WD

rm(list = ls())
library(dplyr)
library(reshape2)
library(useful)

## install.packages('Seurat')
library(Seurat)

setwd(choose.dir())


### next up - import our data
df <- read.csv('gocke2016_taxonomy_mouse_striatum_GSE82187.csv')

### to create a seurat object, we need an unlabelled, barebones expression matrix (rows = genes, columns = cells), so we'll need to drop a few columns
### from this dataframe

corner(df)
df <- subset(df, select = -c(1, 2, 3, 4, 5))


### now to create our Seurat object. We need to pass it our transposed dataframe as seurat recognizes variables (rows) as discrete samples
s_obj <- CreateSeuratObject(counts = t(df), project = 'striatum')

### just printing out the object to the terminal will show you whether it worked or not!
s_obj


###### QUALITY CONTROL STEPS ######

### identify percent of RNA that is mitochondrial (i.e. cell is dead)
s_obj[["percent_Mito"]] <- PercentageFeatureSet(s_obj, pattern = "^MT-")

### visualize QC metrics as a violin plot
VlnPlot(s_obj, features = c("nFeature_RNA", "nCount_RNA", 'percent_Mito'), ncol = 3)


### featureScatter is typically used to visualize feature-feature relationships, but can be used
### for anything calculated by the object, i.e. columns in object metadata, PC scores etc.
plot1 <- FeatureScatter(s_obj, feature1 = "nCount_RNA", feature2 = "percent_Mito")
plot2 <- FeatureScatter(s_obj, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2


### now we can subset the data based on these observations and general standard practice
s_obj <- subset(s_obj, nFeature_RNA > 200 & percent_Mito < 5)



###### NORMALIZE AND IDENTIFY VARIABLE GENES ######

### use the Seurat NormalizeData function to scale and log normalize expression (mathematical details can be found in documentation)
s_obj <- NormalizeData(s_obj, normalization.method = "LogNormalize", scale.factor = 10000)

### now it is time to find some variable features(i.e. genes that are expressed highly in subsets of the samples/cells)
s_obj <- FindVariableFeatures(s_obj, selection.method = 'vst', nfeatures = 2000)

head(VariableFeatures(s_obj), 10)

### plot these variable features
plot1 <- VariableFeaturePlot(s_obj)
plot2 <- LabelPoints(plot = plot1, points = head(VariableFeatures(s_obj), 50), repel = TRUE)
plot2


### scaling the data 
s_obj <- ScaleData(s_obj, features = rownames(s_obj))



### Run PCA on our newly scaled data, specifically feeding it the variable features (genes) present in the dataset
s_obj <- RunPCA(s_obj, features = VariableFeatures(object = s_obj))

### show the most variable  features between the 1st and second principal component
VizDimLoadings(s_obj, dims = 1:2, reduction = "pca")

### plot the cells on a 2D PCA plot
DimPlot(s_obj, reduction = "pca")

### view top variable components from top 500 most variable cells from that principle component (both close and far away from) in a heatmap
DimHeatmap(s_obj, dims = 1, cells = 500, balanced = TRUE)

DimHeatmap(s_obj, dims = 1:15, cells = 500, balanced = TRUE)


### another method for visualizing how many principal components are actually accounted for in the dataset
ElbowPlot(s_obj)

### now we need to choose the number of principal components to include in downstream analyses
### the authors recommend using more rather than less, I chose 12 here
s_obj <- FindNeighbors(s_obj, dims = 1:9)
s_obj <- FindClusters(s_obj, resolution = 0.5)


### these functions allow us to calculate non-linear dimensionality reductions UMAP and TSNE on the dataset, 
### which we can then access and plot with some of the other tools
s_obj <- RunUMAP(s_obj, dims = 1:9)
s_obj <- RunTSNE(s_obj, dims = 1:9)


### note that you can set `label = TRUE` or use the LabelClusters function to help label
### individual clusters
DimPlot(s_obj, reduction = "tsne", label = TRUE)


### also, as a reminder, these are NORMAL ggplots and you can adjust them in most ways you could adjust
### any other ggplot object!
DimPlot(s_obj, reduction = "tsne", label = TRUE)+
  theme(
    axis.title.x = element_text(size = 50)
  )



### this plot will show us which clusters express Il34 
VlnPlot(s_obj, features = c("Gad1"))

FeaturePlot(s_obj, reduction = 'tsne', features = c("Snap25", 'Il34', 'Csf1r', 'Ccl2', 'Cx3cr1', 'Nrxn1', 'Gad1', 'Xist', 'Aqp4'))


### Find and visualize clusters based on what they used to distinguish the cell types in the original publication

FeaturePlot(s_obj, reduction = 'tsne', features = c("Snap25", 'Aqp4', 'Mog', 'Flt1', 'Cx3cr1', 'Ccl2', 'Tmem119', 'Slc17a6'))

### find all markers of cluster 1
cluster1.markers <- FindMarkers(s_obj, ident.1 = 1, min.pct = 0.25)
head(cluster1.markers, n = 5)

### concatenate genes that contribute to the different cluster
s_obj.markers <- FindAllMarkers(s_obj, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
s_obj.markers %>% group_by(cluster) %>% top_n(n = 2, wt = avg_logFC)

### this pipeline can help you to visualize potential overlap between clusters, and identify what the overlapping clusters may represent
top10 <- s_obj.markers %>% group_by(cluster) %>% top_n(n = 10, wt = avg_logFC)
DoHeatmap(s_obj, features = top10$gene) + NoLegend()

