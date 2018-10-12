library(readxl)

NPL <- read_excel("C:/Users/Chen Validus Team/inSync Share/Data Tech/NPL Study/NPL Study feature.xlsx", 
                  sheet = "Sheet1")
sample_data<-NPL[,c(1,3:10,12:14,16:19,22:24)]
sample_data<-sample_data[complete.cases(sample_data), ]

library(caret)  
library(Rtsne)

addline_format <- function(x,...){ 
  gsub('\\s','\n',x) 
} 


## Rtsne function may take some minutes to complete...
set.seed(9)  
tsne_model_1 = Rtsne(as.matrix(scale(sample_data[,-c(1,2,6,7,8,9,11,19)])), check_duplicates=FALSE, 
                     pca=FALSE, perplexity=10, theta=0.5, dims=2)

## getting the two dimension matrix
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1<-cbind(d_tsne_1,sample_data$Label,sample_data$`Borrower Name`)
colnames(d_tsne_1)<-c("V1","V2","Label","Borrower_Name")
## plotting the results without clustering
ggplot(d_tsne_1, aes(x=V1, y=V2,color=as.factor(Label),label=addline_format(Borrower_Name))) +  
  geom_point(size=5) +
  guides(colour=guide_legend(title="Status",override.aes=list(size=4))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE clustering for existing Borrowers") +
  geom_text(angle=0, size=2)+
  theme_light(base_size=15) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size=10, face="bold")) +
  scale_colour_manual(values = c("darkolivegreen3","darkgoldenrod1","firebrick3"),labels = c('Good Standing/Closed','30 DPD','90 DPD'))


## keeping original data
d_tsne_1_original=d_tsne_1

## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans=kmeans(scale(d_tsne_1[,c(1,2)]), 5)  
d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1[,c(1,2)])))

## setting 3 clusters as output
d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=5))

plot_cluster=function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster,label="Label")) +
    geom_point(size=3) +
    guides(colour=guide_legend(override.aes=list(size=4))) +
    xlab("") + ylab("") +
    ggtitle("Clustering") +
    theme_light(base_size=10) +
    geom_text(angle=0, size=2)+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "top",
          legend.box = "horizontal",
          legend.text = element_text(size = 8, face = "bold"),
          legend.title = element_text(size=10, face="bold")) + 
    scale_colour_brewer(palette = palette) 
}


plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Set1")  
plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set1")

## and finally: putting the plots side by side with gridExtra lib...
library(gridExtra)  
grid.arrange(plot_k, plot_h,  ncol=2)  







