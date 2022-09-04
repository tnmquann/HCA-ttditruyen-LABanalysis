library(readr)
library(dplyr)
library(ggplot2)
library(dendextend)
library(factoextra)

LAB_data <- read.csv("<add your PATH here>", header=T,row.names=1)
# which(names(LAB_data)%in%c("E.aver"))

# Create data frame for 3 categories: only pH values, only AMR values and both pH + AMR values.----
df_pH<-LAB_data[,c(3:5)]
df_amr<-LAB_data[,c(1:2,20:ncol(LAB_data))]
df_concat<-LAB_data[,c(1:2,3:5,20:ncol(LAB_data))]

#Create dendrogram---------------------------------
hc_pH <- hclust(dist(df_pH))
hc_amr<-hclust(dist(df_amr))
hc_concat<-hclust(dist(df_concat))
d1 <- df_pH %>% dist() %>% hclust() %>% as.dendrogram()
d2 <- df_amr %>% dist() %>% hclust() %>% as.dendrogram()
d3 <- df_concat %>% dist() %>% hclust() %>% as.dendrogram()
p1 <-fviz_dend(d1, k = 4,                 # Cut in four groups
          cex = 0.75,                 # label size
          main = "Hierarchical Cluster Analysis (HCA) for pH values",
          ylab = "Distance",
          k_colors = c("#2E9FDF", "#48a917", "#e5a50c", "#9a0d54"),
          color_labels_by_k = TRUE,  # color labels by groups
          repel = TRUE,
          border = 4, lty = 3, lwd = 1,
          ggtheme = theme_gray()
)
p2 <-fviz_dend(d2, k = 4,                 # Cut in four groups
               cex = 0.75,                 # label size
               main = "Hierarchical Cluster Analysis (HCA) for AMR",
               ylab = "Distance",
               k_colors = c("#2E9FDF", "#48a917", "#e5a50c", "#9a0d54"),
               color_labels_by_k = TRUE,  # color labels by groups
               repel = TRUE,
               border = 4, lty = 3, lwd = 1,
               ggtheme = theme_gray()
)
p3 <-fviz_dend(d3, k = 4,                 # Cut in four groups
               cex = 0.75,                 # label size
               main = "Hierarchical Cluster Analysis (HCA) for both pH and AMR values",
               ylab = "Distance",
               k_colors = c("#2E9FDF", "#48a917", "#e5a50c", "#9a0d54"),
               color_labels_by_k = TRUE,  # color labels by groups
               repel = TRUE,
               border = 4, lty = 3, lwd = 1,
               ggtheme = theme_gray()
)

#Export dendrogram-------
ggsave("pH.png", plot=p1, dpi=320,limitsize = FALSE)
ggsave("AMR.png", plot=p2, dpi=320,limitsize = FALSE)
ggsave("Concatenate.png", plot=p3, dpi=320,limitsize = FALSE)

#Adjust dendrogram for tanglegram--------
dl <- dendlist(
  d1 %>% 
    set("labels_col", value = c("skyblue", "orange", "royalblue4","darkgreen"), k = 4) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "royalblue4","darkgreen"), k = 4),
  d2 %>% 
    set("labels_col", value = c("skyblue", "orange", "royalblue4"), k = 3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "royalblue4"), k = 3)
)

# Plot tanglegram------
png('tanglegram_HCA.png', pointsize=10, width=4000, height=2000, res=600)
par(mar=c(9,1,1,1))
tanglegram(dl, 
           common_subtrees_color_lines = FALSE, highlight_distinct_edges  = TRUE, highlight_branches_lwd=FALSE, 
           margin_inner=5,
           lwd=2
)
dev.off()
