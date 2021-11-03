#script: Abhi Sudarshana

library("FactoMineR")
library("factoextra")
library(ggplot2)
#data_cells <- data.frame(incremental15percenttest5)
data.active <- data[1:569,3:12]
#need to subset variables (predictors) that are going to be used in PCA, dont include nascency values
head(data.active)
pca <- PCA(data.active, scale.unit = TRUE, ncp = 5, graph = TRUE)
#print(pca)
# extracting different info from PCA:
#get_eigenvalue(pca): Extract the eigenvalues
#fviz_eig(pca): Visualize eigenvalues
#get_pca_ind(pca), get_pca_var(pca): Extract the results for individuals and variables
#fviz_pca_ind(pca), fviz_pca_var(pca): Visualize the results individuals and variables
#fviz_pca_biplot(pca): Plot of individuals and variables
eig.val <- get_eigenvalue(pca)
#eig.val
# The proportion of variation explained by each eigenvalue is in the 2nd column. 
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 60)) #you can adjust the axes parameters to view the graphs
var <- get_pca_var(pca)
#var
#var$coord
#var$cos2
#var$cor
#var$contrib
fviz_pca_var(pca, col.var = "black",repel = TRUE)
#Positively correlated variables together, negatively corr. variables opposite of origin
#Variables close to circumference are well represented by the dimensions (PCs)
library("corrplot")
#corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca, choice = "var")
#corrplot(var$coord, is.corr=FALSE)
#corrplot(var$contrib, is.corr=FALSE)
#fviz_cos2(pca, choice = "var", axes = 1:2)
#fviz_cos2(pca, choice = "var", axes = 1:2, ylim = c(0, 1))
#fviz_cos2(pca, choice = "var", axes = 1:2, ylim = c(0, 10))
# Color by cos2 values
#fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("grey", "blue", "orange"), repel = TRUE) 
#top 10 variables contributing to PCs
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
#Total contribution to PC1 and PC2
fviz_contrib(pca, choice = "var", axes = 1:2, top = 10, ylim = c(0,50))
#fviz_pca_var(pca, col.var = "contrib",gradient.cols = c("grey", "blue", "orange"), repel = TRUE)
#Random continuous variable of length 64 for the 64 columns (color by continuous variable)
#set.seed(123)
#cont_var <- rnorm(64)
#fviz_pca_var(pca, col.var = cont_var, gradient.cols = c("grey", "blue", "orange"), repel = TRUE ,legend.title = "Cont_Var")
#kmeans clustering
# Create appropriate number of groups of variables (# of centers)
set.seed(123)
km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(km$cluster)
fviz_pca_var(pca, col.var = grp, palette = c("grey","blue", "orange"), legend.title = "Clustering", repel = TRUE)
#fviz_pca_var(pca, col.var = grp, palette = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), legend.title = "Clustering")
#data on individuals (reads)
ind <- get_pca_ind(pca)
#ind
#ind$coord
#ind$cos2
#ind$contrib
fviz_pca_ind(pca)

#For plotting individual data, choose clusters and the # of colors in the color gradients based on the number of nascency classes 
fviz_pca_ind(pca, col.ind = data$GRADE, geom = c("point")) 
#fviz_pca_ind(pca, col.ind = "cos2", gradient.cols = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), repel = TRUE) 
#Similar reads grouped together 
#Use data points sizes according to read cos2 values
#fviz_pca_ind(pca, pointsize = "cos2", pointshape = 20, fill = "red", repel = TRUE) 
#Point sizes and color by cos2
#fviz_pca_ind(pca, col.ind = "cos2", pointsize = "cos2", gradient.cols = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), repel = TRUE)
#bar plot of cos2 of individuals (reads)
#top 10 contributing individuals (reads)
#fviz_cos2(pca, choice = "ind")
fviz_cos2(pca, choice = "ind", top = 10)
# Total contribution to PC1 and PC2
fviz_contrib(pca, choice = "ind", axes = 1:2)
fviz_contrib(pca, choice = "ind", axes = 1:2, ylim = c(0,15), top = 30)
#color by continuous variable (same length as # of individuals (reads) in PCA)
set.seed(123)
my_cont_var <- rnorm(569)
#fviz_pca_ind(pca, col.ind = my_cont_var, gradient.cols = c("red", "blue"), legend.title = "Tumor cell type", repel = TRUE)
#fviz_pca_ind(pca, col.ind = my_cont_var, gradient.cols = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), legend.title = "Cont_Var", repel = TRUE)
#color by nascency column in dataset (use title of column for nascency values) 
data_40_increment_multinomialreg$nascency <- as.numeric(data_40_increment_multinomialreg$nascency)
#fviz_pca_ind(pca, col.ind = data_40_increment_multinomialreg$nascency, gradient.cols = c("red", "blue"), legend.title = "Cont_Var", repel = TRUE)
fviz_pca_ind(pca, col.ind = data_40_increment_multinomialreg$nascency, gradient.cols = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), legend.title = "Cont_Var", repel = TRUE)
# Add confidence ellipses (optional)
data_40_increment_multinomialreg$nascency <- as.factor(data_40_increment_multinomialreg$nascency)
#fviz_pca_ind(pca, geom.ind = c("point","text"), col.ind = data_40_increment_multinomialreg$nascency, palette = c("red", "blue"), addEllipses = TRUE, ellipse.type = "confidence", legend.title = "nascency",repel = TRUE)
fviz_pca_ind(pca, geom.ind = "point", col.ind = data_40_increment_multinomialreg$nascency, palette = c("red", "pink", "orange", "yellow", "green", "blue", "purple", "violet", "grey", "black"), addEllipses = TRUE, ellipse.type = "confidence", legend.title = "nascency", repel = TRUE)
#Simultaneously save all plots that were created   
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/abhir/OneDrive/Documents/bme230hw/hw2_plots") # input a path to a folder on your computer to save all the plots







