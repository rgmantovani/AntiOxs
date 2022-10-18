# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(mlr)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

if(!dir.exists("/plots")) {
  dir.create("/plots")
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

data = read.csv(file = "data/biodiesel.csv", sep = ";")
data$X = NULL
data$X.1 = NULL
data$X.2 = NULL
data$X.3 = NULL

data$Class[data$Class == "1"] = "TBHQ"
data$Class[data$Class == "2"] = "BHA"
data$Class[data$Class == "3"] = "BHT"
levels(data$Class) = c("TBHQ", "BHA", "BHT")


# ------------------------------------------------------------------------------
#  Plot: distribuição de classes
# ------------------------------------------------------------------------------
# Class é o Z
sub = data.frame(table(data$Class))
g = ggplot(data = sub, mapping = aes(x = Var1, y = Freq, fill = Var1))
g = g + geom_bar(stat="identity") + theme_bw()
g = g + geom_text(aes(label=Freq), vjust=-0.5)
g = g + labs(x = "Classe", y = "Quantidade", fill = "Antixodidante")
ggsave(g, file = "plots/fig1_distribuicaoClasse.pdf", width = 4.37, height = 3.24)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Get lower triangle of the correlation matrix
get_lower_tri = function(cormat){
  cormat[upper.tri(cormat)] = NA
  diag(cormat) = NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)] = NA
  diag(cormat) = NA
  return(cormat)
}


# Plotar matriz de correlação dos atributos (sem contar a classe)
mydata = data[ ] #, -1]
cormat = round(cor(mydata),2)
upper_tri = get_upper_tri(cormat)
melted_cormat = melt(upper_tri)

g2 = ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))
g2 = g2 + geom_tile(color = "white")
g2 = g2 + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
   midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation")
g2 = g2 + theme_bw()
g2 = g2 + theme(axis.text.x = element_text(angle = 90, vjust = 1,hjust = 1))
g2 = g2 + labs(x = "", y = "")
ggsave(g2, file = "plots/fig2_correlacaoAtributos.pdf", width = 4.37, height = 3.24)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Plotar separabilidade de classes por PCA

mydata.pca = prcomp(mydata, center = TRUE, scale. = TRUE)
summary(mydata.pca)

new.df = as.data.frame(cbind(mydata.pca$x[,c(1,2)], data$Z))
colnames(new.df) = c("PC1", "PC2", "Classe")
new.df$Classe = as.factor(new.df$Class)

g3 = ggplot(new.df, aes(x = PC1, y = PC2, colour = Classe, shape = Classe))
g3 = g3 + geom_point(size = 3) + theme_bw()
ggsave(g3, file = "plots/fig3_separabilidadePCA.pdf", width = 4.37, height = 3.24)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -------------------
# boxplot dos atributos
# -------------------

data2 = data
data2.norm = mlr::normalizeFeatures(obj = data2 , target = "Class",
  method = "range", range = c(0, 1), on.constant = "quiet")
df2   = melt(data2.norm, id.vars = 1)

bg2 = ggplot(df2, aes(x = variable, y = value))
bg2 = bg2 + geom_boxplot() + facet_grid(Class~.)
bg2 = bg2 + labs(x = "Característica", y = "Valor")
ggsave(bg2, file = "plots/fig4A_FeaturesBoxplot.pdf", width = 5.38, height = 5.5)

# -------------------
# violinplot dos atributos
# -------------------

vg2 = ggplot(df2, aes(x = variable, y = value))
vg2 = vg2 + geom_violin() + geom_boxplot(width=0.1) + facet_grid(Class~.)
vg2 = vg2 + labs(x = "Característica", y = "Valor")
ggsave(vg2, file = "plots/fig4B_FeaturesViolinPlot.pdf", width = 5.38, height = 5.5)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
