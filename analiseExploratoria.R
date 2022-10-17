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

# ------------------------------------------------------------------------------
#  Plot: distribuição de classes
# ------------------------------------------------------------------------------
# Class é o Z
sub = data.frame(table(data$Class))
g = ggplot(data = sub, mapping = aes(x = Var1, y = Freq, fill = Var1))
g = g + geom_bar(stat="identity") + theme_bw()
g = g + labs(x = "Classe", y = "Quantidade", fill = "Antixodidante")
ggsave(g, file = "plots/distribuicaoClasse.pdf", width = 4.37, height = 3.24)

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
ggsave(g2, file = "plots/correlacaoAtributos.pdf", width = 4.37, height = 3.24)

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
ggsave(g3, file = "plots/separabilidadePCA.pdf", width = 4.37, height = 3.24)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# boxplot dos atributos
data2 = data[]
df2   = melt(data2, id.vars = 1)

# rename factors
c("1", "2", "3")
c("TBHQ", "BHA", "BHT")

df2$Class[df2$Class == "1"] = "TBHQ"
df2$Class[df2$Class == "2"] = "BHA"
df2$Class[df2$Class == "3"] = "BHT"

levels(df2$Class) = c("TBHQ", "BHA", "BHT")


bg = ggplot(df2, aes(x = variable, y = log(value)))
bg = bg + geom_boxplot() + facet_grid(.~Class) + theme_bw()
bg = bg + labs(x = "Característica", y = "log(valor)")
ggsave(bg, file = "plots/fig4_FeaturesBoxplot.pdf", width = 9.05, height = 2.93)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------