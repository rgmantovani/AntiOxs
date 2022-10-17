# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(mlr)

set.seed(42)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

data = read.csv(file = "biodiesel.csv", sep = ";")
data$X = NULL
data$X.1 = NULL
data$X.2 = NULL
data$X.3 = NULL

# ------------------------------------------------------------------------------
#  Plot: distribuição de classes
# ------------------------------------------------------------------------------
# Class é o Z
sub = data.frame(table(data$Z))
g = ggplot(data = sub, mapping = aes(x = Var1, y = Freq, fill = Var1))
g = g + geom_bar(stat="identity") + theme_bw()
g = g + labs(x = "Classe", y = "Quantidade", fill = "Antixodidante")
ggsave(g, file = "distribuicaoClasse.pdf", width = 4.37, height = 3.24)

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
mydata = data[, -1]
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
ggsave(g2, file = "correlacaoAtributos.pdf", width = 4.37, height = 3.24)

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
ggsave(g3, file = "separabilidadePCA.pdf", width = 4.37, height = 3.24)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# summary(mydata.pca)
# pca task 7 componentes
new.df2 = as.data.frame(cbind(mydata.pca$x[,1:8], data$Z))
colnames(new.df2)[9] = "Z"
new.df2$Z = as.factor(new.df2$Z)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# TODO: tentar normalizacao dos dados

# Aplicação dos algoritmos de AM

task  = mlr::makeClassifTask(data = data, id = "biodisel", target = "Z")
task2 = mlr::makeClassifTask(data = new.df2, id = "pca_biodisel", target = "Z")

res  = mlr::makeResampleDesc("LOO")
measures = list(acc, bac, ber)

lrn1 = mlr::makeLearner("classif.rpart")
lrn2 = mlr::makeLearner("classif.ranger")
lrn3 = mlr::makeLearner("classif.naiveBayes")
lrn4 = mlr::makeLearner("classif.svm")
lrn5 = mlr::makeLearner("classif.kknn")
lrn6 = mlr::makeLearner("classif.nnet")
lrn7 = mlr::makeLearner("classif.xgboost")

lrns = list(lrn1, lrn2, lrn4, lrn5, lrn6, lrn7)
tasks = list(task, task2)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

bmr = mlr::benchmark(tasks = tasks,
  learners = lrns, measures = list(acc), resamplings = res)
  #
  # > print(bmr)
  #         task.id      learner.id acc.test.mean bac.test.mean ber.test.mean
  # 1      biodisel   classif.rpart    0.41818182           NaN           NaN
  # 2      biodisel  classif.ranger    0.47272727           NaN           NaN
  # 3      biodisel     classif.svm    0.36363636           NaN           NaN
  # 4      biodisel    classif.kknn    0.23636364           NaN           NaN
  # 5      biodisel    classif.nnet    0.01818182           NaN           NaN
  # 6      biodisel classif.xgboost    0.50909091           NaN           NaN
  # 7  pca_biodisel   classif.rpart    0.29090909           NaN           NaN
  # 8  pca_biodisel  classif.ranger    0.45454545           NaN           NaN
  # 9  pca_biodisel     classif.svm    0.40000000           NaN           NaN
  # 10 pca_biodisel    classif.kknn    0.20000000           NaN           NaN
  # 11 pca_biodisel    classif.nnet    0.56363636           NaN           NaN
  # 12 pca_biodisel classif.xgboost    0.50909091           NaN           NaN

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

res2 = mlr::makeResampleDesc("RepCV", rep = 5, folds = 5, stratify = TRUE)
bmr2 = mlr::benchmark(tasks = tasks, learners = lrns, measures = measures, resamplings = res2)
print(bmr2)

# > print(bmr2)
#         task.id      learner.id acc.test.mean bac.test.mean ber.test.mean
# 1      biodisel   classif.rpart     0.5108485     0.5044444     0.4955556
# 2      biodisel  classif.ranger     0.5144242     0.5055556     0.4944444
# 3      biodisel     classif.svm     0.4769091     0.4633333     0.5366667
# 4      biodisel    classif.kknn     0.2925455     0.2855556     0.7144444
# 5      biodisel    classif.nnet     0.3070303     0.3133333     0.6866667
# 6      biodisel classif.xgboost     0.5610303     0.5588889     0.4411111
# 7  pca_biodisel   classif.rpart     0.4496970     0.4444444     0.5555556
# 8  pca_biodisel  classif.ranger     0.4498182     0.4344444     0.5655556
# 9  pca_biodisel     classif.svm     0.4666061     0.4555556     0.5444444
# 10 pca_biodisel    classif.kknn     0.2559394     0.2477778     0.7522222
# 11 pca_biodisel    classif.nnet     0.5389697     0.5244444     0.4755556
# 12 pca_biodisel classif.xgboost     0.4957576     0.4855556     0.5144444

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

res3 = mlr::makeResampleDesc("RepCV", rep = 5, folds = 3, stratify = TRUE)
bmr3 = mlr::benchmark(tasks = tasks, learners = lrns, measures = measures, resamplings = res3)
print(bmr3)


res4 = mlr::makeResampleDesc("RepCV", rep = 5, folds = 2, stratify = TRUE)
bmr4 = mlr::benchmark(tasks = tasks, learners = lrns, measures = measures, resamplings = res4)
print(bmr4)
