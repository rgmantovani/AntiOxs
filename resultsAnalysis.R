# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# TODO: tuning for nnet and knn failed (need to debug)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(mlr)

# depends algo on mlr, partykit, dplyr

options(warn=-1)                # suppress warnings

# output directory
if(!dir.exists(path="output")) {
  dir.create(path="output")
}

#--------------------------------------------------------------
# Loading auxiliary functions
#--------------------------------------------------------------

source("R/getRepResults.R")
source("R/getAveragedResults.R")

#--------------------------------------------------------------
# Loading previous results
#--------------------------------------------------------------

results.file = "output/aggregatedResults.RData"
all.files    = list.files(path = "./output", recursive = TRUE, full.names = TRUE)

if(!file.exists(results.file)) {
  cat(' - Generating aggregated results for the first time.')

  # all results from N repetitions
  all.results = getRepsResults(all.files = all.files)
  save(all.results, file = results.file)

} else {
  cat(' - Results already extracted. Loading file.\n')
  load(results.file, verbose = FALSE)
}

#--------------------------------------------------------------
# Creating a new feature joining learner.id and Normalization
#--------------------------------------------------------------

all.results$task.id = as.factor(all.results$task.id)

all.results$algo = paste0(all.results$learner.id,".", all.results$Normalizacao)
all.results$algo = gsub(x = all.results$algo, pattern = "classif.|.preproc|.tuned", replacement = "")
all.results$algo = as.factor(all.results$algo)

#--------------------------------------------------------------
# Reordering factors
#--------------------------------------------------------------

# for each task, measure the rankings
ordered.levels = c("xgboost.no_norm", "xgboost.with_norm",
  "rpart.no_norm", "rpart.with_norm",
  "svm.no_norm", "svm.with_norm",
  "C50.no_norm", "C50.with_norm",
  "J48.no_norm", "J48.with_norm",
  "nnet.no_norm", "nnet.with_norm",
  "naiveBayes.no_norm", "naiveBayes.with_norm",
  "randomForest.no_norm", "randomForest.with_norm",
  "gausspr.no_norm", "gausspr.with_norm",
  "kknn.no_norm", "kknn.with_norm")

all.results$algo = factor(all.results$algo, levels = ordered.levels)
all.results$task = factor(all.results$task,
    levels = c("originalData", "preprocessedData", "pcaData"))

#--------------------------------------------------------------
# Boxplot with all results
#--------------------------------------------------------------

g = ggplot(all.results, aes(x = algo, y =  acc.test.mean))
g = g + geom_boxplot() + facet_grid(Tuning~task.id)
g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))
g = g + labs(x = "Algoritmo", y = "Acur??cia")
ggsave(g, file = "plots/fig5_boxplotOverall.pdf", width = 12.2, height = 5.31)

#--------------------------------------------------------------
# Top best results
#--------------------------------------------------------------

# average results
avg.df = getAveragedResults(all.results = all.results)

# order from best to worst
avg.df = avg.df[order(avg.df$acc.test.mean, decreasing=TRUE),]

#top 20 best algorithms
top = avg.df[1:20,]
# > top[c(1:3,6,12)]
#                 task              algo tuning acc.test.mean acc.test.sd
# 117 preprocessedData   xgboost.no_norm random     0.6011318  0.05334000
# 115     originalData   xgboost.no_norm random     0.5992799  0.05073800
# 118     originalData xgboost.with_norm random     0.5992799  0.05073800
# 120 preprocessedData xgboost.with_norm random     0.5992799  0.05073800
# 43      originalData     rpart.no_norm   none     0.5974063  0.04311161
# 45  preprocessedData     rpart.no_norm   none     0.5974063  0.04311161
# 46      originalData   rpart.with_norm   none     0.5974063  0.04311161
# 48  preprocessedData   rpart.with_norm   none     0.5974063  0.04311161
# 110          pcaData       svm.no_norm random     0.5939829  0.10148995
# 113          pcaData     svm.with_norm random     0.5939829  0.10148995
# 103     originalData     rpart.no_norm random     0.5931780  0.03811435
# 105 preprocessedData     rpart.no_norm random     0.5931780  0.03811435
# 106     originalData   rpart.with_norm random     0.5931780  0.03811435
# 108 preprocessedData   rpart.with_norm random     0.5931780  0.03811435
# 109     originalData       svm.no_norm random     0.5809718  0.07682225
# 111 preprocessedData       svm.no_norm random     0.5809718  0.07682225
# 112     originalData     svm.with_norm random     0.5809718  0.07682225
# 114 preprocessedData     svm.with_norm random     0.5809718  0.07682225
# 55      originalData   xgboost.no_norm   none     0.5788545  0.04693872
# 57  preprocessedData   xgboost.no_norm   none     0.5788545  0.04693872

#----------------------
#----------------------

avg.df$algo = factor(avg.df$algo, levels = ordered.levels)
avg.df$task = factor(avg.df$task,
  levels = c("originalData", "preprocessedData", "pcaData"))

# which.max(all.results$acc.test.mean)
# [1] 651
# > all.results[which.max(all.results$acc.test.mean),]
#    task.id                learner.id multiclass.aunp.test.mean kappa.test.mean
# 651 pcaData classif.svm.preproc.tuned                 0.8337616       0.6143262
#    acc.test.mean ber.test.mean timetrain.test.mean timepredict.test.mean
# 651      0.745614     0.2687831            24.08733           0.002666667
#    Normalizacao Resampling Tuning Repetitions              algo
# 651      no_norm       3-CV random           1 svm.tuned.no_norm

#--------------------------------------------------------------
# Average performance plot
#--------------------------------------------------------------

g1 = ggplot(avg.df, aes(x = algo, y = acc.test.mean, group = tuning, colour = tuning))
g1 = g1 + geom_line() + geom_point() + facet_grid(.~task)
g1 = g1 + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))
g1 = g1 + labs(x = "Algoritmo", y = "Acur??cia (teste)")
ggsave(g1, file = "plots/fig6_performanceMedia.pdf", width = 9.75, height = 4.14)

#--------------------------------------------------------------
#  Loading predictions
#--------------------------------------------------------------

# xgboost with no norm, tuned
xgboost.files =  all.files[grepl(x = all.files,
  pattern = "originalData/classif.xgboost/no_norm/none/3-CV/random")]

#loading preditctions
pred.xgboost.list = lapply(xgboost.files, function(file){
  suppressWarnings(load(file, verbose = FALSE))
  pred.df = mlr::getBMRPredictions(bmr=res, as.df=TRUE)
  sub.pred.df = pred.df[,c("id", "truth", "response")]
  return(sub.pred.df)
})

# rpart with no norm, none
rpart.files = all.files[grepl(x = all.files,
   pattern = "originalData/classif.rpart/no_norm/none/3-CV/none")]

pred.rpart.list = lapply(rpart.files, function(file){
  suppressWarnings(load(file, verbose = FALSE))
  pred.df = mlr::getBMRPredictions(bmr=res, as.df=TRUE)
  sub.pred.df = pred.df[,c("id", "truth", "response")]
  return(sub.pred.df)
})

#--------------------------------------------------------------
# Generating Confusion Matrices
#--------------------------------------------------------------

# Xgboost confusion matrices
aux.mat.xg = lapply(pred.xgboost.list, function(xglist){
  return(table(xglist[,-1]))
})
xg.mat = round(Reduce("+", aux.mat.xg)/length(aux.mat.xg))

# Rpart confusion matrices
aux.mat.dt = lapply(pred.rpart.list, function(dtlist){
  return(table(dtlist[,-1]))
})
dt.mat = round(Reduce("+", aux.mat.dt)/length(aux.mat.dt))

#--------------------------------------------------------------
# Confusion matrices plot
#--------------------------------------------------------------

dt.df = reshape2::melt(dt.mat)
dt.df$algo = "rpart"
xg.df = reshape2::melt(xg.mat)
xg.df$algo = "xgboost"

joined.df = rbind(dt.df, xg.df)

# ---------------
# renaming factors
# ---------------

joined.df$truth[joined.df$truth == "1"] = "TBHQ"
joined.df$truth[joined.df$truth == "2"] = "BHA"
joined.df$truth[joined.df$truth == "3"] = "BHT"
joined.df$response[joined.df$response == "1"] = "TBHQ"
joined.df$response[joined.df$response == "2"] = "BHA"
joined.df$response[joined.df$response == "3"] = "BHT"

levels(joined.df$truth) = c("TBHQ", "BHA", "BHT")
levels(joined.df$response) = c("TBHQ", "BHA", "BHT")

# ---------------
# confusion matrices
# ---------------

g7 = ggplot(data = joined.df, mapping = aes(x = truth, y = response))
g7 = g7 + geom_tile(aes(fill = value), colour = "white") + facet_grid(.~algo)
g7 = g7 + geom_text(aes(label = sprintf("%1.0f", value)), vjust = 1)
g7 = g7 + scale_fill_gradient2(low = "white", high = "red")
g7 = g7 + theme_bw() + theme(legend.position = "none")
g7 = g7 + labs(x = "Classe Real", y = "Classe Predita")
ggsave(g7, file = "plots/fig7_matrizesConfusao.pdf", width=5.82, heitgh=2.97)

#--------------------------------------------------------------
# Decision tree plot (Rules)
#--------------------------------------------------------------

dataset = farff::readARFF(path = "tasks/originalData.arff")
dataset$Class = as.character(dataset$Class)
dataset$Class[dataset$Class == "1"] = "TBHQ"
dataset$Class[dataset$Class == "2"] = "BHA"
dataset$Class[dataset$Class == "3"] = "BHT"

task  = mlr::makeClassifTask(id = "originalData", data = dataset, target = "Class")
lrn   = mlr::makeLearner(cl = "classif.rpart")
model = mlr::train(learner = lrn, task = task)
unwp.models = mlr::getLearnerModel(model = model, more.unwrap = TRUE)

pdf(file = "plots/fig8_arvoreDecisao.pdf", width = 10, height = 4.5)
plot(partykit::as.party(unwp.models))
dev.off()

# ------------------------------------------------------------------------------
# Xgboost feature importance
# ------------------------------------------------------------------------------

xg.lrn   = mlr::makeLearner(cl = "classif.xgboost")
xg.model = mlr::train(learner = xg.lrn, task = task)
xg.unwp.models = mlr::getLearnerModel(model = xg.model, more.unwrap = TRUE)

importance_matrix = xgboost::xgb.importance(model = xg.unwp.models)
importance_matrix$Feature = factor(importance_matrix$Feature, levels = importance_matrix$Feature)
g9 = ggplot(importance_matrix, aes(x = Feature, y = Gain)) + geom_bar(stat = "identity")
g9 = g9 + labs(x = "Atributo", y = "Import??ncia") + theme_bw()
ggsave(g9, file = "plots/fig10_xgboostFeatures.pdf", width = 4.87, height = 3.32)

# ------------------------------------------------------------------------------
# Missclassified examples
# ------------------------------------------------------------------------------

# - descobrir os exemplos classificados erradamente
aux.miss.xg = lapply(pred.xgboost.list, function(elem) {
  return(elem[which(elem$truth != elem$response),])
})
ids.xg = lapply(aux.miss.xg, function(elem) {
  return(elem$id)
})
tab.xg   = table(unlist(ids.xg))
sel.ids.xg = as.numeric(names(tab.xg[tab.xg > 5]))


aux.miss.dt = lapply(pred.rpart.list, function(elem) {
  return(elem[which(elem$truth != elem$response),])
})
ids.dt = lapply(aux.miss.dt, function(elem) {
  return(elem$id)
})
tab.dt   = table(unlist(ids.dt))
sel.ids.dt = as.numeric(names(tab.dt[tab.dt > 5]))

all.ids = unique(c(sel.ids.dt, sel.ids.xg))

# - olhar as estatisticas dos exemplos com erros
miss.df = dataset[all.ids, ]
miss.df.norm = mlr::normalizeFeatures(obj = miss.df , target = "Class",
  method = "range", range = c(0, 1), on.constant = "quiet")

miss.df2 = melt(miss.df.norm, id.vars = ncol(miss.df))
bg = ggplot(miss.df2, aes(x = variable, y = value))
bg = bg + geom_boxplot() + facet_grid(.~Class, scales = "free")
bg = bg + labs(x = "Caracter??stica", y = "Valor Normalizado")
ggsave(bg, file = "plots/fig9_caracteristicas_erros.pdf", width = 8.89, height = 4.05)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
