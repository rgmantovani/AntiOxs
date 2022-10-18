# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(ggplot2)
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

if(!file.exists(results.file)) {
  cat(' - Generating aggregated results for the first time.')

  all.files = list.files(path = "./output", recursive = TRUE, full.names = TRUE)

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
# Boxplot with all results
#--------------------------------------------------------------

g = ggplot(all.results, aes(x = algo, y =  acc.test.mean))
g = g + geom_boxplot() + facet_grid(Tuning~task.id)
g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))
g = g + labs(x = "Algoritmo", y = "Acurácia")
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

avg.df$algo = factor(avg.df$algo, levels = ordered.levels)
avg.df$task = factor(avg.df$task,
  levels = c("originalData", "preprocessedData", "pcaData"))

#--------------------------------------------------------------
# Average performance plot
#--------------------------------------------------------------

g1 = ggplot(avg.df, aes(x = algo, y = acc.test.mean, group = tuning, colour = tuning))
g1 = g1 + geom_line() + geom_point() + facet_grid(.~task)
g1 = g1 + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))
g1 = g1 + labs(x = "Algoritmo", y = "Acurácia (teste)")
ggsave(g1, file = "fig6_performanceMedia.pdf", width = 9.75, height = 4.14)

# TODO:
# - olhar matrizes de confusão da arvore
# - olhar as estatisticas das classes preditas erradamente
# - olhar a arvore de decisão/regras


#----------------------
#----------------------

# which.max(all.results$acc.test.mean)
# [1] 651
# > all.results[which.max(all.results$acc.test.mean),]
#    task.id                learner.id multiclass.aunp.test.mean kappa.test.mean
# 651 pcaData classif.svm.preproc.tuned                 0.8337616       0.6143262
#    acc.test.mean ber.test.mean timetrain.test.mean timepredict.test.mean
# 651      0.745614     0.2687831            24.08733           0.002666667
#    Normalizacao Resampling Tuning Repetitions              algo
# 651      no_norm       3-CV random           1 svm.tuned.no_norm

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
