# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(ggplot2)
options(warn=-1)                # suppress warnings

# output directory
if(!dir.exists(path="output")) {
  dir.create(path="output")
}

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
  load(results.file, verbose = TRUE)
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
#--------------------------------------------------------------

# average results
avg.df = getAveragedResults(all.results = all.results)

# for each task, measure the rankings

#----------------------
# Average performance plot
#----------------------

# TODO:
# - average performance plot (line)
# - order boxplot
# - rankear os algoritmos
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