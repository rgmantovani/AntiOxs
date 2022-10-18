#--------------------------------------------------------------
#--------------------------------------------------------------

# TODO: add an option to choose the measure

getAveragedResults = function(all.results) {

  tasks   = unique(all.results$task.id)
  algos   = unique(all.results$algo)
  tunings = unique(all.results$Tuning)

  # all combinations between vectors
  combinations = expand.grid(tasks, algos, tunings)
  colnames(combinations) = c("task", "algo", "tuning")

  aux = lapply(1:nrow(combinations),function(i) {
      smp = combinations[i, ]
      sub.sample = dplyr::filter(all.results, task.id == smp[1]$task & algo == smp[2]$algo
         & Tuning == smp[3]$tuning)

      values     = sub.sample[,c(3:8)]
      avg.values = colMeans(values)
      sd.values  = apply(values, 2, sd)
      names(sd.values) = gsub(x = names(sd.values), pattern = "mean", replacement = "sd")

      ret = cbind(smp[1,], data.frame(t(avg.values)), data.frame(t(sd.values)))

      return(ret)
  })

  avg.df = do.call("rbind", aux)
  return(avg.df)

}


#--------------------------------------------------------------
#--------------------------------------------------------------
