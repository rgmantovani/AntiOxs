#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRepsResults = function(all.files, debug = FALSE) {

  aux = lapply(all.files, function(data.file) {
    if(debug) {
      print(data.file)
    }
    suppressWarnings(load(data.file, verbose = FALSE))
    ret = mlr::getBMRAggrPerformances(bmr = res, as.df = TRUE)
    return(ret)
  }) #aux

  results = data.frame(do.call("rbind", aux))

  # adding job information
  tokens   = strsplit(x = all.files, split = "/")
  infos    = data.frame(do.call("rbind", tokens))
  infos    = infos[,-c(1:4,6,9,11)]
  df.final = cbind(results, infos)

  # renaming columns
  colnames(df.final)[9:12] = c("Normalization", "Resampling", "Tuning", "Repetitions")

  return(df.final)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
