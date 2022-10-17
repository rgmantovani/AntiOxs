# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library("mlr")
library("farff")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Pre-processing function
# - remove constant attributes
# - rename categorical attributes
# - normalize all attribues (mean=0, std=1), but Class
# - remove duplicate examples
# - format file header

preprocessing = function(data) {

  # ---------------------
  # Doing imputation
  # ---------------------
  if(any(is.na(data))) {
    cat(" - doing imputation\n")
    imp = mlr::impute(obj = data, target = "Class",
      classes = list(integer = mlr::imputeMedian(),
      factor  = mlr::imputeConstant(const = "New"),
      numeric = mlr::imputeMedian())
    )
    data = imp$data
  }

  # ---------------------
  # Creating Dummy features
  # ---------------------
  data = mlr::createDummyFeatures(obj = data, target = "Class", method = "1-of-n")
  target.id = which(colnames(data) == "Class")

  class = data[,target.id]
  data  = data[,-target.id]

  # ---------------------
  # Remove constant attributes
  # ---------------------
  for(i in colnames(data)) {

    # categorical attributes
    if(is.factor(data[,i])) {

      if(nlevels(data[,i]) == 1 || nlevels(data[,i]) == nrow(data)) {
        data[,i] = NULL
      }else{
        #renaming predicates
        data[,i] = factor(data[,i])
        levels(data[,i]) = factor(1:length(levels(data[,i])))
      }
    # numeric attributes
    } else {
      if(sd(data[,i]) == 0) {
        data[,i] = NULL
      }
    }
  }

  print(" - Rescaling data - Range [0,1]")
  new.data = mlr::normalizeFeatures(obj = data, method = "range",
    range = c(0, 1), on.constant = "quiet")

  data = cbind(new.data, class)
  colnames(data)[ncol(data)] = "Class"

  # ---------------------
  # remove duplicated examples
  # ---------------------

  aux = which(duplicated(data))
  if(length(aux) != 0){
    data = data[-aux,]
  }

  # ---------------------
  # format the header
  # ---------------------
  data$Class = factor(data$Class)
  rownames(data) = NULL
  return (data)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

cat(" @ Loading data ...\n")
data = read.csv(file = "data/biodiesel.csv", sep = ";")
data$X = NULL
data$X.1 = NULL
data$X.2 = NULL
data$X.3 = NULL

original = data

cat(" @ Preprocessing data ...\n")
preprocessed = preprocessing(data = data)

# ------------------------------------------------------------------------------
# PCA dataset
# ------------------------------------------------------------------------------

cat(" @ PCA data ...\n")
mydata   = original[, -1]
pca.data = prcomp(mydata, center = TRUE, scale. = TRUE)
pca.df = as.data.frame(pca.data$x)
pca.df = cbind(pca.df, original$Class)
colnames(pca.df)[ncol(pca.df)] = "Class"


# ------------------------------------------------------------------------------
# Class as factor
# ------------------------------------------------------------------------------

pca.df$Class = as.factor(pca.df$Class)

original = cbind(original[,-1], original[1])
colnames(original)[ncol(original)] = "Class"
original$Class = as.factor(original$Class)

# ------------------------------------------------------------------------------
# saving tasks
# ------------------------------------------------------------------------------

if(!dir.exists(paths="tasks/")) {
  dir.create(path = "tasks/")
}

cat(" @ Saving tasks ...\n")
farff::writeARFF(x = original, path = "tasks/originalData.arff", overwrite = TRUE)
farff::writeARFF(x = preprocessed, path = "tasks/preprocessedData.arff", overwrite = TRUE)
farff::writeARFF(x = pca.df, path = "tasks/pcaData.arff", overwrite = TRUE)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
