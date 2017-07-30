
#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
myModelClass<-function(x){
  sum=sum(x)
  Series=c(x,x)
  var=var(x)
  mean=mean(x)
  allProbs=list(sum,var,mean)
  #output<-list()
  #output[["Series"]]<-Series
  #output[["Sum"]]<-sum
  #output[["Mean"]]<-mean
  #output[["Variance"]]<-var
  #Output[["All Probabilities"]]<-allProbs
  output<-list(Series=Series,sum=sum, mean=mean, variance=var, allProbs)
  class(output)<-c(class(output))
  return(output)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
load_mat <- function(infile){
  in.dt <- data.table::fread(infile, header = TRUE)
  in.dt <- in.dt[!duplicated(in.dt[, 1]), ]
  in.mat <- as.matrix(in.dt[, -1, with = FALSE])
  rownames(in.mat) <- unlist(in.dt[, 1, with = FALSE])
  in.mat
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
load_mat2 <- function(infile){
  ...
}





















