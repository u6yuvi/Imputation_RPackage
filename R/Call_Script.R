#' @title Imputation
#'
#' @description  Imputation function
#' @param fram a dataframe consisting of  missing data to be imputed
#' @return dataset
#' @export
#' @example \dontrun{
#' x=impute(dataset)}

source("Functions.R")
# install.packages('VIM')
# install.packages('data.table')
# install.packages('moments')
library(VIM)
library(data.table)
library(moments)

fram <- mtcars

#Function Call
impute(fram)
