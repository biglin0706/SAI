#' Table1 of all variables
#' @description  Create table1 of all variables
#' @param data the data
#' @export
table1_all <- function(data){
  formula <- paste("~ ",paste(names(data), collapse = "+ "))
  for(i in 1:length(data)) label(data[,i]) <- names(data)[i]
  table1(x = as.formula(formula), data = data)
}
