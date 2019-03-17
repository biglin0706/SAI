#' SAI for SCREEN
#' @description do \bold{univariate} regression to all explanatory variables.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @param model the model's name, is a character, `linear` for linear regression, `logit` for logistic regression, `poisson` for poisson regression, `ordinal_logistic` for ordinal logistic regression
#' @return it will output a data.frame with univariate regreesion model to all explanatory variables, the p.value \bold{will be sorted} from smallest to lagest. So that we can easily find out which variable will effect the model dramatically.
#' @examples sai_screen(data = iris, y = "Sepal.Length", model = "linear")
#' @export
sai_screen <- function(data, y, model){
  p <- dim(data)[2]-1
  formula <- list()
  name <- names(data)[names(data)!=y]
  for(i in 1:p) formula[[i]] <- paste0(y, "~",name[i])
  formula <- unlist(formula)
  pvalue <- list()
  switch(model,
         linear = {for(k in 1:p) pvalue[[k]] <- matrix(t(summary(lm(as.formula(formula[k]), data = data))$coef[-1, 4]), nrow = 1)},
         logit = {for(k in 1:p) pvalue[[k]] <- matrix(t(summary(glm(as.formula(formula[k]), data = data, family = binomial(logit)))$coef[-1, 4]), nrow = 1)},
         poisson = {for(k in 1:p) pvalue[[k]] <- matrix(t(summary(glm(as.formula(formula[k]), data = data, family = poisson(log)))$coef[-1, 4]), nrow = 1)},
         ordi_logi = {for(k in 1:p) pvalue[[k]] <- matrix(t(pnorm(abs(coef(summary(polr(formula[k], data = data, Hess = T)))[, "t value"]), lower.tail = F)*2), nrow = 1)})
  pvalue <- data.frame(do.call(rbind.fill.matrix, pvalue)  )
  pvalue <- round(pvalue, 4)
  pvalue <- cbind(name, pvalue)
  names(pvalue)[1:2] <- c("X", "p.value")
  pvalue <- pvalue[with(pvalue, order(p.value)),]
  return(pvalue)
}

#' SAI for Linear Model
#' @description  Find all combinations of explanatory variables, and show the significant linear regression model.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @return \bold{form} is the formulas
#' @return \bold{pvalue} is the p.value correspond to the formulas in \bold{form}
#' @return \bold{sig} is value 1 or 0, represents that if any \bold{pvalue} is significant in the model.
#' @examples sai_linear(data = iris, y = "Sepal.Length")
#' @export
sai_linear<- function(data,y, alpha = 0.05, mustin = NULL){
  name <- names(data)[!(names(data) %in% y | names(data) %in% mustin)] #variable names of x
  p <- ifelse(length(name)==0, 1, length(name))
  mustform <- paste(mustin, collapse = "+")
  form <- list()  # formula
  if(length(name) == 0){
    form <- paste(y, mustform, sep = "~")
  }else{
    for(i in 1:p){
      c <- combn(name, i)
      form[[i]] <- apply(c, 2, paste, collapse = "+")
    }
    if(is.null(mustform)){
      form <- paste(y, unlist(form), sep = "~")
    }else{
      form <- paste(y, paste(unlist(form), mustform, sep = "+"), sep = "~")
    }

  }

  pvalue <- list() #record pvalue

  N = length(form)

  sig <- vector(length = N)
  print(paste("number of all combinations is", N))

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating", value = 0)



  for(k in 1:N){
    if(k%%1000==0) print(k)
    pvalue[[k]] <- data.frame(round(t(summary(lm(form[k], data = data))$coefficient[,4]), 3))
    if(any(pvalue[[k]]<=alpha)) sig[k] <- 1
    progress$inc(1, detail = paste("Doing part", k, "/", N))
    #Sys.sleep(0.1)
  }
  pvalue <- do.call(rbind.fill, pvalue)
  pvalue <- cbind(form, pvalue)
  list(form = form, pvalue = pvalue, sig = sig)
}



#' SAI for Logistic Regression Model(binomial)
#' @description  Find all combinations of explanatory variables, and show the significant linear regression model.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @return \bold{form} is the formulas
#' @return \bold{pvalue} is the p.value correspond to the formulas in \bold{form}
#' @return \bold{sig} is value 1 or 0, represents that if any \bold{pvalue} is significant in the model.
#' @examples sai_logit()
#' @export
sai_logit <- function(data,y, alpha = 0.05, mustin = NULL){
  name <- names(data)[!(names(data) %in% y | names(data) %in% mustin)] #variable names of x
  p <- ifelse(length(name)== 0, 1, length(name))
  mustform <- paste(mustin, collapse = "+")
  form <- list()  # formula
  if(length(name) == 0){
    form <- paste(y, mustform, sep = "~")
  }else{
    for(i in 1:p){
      c <- combn(name, i)
      form[[i]] <- apply(c, 2, paste, collapse = "+")
    }
    if(is.null(mustform)){
      form <- paste(y, unlist(form), sep = "~")
    }else{
      form <- paste(y, paste(unlist(form), mustform, sep = "+"), sep = "~")
    }
  }
  pvalue <- list() #record pvalue
  N = length(form)

  sig <- vector(length = N)


  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating", value = 0)


  print(paste("number of all combinations is", N))
  for(k in 1:N){
    if(k%%1000==0) print(k)
    pvalue[[k]] <- data.frame(t(round(summary(glm(form[k], data = data, family = binomial(logit)))$coefficient[,4], 3)))
    if(any(pvalue[[k]]<=alpha)) sig[k] <- 1
    progress$inc(1, detail = paste("Doing part", k, "/", N))
  }
  pvalue <- do.call(rbind.fill, pvalue)
  pvalue <- cbind(form, pvalue)
  list(form = form, pvalue = pvalue, sig = sig)
}

#' SAI for Logistic Regression Model(poisson)
#' @description  Find all combinations of explanatory variables, and show the significant linear regression model.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @return \bold{form} is the formulas
#' @return \bold{pvalue} is the p.value correspond to the formulas in \bold{form}
#' @return \bold{sig} is value 1 or 0, represents that if any \bold{pvalue} is significant in the model.
#' @examples sai_logit()
#' @export
sai_poisson <- function(data,y, alpha = 0.05, mustin = NULL){
  name <- names(data)[!(names(data) %in% y | names(data) %in% mustin)] #variable names of x
  p <- ifelse(length(name)== 0, 1, length(name))
  mustform <- paste(mustin, collapse = "+")
  form <- list()  # formula
  if(length(name) == 0){
    form <- paste(y, mustform, sep = "~")
  }else{
    for(i in 1:p){
      c <- combn(name, i)
      form[[i]] <- apply(c, 2, paste, collapse = "+")
    }
    if(is.null(mustform)){
      form <- paste(y, unlist(form), sep = "~")
    }else{
      form <- paste(y, paste(unlist(form), mustform, sep = "+"), sep = "~")
    }
  }
  pvalue <- list() #record pvalue
  N = length(form)

  sig <- vector(length = N)

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating", value = 0)


  print(paste("number of all combinations is", N))
  for(k in 1:N){
    if(k%%1000==0) print(k)
    pvalue[[k]] <- data.frame(t(round(summary(glm(form[k], data = data, family = poisson(log)))$coefficient[-1,4], 3)))
    if(any(pvalue[[k]]<=alpha)) sig[k] <- 1
    progress$inc(1, detail = paste("Doing part", k, "/", N))

  }
  pvalue <- do.call(rbind.fill, pvalue)
  pvalue <- cbind(form, pvalue)
  list(form = form, pvalue = pvalue, sig = sig)
}

#' SAI for Proportional Hazards Regression Model
#' @description  Find all combinations of explanatory variables, and show the significant linear regression model.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @return \bold{form} is the formulas
#' @return \bold{pvalue} is the p.value correspond to the formulas in \bold{form}
#' @return \bold{sig} is value 1 or 0, represents that if any \bold{pvalue} is significant in the model.
#' @examples sai_coxph(data,y, alpha = 0.05)
#' @export
sai_coxph <- function(data, y, alpha = 0.05, mustin = NULL){
  name <- names(data)[!(names(data) %in% y | names(data) %in% mustin)] #variable names of x
  p <- ifelse(length(name)== 0, 1, length(name))
  mustform <- paste(mustin, collapse = "+")
  form <- list()  # formula
  if(length(name) == 0){
    form <- paste(paste("Surv(", paste(y, collapse = ","), ")"), mustform, sep = "~")
  }else{
    for(i in 1:p){
      c <- combn(name, i)
      form[[i]] <- apply(c, 2, paste, collapse = "+")
    }
    if(is.null(mustform)){
      form <- paste(paste("Surv(", paste(y, collapse = ","), ")"), unlist(form), sep = "~")
    }else{
      form <- paste(paste("Surv(", paste(y, collapse = ","), ")"), paste(unlist(form), mustform, sep = "+"), sep = "~")
    }
  }

  pvalue <- list() #record pvalue
  N = length(form)

  sig <- vector(length = N)

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating", value = 0)


  print(paste("number of all combinations is", N))
  for(k in 1:N){
    if(k%%1000==0) print(k)
    pvalue[[k]] <- data.frame(t(round(summary(coxph(form[k], data = data, family = poisson(log)))$coefficient[,5], 3)))
    if(any(pvalue[[k]]<=alpha)) sig[k] <- 1
    progress$inc(1, detail = paste("Doing part", k, "/", N))

  }
  pvalue <- do.call(rbind.fill, pvalue)
  pvalue <- cbind(form, pvalue)
  list(form = form, pvalue = pvalue, sig = sig)
}

#' SAI for Ordinal Logistic Regression Model
#' @description  Find all combinations of explanatory variables, and show the significant ordinal logistic regression model.
#' @param data the data.frame which contain responce variable and explanatory variables.
#' @param y the responce variable "name", is a character.
#' @return \bold{form} is the formulas
#' @return \bold{pvalue} is the p.value correspond to the formulas in \bold{form}
#' @return \bold{sig} is value 1 or 0, represents that if any \bold{pvalue} is significant in the model.
#' @examples sai_logit()
#' @export
sai_ordinal_logistic <- function(data,y, alpha = 0.05, mustin = NULL){
  name <- names(data)[!(names(data) %in% y | names(data) %in% mustin)] #variable names of x
  p <- ifelse(length(name)== 0, 1, length(name))
  mustform <- paste(mustin, collapse = "+")
  form <- list()  # formula
  if(length(name) == 0){
    form <- paste(y, mustform, sep = "~")
  }else{
    for(i in 1:p){
      c <- combn(name, i)
      form[[i]] <- apply(c, 2, paste, collapse = "+")
    }
    if(is.null(mustform)){
      form <- paste(y, unlist(form), sep = "~")
    }else{
      form <- paste(y, paste(unlist(form), mustform, sep = "+"), sep = "~")
    }
  }
  pvalue <- list() #record pvalue
  N = length(form)

  sig <- vector(length = N)

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating", value = 0)


  print(paste("number of all combinations is", N))
  for(k in 1:N){
    if(k%%1000==0) print(k)
    pvalue[[k]] <- data.frame(t(round(pnorm(abs(coef(summary(polr(form[k], data = data, Hess = T)))[, "t value"]), lower.tail = F)*2, 3)))
    if(any(pvalue[[k]]<=alpha)) sig[k] <- 1
    progress$inc(1, detail = paste("Doing part", k, "/", N))

  }
  pvalue <- do.call(rbind.fill, pvalue)
  pvalue <- cbind(form, pvalue)
  list(form = form, pvalue = pvalue, sig = sig)
}

