#'Heterosis and Heterobeltiosis
#'@description
#'Calculation of heterosis and heterobeltiosis parameters of hybrids
#'@param GEN The column with the genotype name
#'@param GM The column with the average of the maternal parent
#'@param GP The column with the average of the paternal parent
#'@param PR The column with the average of the progeny
#'@param REP The column with the repetitions (if exists)
#'@param param Value to determine the parameter to be calculated. Default is “all”.
#'To calculate heterosis only, use “het”. To calculate only heterobeltiosis,
#'use “hetb”.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export
#'@examples
#'\donttest{
#' library(EstimateBreed)
#'
#' data("maize")
#' #Extract heterosis and heterobeltiosis
#' with(maize,heterose(GEN,GM,GP,PR,REP,param="all"))
#'
#' #Only extract heterosis
#' with(maize,heterose(GEN,GM,GP,PR,REP,param = "het"))
#'
#' #Extract only heterobeltiosis
#' with(maize,heterose(GEN,GM,GP,PR,REP,param = "hetb"))
#'}

heterose <- function(GEN, GM, GP, PR, REP, param = "all") {
  require(dplyr)
  require(stats)

  data <- data.frame(GEN, GM, GP, PR, REP)
  model1 <- aov(REP ~ GEN, data = data)
  MSe <- summary(model1)[[1]]["Residuals", "Mean Sq"]
  r <- length(unique(REP))
  data <- data %>%
    mutate(
      Heterosis = ((PR - ((GM + GP) / 2)) / ((GM + GP) / 2)) * 100,
      SE_Heterosis = sqrt((3 * MSe) / (2 * r))
    )
  Genitor <- pmax(GM, GP)
  data <- data %>%
    mutate(
      Heterobeltiosis = ((PR - Genitor) / Genitor) * 100,
      SE_Heterobeltiosis = sqrt((2 * MSe) / r)
    )

  if (param == "all") {
    cat("Parameters\n")
    cat("SE_Heterosis:",paste(first(data$SE_Heterosis)),"\n")
    cat("SE_Heterobeltiosis:",paste(first(data$SE_Heterobeltiosis)),"\n")
    cat("-------------------------------------------\n")
    return(data[, c("GEN", "Heterosis", "Heterobeltiosis")])

  } else if (param == "het") {
    cat("Parameters\n")
    cat("SE_Heterosis:",paste(first(data$SE_Heterosis)),"\n")
    cat("-------------------------------------------\n")
    return(data[, c("GEN", "Heterosis")])

  } else if (param == "hetb") {
    cat("Parameters\n")
    cat("SE_Heterobeltiosis:",paste(first(data$SE_Heterobeltiosis)),"\n")
    cat("-------------------------------------------\n")
    print(first(data$SE_Heterobeltiosis))
    return(data[, c("GEN", "Heterobeltiosis")])
  }
}
