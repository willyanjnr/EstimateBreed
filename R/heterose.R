#'Heterosis and Heterobeltiosis
#'@description
#'Calculation of heterosis and heterobeltiosis parameters of hybrids
#'@param GEN The column with the genotype name
#'@param GM The column with the average of the maternal parent
#'@param GP The column with the average of the paternal parent
#'@param PR The column with the average of the progeny
#'@param REP The column with the repetitions
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
#' library(Breeding)
#'
#' data("maize")
#' #Extract heterosis and heterobeltiosis
#' with(maize,heterose(HIB,GM,GP,P,stat="all"))
#'
#' #Only extract heterosis
#' with(maize,heterose(HIB,GM,GP,P,param = "het"))
#'
#' #Extract only heterobeltiosis
#' with(maize,heterose(HIB,GM,GP,P,param = "hetb"))
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
      Heterose = ((PR - ((GM + GP) / 2)) / ((GM + GP) / 2)) * 100,
      SE_Heterose = sqrt((3 * MSe) / (2 * r))
    )
  Genitor <- pmax(GM, GP)
  data <- data %>%
    mutate(
      Heterobeltiose = ((PR - Genitor) / Genitor) * 100,
      SE_Heterobeltiose = sqrt((2 * MSe) / r)
    )

  if (param == "all") {
    return(data[, c("GEN", "Heterosis", "SE_Heterosis", "Heterobeltiosis",
                    "SE_Heterobeltiosis")])

  } else if (param == "het") {
    return(data[, c("GEN", "Heterosis", "SE_Heterosis")])

  } else if (param == "hetb") {
    return(data[, c("GEN", "Heterobeltiosis", "SE_Heterobeltiosis")])
  }
}
