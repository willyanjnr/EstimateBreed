#'Função para cálculo da heterose e heterobeltiose
#'@description
#'Cálculo dos parâmetros da heterose e heterobeltiose de híbridos de milho
#'@param GEN A coluna com o nome do genótipo
#'@param GM A coluna com a média do genitor materno
#'@param GP A coluna com a média do genitor paterno
#'@param PR A coluna com a média da progênie
#'@param REP A coluna com as repetições
#'@param param Valor para determinar o parâmetro a ser calculado. Padrão é "all".
#'Para calcular apenas a heterose, utilize "het". Para calcular apenas a
#'heterobeltiose, utilize "hetb".
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#'@export
#'@examples
#'\donttest{
#' library(Breeding)
#'
#' data("maize")
#' #Extrair heterose e heterobeltiose
#' with(maize,heterose(HIB,GM,GP,P,stat="all"))
#'
#' #Extrair apenas heterose
#' with(maize,heterose(HIB,GM,GP,P,param = "het"))
#'
#' #Extrair apenas heterobeltiose
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
    return(data[, c("GEN", "Heterose", "SE_Heterose", "Heterobeltiose", "SE_Heterobeltiose")])

  } else if (param == "het") {
    return(data[, c("GEN", "Heterose", "SE_Heterose")])

  } else if (param == "hetb") {
    return(data[, c("GEN", "Heterobeltiose", "SE_Heterobeltiose")])
  }
}
