#'Índice de vigor multivariado em sementes
#'@description
#'Citação: "A new method for mutation inducing in rice
#'by using DC electrophoresis bath and its mutagenic ef
#'@param mut Método de mutação
#'@param MSG Média de Sementes Germinadas
#'@param MST Média de Sementes Total
#'@param GT Número de plântulas por dia durante o tempo t
#'@param DT Número de Dias Avaliados
#'@param SL Comprimento da plântula
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#' @references
#' Zou, M., Tong, S., Zou, T. et al. A new method for mutation inducing in rice
#' by using DC electrophoresis bath and its mutagenic effects. Sci Rep 13, 6707
#' (2023). https://doi.org/10.1038/s41598-023-33742-7
#'@export

mut_index <- function(mut,MSG,MST,GT,DT,SL) {
  require(dplyr)
  require(ggplot2)
  mut <- mut
  MSG <- MSG
  MST <- MST
  GT <- GT
  DT <- DT
  PER <- PER
  SL <- SL

  #Cálculo da Germinação Relativa (GR)
  GR <- (MSG/MST)*100

  #Cálculo do Índice de Germinação (GI)
  GI <- GT/DT

  #Cálculo do Índice de Vitalidade (VI)
  VI <- SL*GI

  # Retornar os resultados
  return(list(GR = GR, GI = GI, VI = VI))
}
