#'Índice de vigor multivariado em sementes

#'Citação: "A new method for mutation inducing in rice
#'by using DC electrophoresis bath and its mutagenic ef

#'@param mut Método de mutação
#'@param MSG Média de Sementes Germinadas
#'@param MST Média de Sementes Total
#'@param GT Número de plântulas por dia durante o tempo t
#'@param DT Número de Dias Avaliados
#'@param SL Comprimento da plântula
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

  # Cálculo de Germinação Relativa (GR)
  GR <- (MSG/MST)*100

  # Cálculo do Índice de Germinação (GI)
  GI <- GT/DT

  # Cálculo do Índice de Vitalidade (VI)
  VI <- SL*GI

  # Retornar os resultados
  return(list(GR = GR, GI = GI, VI = VI))
}
