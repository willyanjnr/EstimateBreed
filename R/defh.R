#' Determinação do Déficit Hídrico
#' @description
#'
#' Estimativa do déficit hídrico das culturas agrícolas
#'
#'@param CICLO Coluna referente ao ciclo da cultura
#'@param FASE Fase em que a cultura se encontra
#'@param ETo Evapotranspiração potencial do dia
#'@param Cultura Definir a cultura (Padrão = "Soja")
#'@param ADP Água Disponível Predita
#'@param PROF Profundidade Aproximada do Solo (em cm)
#'@param plot Plotar um gráfico do acúmulo (Padrão é F (FALSE))
#'@export

dhid <- function(CICLO,FASE,ETo,AD=4,PROF=100,Cultura="Soja"){
  CICLO <- CICLO
  FASE <- FASE
  ETo <- ETo
  if(FASE=="VEGET")
  ETc <- 3.5
  BH = ETo-ETc
  if(FASE=="REPROD"){
  ETc <- 7
  BH = ETo-ETc
  }
#CAD = (CC-PMP)*Z*1000
}
