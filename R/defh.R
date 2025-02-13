#' Determinação do Déficit Hídrico
#' @description
#' Estimativa do déficit hídrico das culturas agrícolas
#'@param CICLO Coluna referente ao ciclo da cultura
#'@param FASE Fase em que a cultura se encontra
#'@param ETo Evapotranspiração potencial do dia
#'@param Cultura Definir a cultura (Padrão = "Soja")
#'@param ADP Água Disponível Predita
#'@param PROF Profundidade Aproximada do Solo (em cm)
#'@param plot Plotar um gráfico do acúmulo (Padrão é F (FALSE))
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#'@references
#'Bortoluzzi, M. P., Heldwein, A. B., Trentin, R., Maldaner, I. C., & da Silva,
#'J. R. (2020). Risk of occurrence of water deficit in soybean cultivated in
#'lowland soils. Earth Interactions, 24(4), 1-16.
#'https://doi.org/10.1175/EI-D-19-0029.1
#'@export

dhid <- function(CICLO,FASE,ETo,AD=4,PROF=100,Cultura="Soja"){
  #Agrupar por RMG

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
