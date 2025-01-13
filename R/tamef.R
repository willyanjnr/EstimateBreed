#'Tamanho Efetivo Populacional
#'@description
#'Estimativa do tamanho efetivo populacional adaptado por Morais (1997).
#'@param GEN A coluna com o nome do genótipo (progênie).
#'@param SI A coluna com o número de indivíduos selecionados.
#'@param NE Número de indivíduos conduzidos no período de seleção.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho.
#'@export

tamef <- function(GEN,SI,NE){
  require(dplyr)
  data <- data.frame(GEN,SI,NE)
  Sum_SI = (sum(data$SI))^2
  data <- data %>%
    mutate(Gen_SI=SI^2)
  Pond_SI=sum(data$Gen_SI/data$NE)
  Nej=Sum_SI/Pond_SI
  return(Nej)
}
