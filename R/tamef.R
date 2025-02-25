#'Effective Population Size
#'@description
#'Estimated effective population size adapted from Morais (1997).
#'@param GEN The column with the name of the genotype (progeny).
#'@param SI The column with the number of individuals selected
#'@param NE Number of individuals conducted during the selection period.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

tamef <- function(GEN,SI,NE){

  data <- data.frame(GEN,SI,NE)
  Sum_SI <- (sum(data$SI))^2
  data <- data %>%
    mutate(Gen_SI=SI^2)
  Pond_SI <- sum(data$Gen_SI/data$NE)
  Nej <- Sum_SI/Pond_SI
  return(Nej)
}
