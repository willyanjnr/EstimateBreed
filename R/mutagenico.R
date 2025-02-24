#'Multivariate seed vigor index
#'@description
#'Determining the vigor of seeds obtained from mutation induction processes
#'@param mut Mutation method
#'@param MSG The column with the average number of germinated seeds
#'@param MST The column with the average total seeds
#'@param GT Number of seedlings germinated per day during 't' time
#'@param DT Number of evaluation days
#'@param SL Shoot Length
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Zou, M., Tong, S., Zou, T. et al. A new method for mutation inducing in rice
#'by using DC electrophoresis bath and its mutagenic effects. Sci Rep 13, 6707
#'(2023). https://doi.org/10.1038/s41598-023-33742-7
#'@export

mut_index <- function(mut=NULL,MSG,MST,GT,DT,SL) {

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
