#'Jinks and Pooni method
#'@description
#'Function for estimating the probability of extracting superior lines from
#'populations by the Jinks and Pooni method
#'@param Pop The column with the population name
#'@param Var The column with the variable name
#'@param VG The column with the genotypic variance values
#'@param Test The column with the witnesses' names
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Port, E. D., Carvalho, I. R., Pradebon, L. C., Loro, M. V., Colet, C. D. F.,
#'Silva, J. A. G. D., & Sausen, N. H. (2024).
#'Early selection of resilient progenies to seed yield in soybean populations.
#'Ciencia Rural, 54, e20230287.
#'@export

#Finalizar
Jinks_Pooni<-function(Pop, Var, VG, Test){

  Population <- Pop
  Var <- Var
  VG <- VG
  Testemunhas <- Test

  Z<-((Testemunhas-Var)/(sqrt(VG)))
  P<-((1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE))*100)
  Gen_Value <- ifelse(P > 50, "High", "Low")

  Parameters <- data.frame(Populations, Z, P, Gen_Value)

  cat("\n-----------------------------------------------------------------\n")
  cat("Probability of extracting superior strains from populations
                   - Jinks and Pooni method")
  cat("\n-----------------------------------------------------------------\n")
  cat("Parameters")
  cat("\n-----------------------------------------------------------------\n")
  print(Parameters)
  cat("\n-----------------------------------------------------------------\n")
}
