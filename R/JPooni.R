#'Método de Jinks and Pooni
#'@description
#'Função para estimar a probabilidade de extrair linhagens superiores de populações
#'Método de Jinks and Pooni
#'
#'@param name description
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#'@references
#'Port, E. D., Carvalho, I. R., Pradebon, L. C., Loro, M. V., Colet, C. D. F.,
#'Silva, J. A. G. D., & Sausen, N. H. (2024).
#'Early selection of resilient progenies to seed yield in soybean populations.
#'Ciência Rural, 54, e20230287.
#'@export

#Finalizar
Jinks_Pooni<-function(População, Var, VG, Testemunhas){

  require(dplyr)
  require(ggplot2)

  População = População
  Var = Var
  VG = VG
  Testemunhas = Testemunhas

  Z<-((Testemunhas-Var)/(sqrt(VG)))
  P<-((1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE))*100)
  Valor_Genético<- ifelse(P > 50, "alto", "baixo")

  Parâmetros<-
    data.frame(População, Z, P, Valor_Genético)


  cat("\n-----------------------------------------------------------------\n")
  cat("Probabilidade de extrair linhagens superiores da populações
                   - Método de Jinks and Pooni")
  cat("\n-----------------------------------------------------------------\n")
  cat("Parâmetros")
  cat("\n-----------------------------------------------------------------\n")
  print(Parâmetros)
  cat("\n-----------------------------------------------------------------\n")
}
