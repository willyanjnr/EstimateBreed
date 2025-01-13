#'Função para estimar a probabilidade de extrair linhagens superiores de populações
#' Método de Jinks and Pooni
#'
#' Port, E. D., Carvalho, I. R., Pradebon, L. C., Loro, M. V., Colet, C. D. F.,
#'Silva, J. A. G. D., & Sausen, N. H. (2024).
#' Early selection of resilient progenies to seed yield in soybean populations.
#'  Ciência Rural, 54, e20230287.
#
#' @param name description
#'
#' @export

Jinks_Pooni<-function(População, Var, VG, Testemunhas){

  require(dplyr)
  require(crayon)
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


  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
  green(italic(cat("Probabilidade de extrair linhagens superiores da populações
                   - Método de Jinks and Pooni")))
  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
  green(italic(cat("Parâmetros")))
  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
  print(Parâmetros)
  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
}
