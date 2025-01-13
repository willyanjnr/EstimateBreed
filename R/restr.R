#'Restrição da variabilidade das testemunhas
#'@description
#'Método para restrição da variabilidade das testemunhas proposta por
#'Carvalho et al. (2023).
#'@param GEN A coluna com o nome do genótipo
#'@param REP A coluna com as repetições
#'@param Xi A coluna com o valor observado para determinado genótipo.
#'@param scenario Cenário a ser utilizado para o cálculo.
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'https://doi.org/10.4025/actasciagron.v45i1.56156
#'@export

restr <- function(GEN, REP, Xi, scenario = "original"){
  require(dplyr)
  require(ggplot2)

  media <- mean(Xi)
  desvio <- sd(Xi)

  lim_1s <- c(media - desvio, media + desvio)

  dentro_limite <- Xi >= lim_1s[1] & Xi <= lim_1s[2]
  ream <- data.frame(GEN = GEN[dentro_limite], REP = REP[dentro_limite], Xi = Xi[dentro_limite])

  # Concatenando genótipos e repetições removidas
  removidos <- GEN[!dentro_limite]
  rep_removidos <- REP[!dentro_limite]
  gen_rep_removidos <- paste(removidos, rep_removidos, sep = "R")

  # Exibindo os genótipos e repetições removidos
  cat("Genótipos e repetições removidos: \n")
  print(gen_rep_removidos)
  cat("-------------------------------------\n")
  cat("Observações reamostradas: \n")
  print(ream)
  cat("-------------------------------------\n")
}
