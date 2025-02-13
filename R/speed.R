#'Estimativa dos ganhos por Speed Breeding
#'@description
#'Função para determinar o tempo ganho com a técnica do speed breeding na soja,
#'conforme descrito por Mescouto et al. (2024).
#'@param GEN Genótipos a serem selecionados.
#'@param HD Data da colheita nos estádios R6, R7 e R8.
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#'@references
#'Mescouto, L. F. L., Piza, M. R., Costa, J. C., Pessoni, L. O., Bruzi, A. T.,
#'Pulcinelli, C. E., & Rocha, T. T. T. DA. (2024). Early harvesting: An efficient
#'technique for speed breeding in soybean. Genetics and Molecular Research, 23(1).
#'https://doi.org/10.4238/gmr19232
#'@export
#'@examples
#'\donttest{
#' library(Breeding)
#'
#' data("aveia")
#' with(aveia,estresse(GEN,MC,MG,index = "ALL",bygen=T))
#'
#'}

#Finalizar
speedb <- function(GEN,HD){
  #Coeficiente de determinação genético
  QCG = (MSG-MSGE)/te
  CVge = (MSGE-MSE)/te
  GCD = QCG/(QCG+t/e*(t-1)*QCge+CVg/we)
  CV
  HAGD
  HAGPERC
}
