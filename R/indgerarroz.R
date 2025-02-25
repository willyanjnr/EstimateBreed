#' Germination index by subsequent counting
#' @description
#' Estimation of germination index by subsequent seedling count
#' germinated in a given period (Wang et al., 2017)
#' @param TESTE Identification number of the test performed
#' @param DIA Numerical values for the days tested
#' @param TSG Name of the column with the total number of seeds germinated in each period
#' @param NST Default column name with number of seeds tested
#' @export
#' @references
#' Wang, Pan, Li, Dong, Wang, Li-jun and Adhikari, Benu. "Effect of High
#' Temperature Intermittent Drying on Rice Seed Viability and Vigor" International
#' Journal of Food Engineering, vol. 13, no. 10, 2017, pp. 20160433.
#' https://doi.org/10.1515/ijfe-2016-043
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#' @examples
#' with(data,indviab(genot,var1,var2))

#Ainda falta terminar!
indger <- function(TESTE,DIA,TSG,NST){
  TESTE <- TESTE
  DIA <- DIA
  TSG <- TSG
  NST <- NST
  values <- data.frame(TESTE,DIA,TSG,NST)

  #Posso ter mais de um teste, preciso adicionar uma forma de identificar isso
  TSD4 <- values$TSG[values$DIA==4]

  #Energia da Germ
  GE <- (TSD4/values$NST[1])*100
  #Potencial de Germ
  GP <- (max(values$TSG)/values$NST[1])*100
  #Tempo para obter 50 percent da germ
  #Tempo medio para germ
  D1 <- values$DIA[which(values$TSG != 0)[1]]
  vgerm <- values[D1:nrow(values),]
  vgerm$acum <- cumsum(vgerm$TSG)
  MGT <- vgerm$acum/sum(values$TSG)
  #Indice de germ
  #Sintaxe abaixo obtida no GPT, ajustar
  dados_simulados$indice_germinacao <- ifelse(
    dados_simulados$dia >= dados_simulados$dia[primeiro_dia_germinacao],
    dados_simulados$total_sementes / dados_simulados$sementes_avaliadas * 100,
    NA
  )
  final <- data.frame(GE,GP,MGT)

  cat("\n--------------------\n")
  cat("Germination Index")
  cat("\n--------------------\n")
  print(final)
}
