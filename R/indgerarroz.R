#' Índice de germinação pela contagem subsequente
#' @description
#' Estimativa do índice de germinação pela contagem subsequente de plântulas
#' germinadas em determinado período (Wang et al., 2017)
#' @param TESTE Número identificador do teste realizdo
#' @param DIA Valores numéricos para os dias testados
#' @param TSG Nome da coluna com o total de sementes germinadas em cada período
#' @param NST Nome da coluna padrão com o número de sementes testadas
#' @export
#' @references
#' Wang, Pan, Li, Dong, Wang, Li-jun and Adhikari, Benu. "Effect of High
#' Temperature Intermittent Drying on Rice Seed Viability and Vigor" International
#' Journal of Food Engineering, vol. 13, no. 10, 2017, pp. 20160433.
#' https://doi.org/10.1515/ijfe-2016-043
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
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

  #Energia da Germinação
  GE <- (TSD4/values$NST[1])*100
  #Potencial de Germinação
  GP <- (max(values$TSG)/values$NST[1])*100
  #Tempo para obter 50% da germinação
  #Tempo médio para germinação
  D1 <- values$DIA[which(values$TSG != 0)[1]]
  vgerm <- values[D1:nrow(values),]
  vgerm$acum <- cumsum(vgerm$TSG)
  MGT <- vgerm$acum/sum(values$TSG)
  #Índice de germinação
  #Sintaxe abaixo obtida no GPT, ajustar
  dados_simulados$indice_germinacao <- ifelse(
    dados_simulados$dia >= dados_simulados$dia[primeiro_dia_germinacao],
    dados_simulados$total_sementes / dados_simulados$sementes_avaliadas * 100,
    NA  # Deixa como NA para os dias antes da germinação
  )
  final <- data.frame(GE,GP,MGT)

  cat("\n--------------------\n")
  cat("Índice de Germinação")
  cat("\n--------------------\n")
  print(final)
}
