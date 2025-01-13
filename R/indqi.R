#'Estimativa do índice de seleção para qualidade industrial do trigo
#'@description
#'Função para determinar índices de qualidade industrial de genótipos de trigo,
#'descritas por Szareski et al. (2019).
#'@param GEN A coluna com o nome do genótipo
#'@param NQ A coluna com o valor do falling number
#'@param W A coluna com a força de glúten (W)
#'@param PTN A coluna com os valores de proteína
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Szareski, V. J., Carvalho, I. R., Kehl, K., Levien, A. M.,
#'Lautenchleger, F., Barbosa, M. H., ... & Aumonde, T. Z. (2019).
#'Genetic and phenotypic multi-character approach applied to multivariate
#'models for wheat industrial quality analysis.
#'Genetics and Molecular Research, 18(3), 1-14.
#'@export

is_qindustrial <- function(GEN, NQ, W, PTN){
  require("dplyr")

  genot <- as.factor(GEN)
  variav1 = NQ
  variav2 = W
  variav3 = PTN

  sd_FN <- sd(variav1)
  sd_W <- sd(variav2)
  sd_PTN <- sd(variav3)
  desvios <- data.frame(sd_FN,sd_W,sd_PTN)

  final <- data.frame(sd_FN,sd_W,sd_PTN)
  indice <- ((variav1/sd_FN)*(variav2/sd_W)*(variav3/sd_PTN))

  dadosfinal <- data.frame(genot,indice)
  colnames(dadosfinal) <- c("GEN","Index")
  cat("\n-----------------------------\n")
  cat("Índice de Qualidade do Trigo")
  cat("\n-----------------------------\n")
  print(dadosfinal)
  cat("\n-----------------------------\n")
  cat("Desvios")
  cat("\n-----------------------------\n")
  print(desvios)
}

#'Índice de Descasque e Rendimento Industrial
#'@description
#'Cálculo do Índice de Descasque e Rendimento Industrial de Aveia Branca
#'@param GEN A coluna com o nome dos genótipos.
#'@param NG2M Coluna com os valores do número de grãos maior que 2mm.
#'@param MG Coluna com os valores da massa de grãos.
#'@param MC Coluna com os valores da massa de cariópse.
#'@param RG Colunas com os valores do rendimento de grãos (kg/ha).
#'@param stat Extrair ou não a média por genótipo. Padrão é `"all"`.
#'@export
#'@examples
#'\donttest{
#' library(Breeding)
#'
#'data("aveia")
#'# Calcular o rendimento industrial sem extrair a média
#'with(aveia, rendind(GEN, NG2M, MG, MC, RG))
#'
#'# Calcular o rendimento industrial extraindo a média por genótipo
#'with(aveia, rendind(GEN, NG2M, MG, MC, RG, stat = "mean"))
#'}
rend_ind <- function(GEN,NG2M,MG,MC,RG,stat="all",...){
  require(dplyr)
  GEN = as.factor(GEN)
  NG2M = NG2M
  MG = MG
  MC = MC
  RG = RG
  if(stat=="all"){
    ID = MC/MG
    RI = RG*(NG2M/100)*ID
    final <- data.frame(GEN,ID,RI)
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice de Descasque e Rendimento Industrial")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if (stat=="mean"){
    ID = MC/MG
    RI = RG*(NG2M/100)*ID
    media_gen <- aggregate(cbind(ID, RI) ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice de Descasque e Rendimento Industrial (Média por Genótipo)")
    cat("\n-----------------------------------------------------------------\n")
    print(media_gen)
  }
}

#'Estimativa de índices de espiga
#'@description
#'Estimativa do índice de viabilidade a partir de duas variáveis de campo.
#'@param genot Nome da coluna que contém os genótipos
#'@param var1 Nome da coluna que contém a primeira variável
#'@param var2 Nome da coluna que contém a segunda variável
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export
#'@examples
#'with(data,indviab(genot,var1,var2)) #Ajustar o exemplo###

indviab <- function(GEN,var1,var2,ylab="Índice",xlab="Genótipo",stat="all",plot=F){
  require("dplyr")
  require("ggplot2")

  GEN = as.factor(GEN)
  variav1 = var1
  variav2 = var2

  if(stat=="all"){
    indesp <- variav1/variav2
    mediaind <- mean(indesp)
    dados=data.frame(genot,indesp)
    colnames(dados) <- c("Genótipo","Índice")
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice de Viabilidade Geral")
    cat("\n-----------------------------------------------------------------\n")
    print(dados)
  }
  else if (stat=="mean"){
    indesp = variav1/variav2
    dados <- data.frame(GEN,indesp)
    media_gen <- aggregate(indesp ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    colnames(media_gen) <- c("Genótipo","Índice")
    if(plot==T){
      grafico=ggplot(media_gen, aes(x=Genótipo, y=Índice)) +
        geom_bar(stat = "identity")+
        ylab(ylab)+xlab(xlab)+theme_classic()
      print(grafico)
    }
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice de Viabilidade (Média por Genótipo)")
    cat("\n-----------------------------------------------------------------\n")
    print(media_gen)
  }}

#' Determinação do peso do hectolitro de cereais
#' @description
#' Função útil para caracterizar o peso do hectolitro (PH) de experimentos com
#' cereais.
#' @param GEN A coluna com o nome do genótipo
#' @param PESO Peso obtido em balança de 1qt lt
#' @return Retorna o valor estimado do peso do hectolitro (PH).
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#' @export

ph <- function(GEN, PESO, crop="trigo"){
  require(dplyr)
  dados <- data.frame(GEN, PESO)

  if(crop == "trigo"){
    dados <- dados %>%
      mutate(PH = format(-9.935757 + (PESO * 0.451821), nsmall = 2))
    return(dados)
  } else if (crop == "aveia"){
    dados <- dados %>%
      mutate(PH = format(-3.512294 + (PESO * 0.425507), nsmall = 2))
    return(dados)
  }
}

#'Índice de seleção para proteína x rendimento de grãos
#'@description
#'Índice de seleção para proteína e rendimento de grãos (Pelegrin et al., 2017).
#'@param GEN A coluna com o nome dos genótipos
#'@param PTN Proteina Bruta
#'@param RG Rendimento de grãos
#'@references
#'de Pelegrin, A. J., Carvalho, I. R., Nunes, A. C. P., Demari, G. H., Szareski,
#'V. J., Barbosa, M. H., ... & da Maia, L. C. (2017).
#'Adaptability, stability and multivariate selection by mixed models.
#'American Journal of Plant Sciences, 8(13), 3324.
#'@export

is_ptnerg <- function(GEN, PTN, RG){
  requireNamespace("dplyr")
  requireNamespace("crayon")

  genot <- as.factor(GEN)
  variav1 = PTN
  variav2 = RG

  sd_ptn <- sd(variav1)
  sd_rg <- sd(variav2)

  final <- data.frame(sd_ptn,sd_rg)
  indice <- ((variav1/sd_ptn)*(variav2/sd_rg))

  dadosfinal <- data.frame(genot,indice)
  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
  green(italic(cat("Genótipo")))
  cat(crayon::white(bold("\n-----------------------------------------------------------------\n")))
  print(dadosfinal)
}
