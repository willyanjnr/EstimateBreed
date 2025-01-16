#' Função para cálculo da heterose e heterobeltiose
#' @description
#' Cálculo dos parâmetros da heterose e heterobeltiose de híbridos de milho
#' @param GEN A coluna com o nome do genótipo
#' @param GM A coluna com a média do genitor materno
#' @param GP A coluna com a média do genitor paterno
#' @param PR A coluna com a média da progênie
#' @param param Valor para determinar o parâmetro a ser calculado. Padrão é "all".
#' Para calcular apenas a heterose, utilize "het". Para calcular apenas a
#' heterobeltiose, utilize "hetb".
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho.
#'@export
#'@examples
#'\donttest{
#' library(Breeding)
#'
#' data("maize")
#' #Extrair heterose e heterobeltiose
#' with(maize,heterose(HIB,GM,GP,P,stat="all"))
#'
#' #Extrair apenas heterose
#' with(maize,heterose(HIB,GM,GP,P,param = "het"))
#'
#' #Extrair apenas heterobeltiose
#' with(maize,heterose(HIB,GM,GP,P,param = "hetb"))
#'}

heterose <- function(GEN,
                     GM,
                     GP,
                     PR,
                     param = "all"){
  require(dplyr)
  GEN=GEN
  GM=GM
  GP=GP
  PR=PR
  data <- data.frame(GEN,GM,GP,PR)

  if(param=="all"){
    data <- data %>%
    mutate(
      het<-((PR-((GM+GP)/2))/
              ((GM+GP)/2))*100
    )
    Genitor <- apply(data[, c("GM", "GP")], 1, max)
    data <- data %>%
      mutate(
      hetb <- ((PR-Genitor)/Genitor)*100
      )
    colnames(data)<-c("GEN","GM","GP","PR","Heterose","Heterobeltiose")
    df_s <- data[,c(1,5,6)]
    return(df_s)
  } else if (param=="het"){
    data <- data %>%
      mutate(
        het<-((PR-((GM+GP)/2))/
                ((GM+GP)/2))*100
      )
    colnames(data)<-c("GEN","GM","GP","PR","Heterose")
    df_s <- data[,c(1,5)]
    return(df_s)
  } else if (param=="hetb"){
    Genitor <- apply(data[, c("GM", "GP")], 1, max)
    data <- data %>%
      mutate(
        hetb <- ((PR-Genitor)/Genitor)*100
      )
    colnames(data)<-c("GEN","GM","GP","PR","Heterobeltiose")
    df_s <- data[,c(1,5)]
    return(df_s)
}
}
