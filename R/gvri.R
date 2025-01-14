#' Índice de Seleção para Volume de Grão
#' @description
#'
#' Cálculo do Índice de Seleção para Volume de Grão
#'
#'@param GEN Genótipo
#'@param C Comprimento do Grãos
#'@param L Largura do Grão
#'@param E Espessura do Grão
#'@param stat Extrair ou não a média por genótipo. Padrão é `"all"`.
#'@param plot Plotar o gráfico se `T`
#'@param ylab Nome do eixo Y
#'@param xlab Nome do eixo X
#'@export
gvri <- function(GEN,C,L,E,stat="all",plot=F,ylab="GVRI",xlab="Genótipo"){
  require(dplyr)
  require(ggplot2)
  GEN = as.factor(GEN)
  Comp = C
  Larg = L
  Esp = E
  if(stat=="all"){
    gvri = ((Comp*Larg*Esp)/sum(Comp+Larg+Esp))
    final <- data.frame(GEN,gvri)
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice Relativo do Volume de Grão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if (stat=="mean"){
    gvri = ((Comp*Larg*Esp)/sum(Comp+Larg+Esp))
    dados <- data.frame(GEN,gvri)
    media_geral <- mean(dados$gvri)
    media_gen <- aggregate(gvri ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    cat("\n-----------------------------------------------------------------\n")
    cat("Índice Relativo do Volume de Grão (Média por Genótipo)")
    cat("\n-----------------------------------------------------------------\n")
    print(media_gen)
    cat("Média Geral =",paste(round(media_geral,digits = 5)))
    if(plot==T){
      grafico=ggplot(media_gen, aes(x=GEN, y=gvri)) +
      geom_bar(stat = "identity")+
      geom_hline(yintercept = media_geral, color = "red", linetype = "dashed", size = 1)+
      ylab(ylab)+xlab(xlab)+theme_classic()
      plot(grafico)
    }
  }
}
