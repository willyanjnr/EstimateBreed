#'Índice de vigor multivariado em sementes
#'@description
#'Determinação do vigor multivariado de sementes
#'@param PC desc
#'@param G desc
#'@param CPA desc
#'@param RAD desc
#'@param MS desc
#'@param EC desc
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Szareski, V. J., Carvalho, I. R., Demari, G. H., Rosa, T. C. D.,
#'Souza, V. Q. D., Villela, F. A.,Aumonde, T. Z. (2018).
#'Multivariate index of soybean seed vigor:
#'a new biometric approach applied to the effects of genotypes and
#'environments. Journal of Seed Science, 40(4), 396-406.
#'@export

index_vigor <- function(GEN,PC,G,CPA,RAD,MS,EC){
require("dplyr")
require("ggplot2")

genot <- as.factor(GEN)
variav1 = PC
variav2 = G
variav3 = CPA
variav4 = RAD
variav5 = MS
variav6 = EC

sd_pc <- sd(variav1)
sd_g <- sd(variav2)
sd_cpa <- sd(variav3)
sd_rad <- sd(variav4)
sd_ms <- sd(variav5)
sd_ec <- sd(variav6)

final <- data.frame(sd_pc,sd_g,sd_cpa,sd_rad,sd_ms,sd_ec)
indice <- ((variav1/sd_pc)*(variav2/sd_g)*(variav3/sd_cpa)*(variav4/sd_rad)*(variav5/sd_ms)*(variav6/sd_ec))

dadosfinal <- data.frame(genot,indice)
cat("\n-----------------------------------------------------------------\n")
cat("Genótipo")
cat("\n-----------------------------------------------------------------\n")
print(dadosfinal)
}

#'Índice de vigor multivariado em sementes - SIMPLES
#'@description
#' Índice de vigor simples de sementes (Szareski et al., 2018).
#'@param PC desc
#'@param G desc
#'@references
#'Szareski, V. J., Carvalho, I. R., Kehl, K., Levien, A. M., Nardino,
#'M., Dellagostin, S. M., ... & Aumonde, T. Z. (2018).
#'Adaptability and stability of wheat genotypes according to the
#'phenotypic index of seed vigor. Pesquisa Agropecuária Brasileira,
#'53, 727-735.
#'@export

ivig_simp <- function(GEN,PC,G){
  requireNamespace("dplyr")

  genot <- as.factor(GEN)
  variav1 = PC
  variav2 = G

  sd_pc <- sd(variav1)
  sd_g <- sd(variav2)

  final <- data.frame(sd_pc,sd_g)
  indice <- ((variav1/sd_pc)*(variav2/sd_g))

  dadosfinal <- data.frame(genot,indice)
  cat("\n-----------------------------------------------------------------\n")
  cat("Genótipo")
  cat("\n-----------------------------------------------------------------\n")
  print(dadosfinal)
}

#'Índice de Seleção para Volume de Grão
#'@description
#'Índice de seleção para volume de grão proposto por Carvalho et al. (2017).
#'@param GEN A coluna com o nome dos genótipos.
#'@param C Comprimento do Grãos
#'@param L Largura do Grão
#'@param E Espessura do Grão
#'@param stat Extrair ou não a média por genótipo. Padrão é `"all"`.
#'@param plot Plotar o gráfico se `T`
#'@param ylab Nome do eixo Y
#'@param xlab Nome do eixo X
#'@references
#' Carvalho, I. R., Pelegrin, A. D., Szareski, V. J., Ferrari, M.,
#' Rosa, T. D., Martins, T. S., ... & Maia, L. D. (2017).
#'Diallel and prediction (REML/BLUP) for yield components in intervarietal maize hybrids.
#' Genet Mol Res, 16(3).
#'@export
#'
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
