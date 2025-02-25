#'Complete vigor index
#'@description
#'Determining the multivariate vigor of seeds
#'@param GEN The column with the genotype name
#'@param PC The column with the values from the first count
#'@param G The column with the values of percentage of germinated seeds
#'@param CPA The column with the values of shoot length
#'@param RAD The column with the values of Root Length
#'@param MS The column with the values of dry mass
#'@param EC TThe column with the field emergency values
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Szareski, V. J., Carvalho, I. R., Demari, G. H., Rosa, T. C. D.,
#'Souza, V. Q. D., Villela, F. A.,Aumonde, T. Z. (2018).
#'Multivariate index of soybean seed vigor:
#'a new biometric approach applied to the effects of genotypes and
#'environments. Journal of Seed Science, 40(4), 396-406.
#'@seealso \code{\link{ivig_simp}}
#'@export

index_vigor <- function(GEN,PC,G,CPA,RAD,MS,EC){

genot <- as.factor(GEN)
variav1 <- PC
variav2 <- G
variav3 <- CPA
variav4 <- RAD
variav5 <- MS
variav6 <- EC

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
cat("Complete Vigor Index by Genotype")
cat("\n-----------------------------------------------------------------\n")
print(dadosfinal)
}

#'Simple Vigor Index
#'@description
#'Simple seed vigor index described by Szareski et al. (2018).
#'@param GEN The column with the genotypes.
#'@param PC First count values
#'@param G Germination percentage
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Szareski, V. J., Carvalho, I. R., Kehl, K., Levien, A. M., Nardino,
#'M., Dellagostin, S. M., ... & Aumonde, T. Z. (2018).
#'Adaptability and stability of wheat genotypes according to the
#'phenotypic index of seed vigor. Pesquisa Agropecuaria Brasileira,
#'53, 727-735.
#'@seealso \code{\link{index_vigor}}
#'@export

ivig_simp <- function(GEN,PC,G){

  genot <- as.factor(GEN)
  variav1 <- PC
  variav2 <- G

  sd_pc <- sd(variav1)
  sd_g <- sd(variav2)

  final <- data.frame(sd_pc,sd_g)
  indice <- ((variav1/sd_pc)*(variav2/sd_g))

  dadosfinal <- data.frame(genot,indice)
  cat("\n-----------------------------------------------------------------\n")
  cat("Simple Vigor Index by Genotype")
  cat("\n-----------------------------------------------------------------\n")
  print(dadosfinal)
}

#'Selection for Grain Volume
#'@description
#'Calculation of the selection index for grain volume, based on the values for
#'grain length, width and thickness
#'@param GEN The column with the genotype name
#'@param C Grain length
#'@param L Grain width
#'@param E Grain thickness
#'@param stat Extract or not the average per genotype. Use 'everything' to obtain
#' information on all the observations or 'mean' to extract the average.
#'@param plot Logical argument. Plot a graph if TRUE
#'@param ylab Y axis name
#'@param xlab X axis name
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Carvalho, I. R., de Pelegrin, A. J., Szareski, V. J., Ferrari, M., da Rosa, T.
#' C., Martins, T. S., dos Santos, N. L., Nardino, M., de Souza, V. Q., de
#' Oliveira, A. C., & da Maia, L. C. (2017). Diallel and prediction (REML/BLUP)
#' for yield components in intervarietal maize hybrids. Genetics and Molecular
#' Research, 16(3), gmr16039734.
#'  https://doi.org/10.4238/gmr16039734
#'@export
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'Gen = rep(paste0("G", 1:10), each = 3)
#'Rep = rep(1:3, times = 10)
#'L = round(rnorm(30, mean = 3.2, sd = 0.3), 2)
#'C = round(rnorm(30, mean = 8.5, sd = 0.5), 2)
#'E = round(rnorm(30, mean = 2.1, sd = 0.2), 2)
#'
#'data <- data.frame(Gen,Rep,L,C,E)
#'
#'with(data,gvri(Gen,C,L,E, stat="mean", plot=TRUE))
#'}

gvri <- function(GEN,C,L,E,stat="everything",plot=F,ylab="GVRI",xlab="Genotype"){

  GEN <- as.factor(GEN)
  Comp <- C
  Larg <- L
  Esp <- E
  if(stat=="everything"){
    gvri <- ((Comp*Larg*Esp)/sum(Comp+Larg+Esp))
    final <- data.frame(GEN,gvri)
    cat("\n-----------------------------------------------------------------\n")
    cat("Grain Volume Relative Index (GVRI)")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if (stat=="mean"){
    gvri <- ((Comp*Larg*Esp)/sum(Comp+Larg+Esp))
    dados <- data.frame(GEN,gvri)
    media_geral <- mean(dados$gvri)
    media_gen <- aggregate(gvri ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    cat("\n-----------------------------------------------------------------\n")
    cat("Grain Volume Relative Index (Mean by Genotype)")
    cat("\n-----------------------------------------------------------------\n")
    print(media_gen)
    cat("General Mean =",paste(round(media_geral,digits = 5)))
    if(plot==T){
      grafico <- ggplot(media_gen, aes(x=GEN, y=gvri)) +
        geom_bar(stat = "identity")+
        geom_hline(yintercept = media_geral, color = "red", linetype = "dashed", size = 1)+
        ylab(ylab)+xlab(xlab)+theme_classic()
      plot(grafico)
    }
  }
}
