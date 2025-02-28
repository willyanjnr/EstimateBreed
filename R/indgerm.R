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

#' Germination index by subsequent counting
#' @description
#' Estimation of germination index by subsequent seedling count
#' germinated in a given period (Wang et al., 2017)
#' @param TESTE Identification number of the test performed
#' @param DIA Numerical values for the days tested
#' @param TSG Name of the column with the total number of seeds germinated in
#'  each period
#' @param NST Default column name with number of seeds tested
#' @return Returns the germination index by subsequent count.
#' @author Willyan Junior Adorian Bandeira
#' @author Ivan Ricardo Carvalo
#' @author Murilo Vieira Loro
#' @author Leonardo Cesar Pradebon
#' @author Jose Antonio Gonzalez da Silva
#' @export
#' @references
#' Wang, Pan, Li, Dong, Wang, Li-jun and Adhikari, Benu. "Effect of High
#' Temperature Intermittent Drying on Rice Seed Viability and Vigor" International
#' Journal of Food Engineering, vol. 13, no. 10, 2017, pp. 20160433.
#' https://doi.org/10.1515/ijfe-2016-043

indger <- function(TESTE,DIA,TSG,NST){
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

#'AIC and black oat seed multitrait
#'@description
#'Multitrait selection index for variables of interest
#'@param fc First germination count.
#'@param germ Germination percentage.
#'@param sdm Shoot dry mass.
#'@param sl Shoot lenght.
#'@param radl Radicle lenght.
#'@param index Argument to choose the desired index.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Moura, N. B., Carvalho, I. R., Silva, J. A. G., Loro, M. V., Barbosa, M. H.,
#'Lautenchleger, F., Marchioro, V. S., & Souza, V. Q. (2021). Akaike criteria
#'and selection of physiological multi-character indexes for the production of
#'black oat seeds. Current Plant Studies, 11, 1-8.
#'https://doi.org/10.26814/cps2021003
#'@export

tindex <- function(fc, germ, sdm, sl, radl, index="PI") {
  #Funcao nao testada

  variaveis <- list(fc = fc, germ = germ, sdm = sdm, sl = sl, radl = radl)
  medias <- sapply(variaveis, mean)
  desvios <- sapply(variaveis, sd)

  #Fenotipicas
  indices <- lapply(names(variaveis), function(nome) {
    valores <- variaveis[[nome]]
    media <- medias[nome]
    desvio <- desvios[nome]
    PI <- valores / desvio
    ZI <- (valores - media) / desvio
    pesos <- c(0.5, 0.2, 0.125, 0.125, 0.05)
    WI <- sum(medias * pesos)
    data.frame(Variavel = nome, Valor_Original = valores, PI = PI, ZI = ZI, WI = WI)
  })

  #Valor Genetico
  modelo_fc <- lmer(fc ~ 1 + (1|repeticao), data = dados, REML = TRUE)
  modelo_germ <- lmer(germ ~ 1 + (1|repeticao), data = dados, REML = TRUE)
  modelo_sdm <- lmer(sdm ~ 1 + (1|repeticao), data = dados, REML = TRUE)
  modelo_sl <- lmer(sl ~ 1 + (1|repeticao), data = dados, REML = TRUE)
  modelo_radl <- lmer(radl ~ 1 + (1|repeticao), data = dados, REML = TRUE)
  #Multiplicative Index
  MI <- fixef(modelo_fc)*fixef(modelo_sl)*fixef(modelo_sdm)*
    fixef(modelo_germ)*fixef(modelo_radl)
  #Mulamba Mock
  MMI <- (fixef(modelo_fc)*0.5)+(fixef(modelo_sl)*0.2)+(fixef(modelo_sdm)*0.125)
  +(fixef(modelo_germ)*0.125)+(fixef(modelo_radl)*0.05)
  #Resultados finais
  resultado_final <- do.call(rbind, indices)
  return(resultado_final)
}

#'Multivariate seed vigor index
#'@description
#'Determining the vigor of seeds obtained from mutation induction processes
#'@param mut Mutation method
#'@param MSG The column with the average number of germinated seeds
#'@param MST The column with the average total seeds
#'@param GT Number of seedlings germinated per day during 't' time
#'@param DT Number of evaluation days
#'@param SL Shoot Length
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Zou, M., Tong, S., Zou, T. et al. A new method for mutation inducing in rice
#'by using DC electrophoresis bath and its mutagenic effects. Sci Rep 13, 6707
#'(2023). https://doi.org/10.1038/s41598-023-33742-7
#'@export

mut_index <- function(mut=NULL,MSG,MST,GT,DT,SL) {

  mut <- mut
  MSG <- MSG
  MST <- MST
  GT <- GT
  DT <- DT
  PER <- PER
  SL <- SL

  #Calculo da Germ Relativa (GR)
  GR <- (MSG/MST)*100

  #Calculo do Indice de Germ (GI)
  GI <- GT/DT

  #Calculo do Indice de Vitalidade (VI)
  VI <- SL*GI

  # Retornar os resultados
  return(list(GR = GR, GI = GI, VI = VI))
}
