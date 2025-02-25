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
