#' Função auxiliar para o cálculo do ISGR
#' @description
#' Esta função recebe um dataframe com os dados de temperatura e precipitação
#' e calcula o desvio padrão desses parâmetros para cada ambiente.
#' @param ENV Identificação de cada ambiente de seleção (para diferenciar caso
#' haja mais de um ciclo de cultivo).
#' @param TMED Temperatura média do ar (em ºC) ocorrida durante o ciclo de
#' cultivo em cada ambiente.
#' @param PREC Precipitação pluviométrica (em mm) ocorrida durante o ciclo de
#' cultivo em cada ambiente.
#' @return Um dataframe contendo o identificador do ambiente de seleção e
#' os desvios padrões para temperatura e precipitação.
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#' @export
#' @examples
#' \donttest{
#' library(EstimateBreed)
#' data("desvamb")
#'
#' with(desvamb,desv_clim(ENV,TMED,PREC))
#' }

desv_clim <- function(ENV,TMED,PREC) {
  require(dplyr)
  ENV = ENV
  TMED = TMED
  PREC = PREC
  desvio <- data.frame(ENV,TMED,PREC)

  resultado <- desvio %>%
    group_by(ENV) %>%
    summarise(
      STMED = sd(TMED, na.rm = TRUE),
      TMEDR = mean(TMED, na.rm = TRUE),
      SPREC = sd(PREC, na.rm = TRUE),
      PRECIR = sum(PREC, na.rm = TRUE)
    )
  assign("DPclim",resultado,envir = .GlobalEnv)
  return(resultado)
}

################################################################################
#'ISGR - Índice de Seleção Genético para Resiliência.
#'@description
#'Estimativa do índice de seleção para resiliência ambiental (Bandeira et al., 2024).
#'@param GEN Coluna referente aos genótipos. As linhagens devem obrigatoriamente
#'ter o prefixo "L" antes do número. Ex: L139.
#'@param ENV Coluna referente ao ambiente de seleção.
#'@param NG Número de grãos de todos os genótipos avaliados
#'@param MG Massa de grãos de todos os genótipos avaliados
#'@param CICLO Número de dias do ciclo, para definir a precipitação pluviométrica
#'ideal (valor de 3.5 mm por dia)
#'@param req Valor da demanda hídrica diária média para a cultura da soja
#'(padrão 3.5 mm). Pode ser alterado conforme o estádio fenológico.
#'@param stage Parâmetro para definir o estádio fenológico que a cultura se
#'encontra. Utilizar "veg" para vegetativo e "rep" para reprodutivo, caso as
#'avaliações tenham sido realizadas apenas em um determinado período.
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho, Murilo V. Loro,
#' Leonardo C. Pradebon, José A. G. da Silva
#'@references
#'Bandeira, W. J. A., Carvalho, I. R., Loro, M. V., da Silva, J. A. G.,
#'Dalla Roza, J. P., Scarton, V. D. B., Bruinsma, G. M. W., & Pradebon, L. C. (2024).
#'Identifying soybean progenies with high grain productivity and stress resilience
#'to abiotic stresses. Aust J Crop Sci, 18(12), 825-830.
#'https://doi.org/10.21475/ajcs.24.18.12.p98
#' @export
#' @examples
#' \donttest{
#' library(EstimateBreed)
#' #Obter os desvios ambientais
#' data("desvamb")
#' with(desvamb, desv_clim(ENV,TMED,PREC))
#'
#' #Calcular o ISGR
#' data("genot")
#' with(genot, isgr(GEN,ENV,NG,MG,CICLO))
#'
#' #Definir o requerimento hídrico por estádio
#' with(genot, isgr(GEN,ENV,NG,MG,CICLO,req=5,stage="rep"))
#' }

isgr <- function(GEN, ENV, NG, MG, CICLO,req=3.5, stage=NULL) {
  require(dplyr)

  GEN <- as.factor(GEN)
  ENV <- as.factor(ENV)
  NG <- as.numeric(NG)
  MG <- as.numeric(MG)
  CICLO <- as.numeric(CICLO)
  dados <- data.frame(GEN, ENV, NG, MG,CICLO)

  # Verificação inicial dos dados
##############################################################################
  if(is.null(stage)){
    req <- req
  } else if(stage=="veg"){
    req <- req
  } else if(stage=="rep"){
    req <- req
  }
  desvng <- sd(dados$NG, na.rm = TRUE)
  desvmg <- sd(dados$MG, na.rm = TRUE)

  dados <- dados %>%
    mutate(tipo = ifelse(grepl("^L", as.character(GEN)), "Lin", "Test"))

  control <- dados %>%
    filter(tipo == "Test")

  if (nrow(control) > 0) {
    NGT <- mean(control$NG, na.rm = TRUE)
    MGT <- mean(control$MG, na.rm = TRUE)
  } else {
    NGT <- NA
    MGT <- NA
    stop("Nenhuma testemunha encontrada!")
  }

  SNG <- desvng
  SMG <- desvmg

  if(exists("DPclim")){
    DPclim <- DPclim
  } else {
    stop("É necessário obter os desvios padrões a partir da função desv_clim!")
  }

  num_lins <- sum(dados$tipo == "Lin")
  PRECI <- dados %>% group_by(ENV) %>%
    summarise(PREC=CICLO[1]*req)
  prep1 <- dados %>%
    filter(tipo == "Lin") %>%
    mutate(
      NGT = rep(NGT, n()),
      SNG = rep(SNG, n()),
      MGT = rep(MGT, n()),
      SMG = rep(SMG, n())
    ) %>%
    mutate(ENV = as.character(ENV)) %>%
    as_tibble() %>%
    left_join(PRECI, by = "ENV") %>%
    select(
      GEN, ENV, NG, NGL = NG, MGL = MG, NGT, SNG, MGT, SMG, PREC
    ) %>%
    left_join(DPclim %>% mutate(ENV = as.character(ENV)), by = "ENV") %>%
    select(
      GEN, ENV, NGL, MGL, NGT, SNG, MGT, SMG, PREC, STMED, TMEDR, SPREC, PRECIR
    )

  prep1 <- prep1 %>%
    mutate(
      ISGR = ((NGT - NGL) / SNG) *
        ((MGT - MGL) / SMG) *
        ((PREC - PRECIR) / SPREC) *
        ((25 - TMEDR) / STMED)
    )

  final <- data.frame(prep1$GEN, prep1$ENV, prep1$ISGR)
  colnames(final) <- c("Gen", "Env", "ISGR")
  final <- final[order(final$ISGR),]
  assign("isgr_index", final, envir = .GlobalEnv)
  return(final)
}
