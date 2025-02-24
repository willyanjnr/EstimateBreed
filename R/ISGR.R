#'Auxiliary function for calculating ISGR
#'@description
#'This function receives a dataframe with temperature and precipitation data
#'and calculates the standard deviation of these parameters for each environment.
#'@param ENV Identification of each selection environment (to differentiate if
#'there is more than one cultivation cycle).
#'@param TMED Average air temperature (in ºC) during the cycle in each environment.
#'@param PREC Rainfall (in mm) during the cultivation cycle in each environment
#'@return A dataframe containing the identifier of the selection environment and
#'the standard deviations for temperature and precipitation.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'data("desvamb")
#'head(desvamb)
#'
#'with(desvamb,desv_clim(ENV,TMED,PREC))
#'}

desv_clim <- function(ENV,TMED,PREC) {

  ENV <- ENV
  TMED <- TMED
  PREC <- PREC
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
#'ISGR - Genetic Selection Index for Resilience
#'@description
#'Estimation of the selection index for environmental resilience
#'(Bandeira et al., 2024).
#'@param GEN Column referring to genotypes. Lines must have the prefix “L” before
#' the number. Ex: L139.
#'@param ENV The column for the selection environment.
#'@param NG Number of grains of all genotypes evaluated
#'@param MG Grain mass of all genotypes evaluated
#'@param CICLO Number of days in the cycle to define rainfall
#'ideal (value of 3.5 mm per day). Can be changed manually in the 'req' argument.
#'@param req Average daily water demand for the soybean crop (standard 3.5 mm).
#'May change depending on the phenological stage.
#'@param stage Parameter to define the phenological stage the crop is in
#'Use “veg” for vegetative and “rep” for reproductive, if the
#'evaluations have only been carried out in a given period.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Bandeira, W. J. A., Carvalho, I. R., Loro, M. V., da Silva, J. A. G.,
#'Dalla Roza, J. P., Scarton, V. D. B., Bruinsma, G. M. W., & Pradebon, L. C. (2024).
#'Identifying soybean progenies with high grain productivity and stress resilience
#'to abiotic stresses. Aust J Crop Sci, 18(12), 825-830.
#'https://doi.org/10.21475/ajcs.24.18.12.p98
#'@export
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'#Obtain environmental deviations
#'data("desvamb")
#'head(desvamb)
#'with(desvamb, desv_clim(ENV,TMED,PREC))
#'
#'#Calculate the ISGR
#'data("genot")
#'head(genot)
#'with(genot, isgr(GEN,ENV,NG,MG,CICLO))
#'
#'#Define the water requirement per stage
#'with(genot, isgr(GEN,ENV,NG,MG,CICLO,req=5,stage="rep"))
#'}

isgr <- function(GEN, ENV, NG, MG, CICLO,req=3.5, stage=NULL) {

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
    stop("No witnesses found!")
  }

  SNG <- desvng
  SMG <- desvmg

  if(exists("DPclim")){
    DPclim <- DPclim
  } else {
    stop("The standard deviations must be obtained from the desv_clim!")
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
