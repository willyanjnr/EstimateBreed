#'Leaf Area Index (LAI)
#'@description
#'Utility function for estimating crop LAI
#'@param GEN The column with the genotype name
#'@param W The column with the width of the leaf.
#'@param L The column with the length of the leaf.
#'@param crop Crop sampled. Use “soy” for soybean and “maize” for corn, “trit”
#'for wheat, “rice” for rice, “bean” for bean, “sunflower” for sunflower,
#'“cotton” for cotton, “sugarcane” for sugarcane, “potato” for potato and
#'“tomato” for tomato.
#'@param sp Row spacing (Standard sp=0.45).
#'@param sden Sowing density, in plants per linear meter (standard sden=14).
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Meira, D., Queiróz de Souza, V., Carvalho, I. R., Nardino, M., Follmann,
#'D. N., Meier, C., Brezolin, P., Ferrari, M., & Pelegrin, A. J. (2015).
#'Plastocrono e caracteres morfológicos da soja com hábito de crescimento
#'indeterminado. Revista Cultivando o Saber, 8(2), 184-200.
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'#Finalizar o exemplo
#'}
#'@export

iaf <- function(GEN, W, L, crop = "soy", sp = 0.45, sden = 14) {
  require(dplyr)

  k_correction <- c(
    soy = 0.7,
    maize = 0.75,
    trit = 0.88,
    rice = 0.85,
    bean = 0.72,
    sunflower = 0.80,
    cotton = 0.85,
    sugarcane = 0.75,
    potato = 0.90,
    tomato = 0.92
  )

  if(!(crop %in% names(k_correction))){
    stop("Unknown culture. Please enter a valid culture: \n",
         paste(names(k_correction),collapse = ", "))
  }
  k <- k_correction[crop]
  if (missing(W)) {
    stop("Please enter the width of the sheet", call. = FALSE)
  }
  if (missing(L)) {
    stop("Please enter the length of the sheet", call. = FALSE)
  }
  if (missing(GEN)) {
    stop("Please enter the genotype", call. = FALSE)
  }
  if(!is.numeric(W) || !is.numeric(L)){
    stop("The width and length of the sheet must be numerical",call. = F)
  }
    a1 <- data.frame(GEN, W, L)
    a1 <- a1 %>%
      mutate(AF = (W * L) * k)
    resultado <- a1 %>%
      group_by(GEN) %>%
      summarise(
        AFA = sum(AF)/10000,
        SD = (10000 / sp) * sden,
        AS = 10000 / ((10000 / sp) * sden),
        LAI = AFA / AS
      )
    resultado_f <- resultado %>%
      select(GEN,AFA,IAF)
    cat("Selected crop:",paste(crop),"\n")
    cat("Row Spacing used: ",paste(sp),"\n")
    cat("Sowing density used: ",paste(sden),"\n")
    return(resultado_f)
  }
