#'Estimativa do Índice de Área Foliar (IAF)
#'@description
#'Função utilitária para a determinação do IAF de culturas
#'@param GEN Colune com o nome do genótipo
#'@param W Largura da Folha.
#'@param L Comprimento da Folha.
#'@param crop Cultura amostrada. Utilizar "soy" para soja e "maize" para milho.
#'@param sp Espaçamento entre linhas (Padrão sp=0.45).
#'@param sden Densidade de semeadura, em plantas por metro linear (padrão sden=14).
#'@export

iaf <- function(GEN, W, L, crop = "soy", sp = 0.45, sden = 14) {
  require(dplyr)

  k_correction <- c(
    soy = 0.7,
    maize = 0.75,
    wheat = 0.88,
    rice = 0.85,
    bean = 0.72,
    sunflower = 0.80,
    cotton = 0.85,
    sugarcane = 0.75,
    potato = 0.90,
    tomato = 0.92
  )

  if(!(crop %in% names(k_correction))){
    stop("Cultura desconhecida. Por favor, informe uma cultura válida: \n",
         paste(names(k_correction),collapse = ", "))
  }
  k <- k_correction[crop]
  if (missing(W)) {
    stop("Por favor, informe a largura da folha", call. = FALSE)
  }
  if (missing(L)) {
    stop("Por favor, informe o comprimento da folha", call. = FALSE)
  }
  if (missing(GEN)) {
    stop("Por favor, informe o genótipo", call. = FALSE)
  }
  if(!is.numeric(W) || !is.numeric(L)){
    stop("A largura e o comprimento da folha precisam ser numéricos",call. = F)
  }
    a1 <- data.frame(GEN, W, L)
    a1 <- a1 %>%
      mutate(AF = (W * L) * k)
    resultado <- a1 %>%
      group_by(GEN) %>%
      summarise(
        AFA = sum(AF)/10000,
        DENS = (10000 / sp) * sden,
        AS = 10000 / ((10000 / sp) * sden),
        IAF = AFA / AS
      )
    resultado_f <- resultado %>%
      select(GEN,AFA,IAF)
    cat("Cultura selecionada:",paste(crop),"\n")
    cat("Espaçamento utilizado: ",paste(sp),"\n")
    cat("Densidade de semeadura utilizada: ",paste(sden),"\n")
    return(resultado_f)
  }
