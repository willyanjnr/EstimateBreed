#'Restrição da variabilidade das testemunhas
#'@description
#'Método para restrição da variabilidade das testemunhas proposta por
#'Carvalho et al. (2023). Utiliza a restriçõa da média mais ou menos um desvio
#'padrão, o que restringe a variação a partir da remoção de valores assimétricos.
#'@param TEST A coluna com o nome da testemunha
#'@param REP A coluna com as repetições
#'@param Xi A coluna com o valor observado para determinado genótipo.
#'@param scenario Cenário a ser utilizado para o cálculo. Usar "original" para
#'não restringir as testemunhas pela média mais ou menos os desvios padrões, ou
#'"restr" para aplicar a restrição.
#'@param zstat Argumento lógico. Aplica a normalização pela notação Z se "TRUE".
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'https://doi.org/10.4025/actasciagron.v45i1.56156
#'@export

#Usar o assign para criar um objeto, para o usuário imprimir apenas os resultados desejados
restr <- function(TEST, REP, Xi, scenario = NULL, zstat = NULL){
  require(dplyr)
  require(ggplot2)

  if (is.null(scenario)){
    stop("Por favor, informe se a restrição será aplicada!")
  }
  if (scenario == "restr"){
    media <- mean(Xi)
    desvio <- sd(Xi)
    lim_1s <- c(media - desvio, media + desvio)
    dentro_limite <- Xi >= lim_1s[1] & Xi <= lim_1s[2]
    ream <- data.frame(TEST = TEST[dentro_limite], REP = REP[dentro_limite], Xi = Xi[dentro_limite])
    removidos <- TEST[!dentro_limite]
    rep_removidos <- REP[!dentro_limite]
    gen_rep_removidos <- paste(removidos, rep_removidos, sep = "R")
    n_desvio <- sd(ream$Xi)
    n_media <- mean(ream$Xi)
    print(gen_rep_removidos)
  }

  if (is.null(zstat)){
    stop("Por favor, informe se a normalização será aplicada!")
  }
  if (zstat == FALSE){
    if (scenario == "restr"){
      assign("Control",ream,envir = .GlobalEnv)
    } else if (scenario == "original"){
    datatest <- data.frame(TEST,REP,Xi)
    colnames(datatest) <- c("GEN","REP","Xi")
    assign("Control",datatest,envir = .GlobalEnv)
    }
  }
  if (zstat == TRUE){
    if (scenario == "restr"){
      reamost <- ream %>%
        mutate(znorm = (Xi - n_media) / n_desvio)
      assign("Control", reamost, envir = .GlobalEnv)
    } else if (scenario == "original"){
      orig <- data.frame(TEST, REP, Xi)
      media <- mean(orig$Xi)
      desvio <- sd(orig$Xi)
      reamost_orig <- orig %>%
        mutate(znorm = (Xi - media) / desvio)
      assign("Control", reamost_orig, envir = .GlobalEnv)
    }
  }
}

#'Estimativa dos componentes de variância pela restrição da variabilidade das
#'testemunhas.
#'@description
#'Método para restrição da variabilidade das testemunhas proposta por
#'Carvalho et al. (2023). Utiliza a restriçõa da média mais ou menos um desvio
#'padrão, o que restringe a variação a partir da remoção de valores assimétricos.
#'@param GEN A coluna com o nome dos genótipos (sem testemunhas).
#'@param REP A coluna com as repetições (se houver).
#'@param Xi A coluna com o valor observado para a variável em determinado genótipo.
#'@param approach Método a ser utilizado para estimativa dos componentes de
#'variância. Usar "apI" para regressão genitor progênie, "apII" para o método
#'da soma de quadrados de blocos aumentados com testemunhas intercalares, "apIII"
#'para o método com modelos lineares mistos com efeitos genéticos aleatórios.
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'https://doi.org/10.4025/actasciagron.v45i1.56156
#'@export

cvar <- function(GEN,REP,Xi,approach=NULL,zstat=NULL){
  require(lme4)

  if(is.null(approach)){
    stop("Por favor, informe um método de estimativa dos componentes de variância")
  }
  if(exists("Control")){
    control <- Control
    genot <- data.frame(GEN,REP,Xi)
    datag <- rbind(genot,control)
    print(datag)
  } else {
    stop("É preciso realizar as operações da função restr para as testemunhas")
  }
  #apI Modelo Linear
  if(approach=="apI"){
  mod1 <- lm(Xi~GEN,data=datag)
  summary(mod1)
  }
  #apII Soma de quadrados RCBD

  #apIII MLM
}
