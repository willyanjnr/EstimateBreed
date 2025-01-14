#'Restrição da variabilidade das testemunhas
#'@description
#'Método para restrição da variabilidade das testemunhas proposta por
#'Carvalho et al. (2023). Utiliza a restriçõa da média mais ou menos um desvio
#'padrão, o que restringe a variação a partir da remoção de valores assimétricos.
#'@param GEN A coluna com o nome do genótipo
#'@param REP A coluna com as repetições
#'@param Xi A coluna com o valor observado para determinado genótipo.
#'@param scenario Cenário a ser utilizado para o cálculo. Usar "original" para
#'não restringir as testemunhas pela média mais ou menos os desvios padrões, ou
#'"restr" para aplicar a restrição.
#'@param approach Método a ser utilizado para estimativa dos componentes de
#'variância. Usar "apI" para regressão genitor progênie, "apII" para o método
#'da soma de quadrados de blocos aumentados com testemunhas intercalares, "apIII"
#'para o método com modelos lineares mistos com efeitos genéticos aleatórios.
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

restr <- function(GEN, REP, Xi, scenario = NULL, zstat=NULL, approach="apI"){
  require(dplyr)
  require(ggplot2)
  require(lme4)
  if (is.null(scenario)){
    stop("Por favor, informe se a restrição será aplicada")
  }
  if(scenario=="restr"){
    media <- mean(Xi)
    desvio <- sd(Xi)

    lim_1s <- c(media - desvio, media + desvio)

    dentro_limite <- Xi >= lim_1s[1] & Xi <= lim_1s[2]
    ream <- data.frame(GEN = GEN[dentro_limite], REP = REP[dentro_limite], Xi = Xi[dentro_limite])
    removidos <- GEN[!dentro_limite]
    rep_removidos <- REP[!dentro_limite]
    gen_rep_removidos <- paste(removidos, rep_removidos, sep = "R")
    n_desvio <- sd(ream$Xi)
    percent <- 100-((n_desvio*100)/desvio)

    cat("Genótipos e repetições removidos: \n")
    print(gen_rep_removidos)
    cat("-------------------------------------\n")
    cat("Observações reamostradas: \n")
    print(ream)
    cat("-------------------------------------\n")
    cat("Desvio SR:",paste(round(desvio,digits = 4)),"\n")
    cat("Desvio CR:",paste(round(n_desvio,digits = 4)),"\n")
    cat("Percent Red:",paste(round(percent,digits = 2)),"\n")
  } else if(scenario=="original"){
    media <- mean(Xi)
    desvio <- sd(Xi)
  }
  if(scenario=="restr" && zstat=="T"){

  }
}
