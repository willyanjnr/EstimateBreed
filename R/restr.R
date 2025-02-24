#'Restriction of witness variability
#'@description
#'Method for restricting the variability of witnesses proposed by Carvalho et al.
#'(2023). It uses the restriction of the mean plus or minus one standard deviation.
#'standard deviation, which restricts variation by removing asymmetric values.
#'@param TEST The column with the name of the witness
#'@param REP The column with the replications
  #'@param Xi The column with the observed value for a given genotype.
#'@param scenario Scenario to be used for the calculation. Use “original” to
#'do not restrict the witnesses by the mean plus or minus the standard deviations,
#' or “restr” to apply the restriction.
#'@param zstat Logical argument. Applies Z-notation normalization if “TRUE”.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'https://doi.org/10.4025/actasciagron.v45i1.56156
#'@export
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'TEST <- rep(paste("T", 1:5, sep=""), each=3)
#'REP <- rep(1:3, times=5)
#'Xi <- rnorm(15, mean=10, sd=2)
#'
#'data <- data.frame(TEST,REP,Xi)
#'
#'#Apply the witness variability constraint
#'with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = FALSE))
#'print(Control)
#'
#'#Apply witness variability restriction with normalization (Z statistic)
#'with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = T))
#'print(Control)
#'}

restr <- function(TEST, REP, Xi, scenario = NULL, zstat = NULL){

  if (is.null(scenario)){
    stop("Please inform if the restriction will be applied!")
  }
  if (scenario == "restr"){
    media <- mean(Xi)
    desvio <- sd(Xi)
    lim_1s <- c(media - desvio, media + desvio)
    dentro_limite <- Xi >= lim_1s[1] & Xi <= lim_1s[2]
    ream <- data.frame(TEST = TEST[dentro_limite], REP = REP[dentro_limite],
                       Xi = Xi[dentro_limite])
    removidos <- TEST[!dentro_limite]
    rep_removidos <- REP[!dentro_limite]
    gen_rep_removidos <- paste(removidos, rep_removidos, sep = "R")
    n_desvio <- sd(ream$Xi)
    n_media <- mean(ream$Xi)
    cat("Removed Controls\n")
    print(gen_rep_removidos)
  }

  if (is.null(zstat)){
    stop("Please inform if standardization will be applied!")
  }
  if (zstat == FALSE){
    if (scenario == "restr"){
      colnames(ream) <- c("GEN","REP","Xi")
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
      colnames(reamost) <- c("GEN","REP","Xi","znorm")
      assign("Control", reamost, envir = .GlobalEnv)
    } else if (scenario == "original"){
      orig <- data.frame(TEST, REP, Xi)
      media <- mean(orig$Xi)
      desvio <- sd(orig$Xi)
      reamost_orig <- orig %>%
        mutate(znorm = (Xi - media) / desvio)
      colnames(reamos_orig) <- c("GEN","REP","Xi","znorm")
      assign("Control", reamost_orig, envir = .GlobalEnv)
    }
  }
}

#'Estimation of variance components by restricting the variability of the
#'witnesses.
#'@description
#'Estimation of variance components and genetic parameters from the restriction
#'of witness values
#'@param GEN The column with the name of the genotypes (without controls).
#'@param REP The column with the repetitions (if any).
#'@param Xi The column with the observed value for the variable in a given genotype.
#'@param approach Method to be used for estimating variance components. Use “apI”
#' for parent-offspring regression, “apII” for the of the sum of squares of
#' augmented blocks with intercalary parents, “apIII” for the method with linear
#'  mixed models with random genetic effects.
#'@param zstat Logical argument. Applies Z-notation normalization if “TRUE”.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'https://doi.org/10.4025/actasciagron.v45i1.56156
#'@export

cvar <- function(GEN,REP,Xi,approach=NULL,zstat=NULL){

  if(is.null(approach)){
    stop("Please provide a method for estimating variance components!")
  }
  if(is.null(zstat)){
    stop("Please inform if standardization will be applied!")
  }
  if(exists("Control")){
    control <- Control
    genot <- data.frame(GEN,REP,Xi)
    if(zstat==F){
      datag <- rbind(genot,control)
    } else if(zstat==T){
      media <- mean(genot$Xi)
      desvio <- sd(genot$Xi)
      datag <- genot %>%
        mutate(znorm=(Xi-media)/desvio)
      datag <- rbind(genot,control)
    }
  } else {
    stop("You have to carry out the operations of the restr function for the
         witnesses")
  }

  #apIII Modelo Linear Misto
  if(approach=="apIII"){
    mod1 <- lmm(Xi~+1|GEN,method=c("reml"),data=datag)
    assign("mod1",mod1,envir = .GlobalEnv)

    V_GEN <- mod1$Var$Xi["V(GEN)", "Est"]
    V_e <- mod1$Var$Xi["V(e)", "Est"]
    h2 <- V_GEN / (V_GEN + V_e)
    Ac <- sqrt(h2)
    param <- data.frame(V_GEN,V_e,h2,Ac)
    print(param)
  }
}
