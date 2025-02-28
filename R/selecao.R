#' General Selection Gain Function
#' @description
#' Computes selection gain using different selection methods
#' @param Var The column with the name of the variables of interest
#' @param h The column with the restricted heritability values
#' @param VF The column with the phenotypic variance values (optional)
#' @param P The column with the progeny values or selection pressure (optional)
#' @param DS The column with the selection differential values (optional)
#' @param Year The column with the year of selection (optional)
#' @param method The selection method: 'pressure', 'differential',
#' 'genitor_control", or 'year_weighted'
#' @return A data frame with selection gain results
#' @author Willyan Junior Adorian Bandeira
#' @author Ivan Ricardo Carvalo
#' @author Murilo Vieira Loro
#' @author Leonardo Cesar Pradebon
#' @author Jose Antonio Gonzalez da Silva
#' @export
#' @examples
#'\donttest{
#'library(EstimateBreed)
#'
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", method = "pressure")
#'SG(Var = c("A", "B", "C"), h = 0.5, DS = 1.5, method = "differential")
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", method = "genitor_control")
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", Year = 5, method = "year_weighted")
#'}

SG <- function(Var, h, VF = NULL, P = "1", DS = NULL, Year = NULL, method = "pressure") {
  Var <- as.factor(Var)

  coeficientes <- c("1" = 2.7, "2" = 2.44, "3" = 2.27, "4" = 2.16, "5" = 2.08,
                    "10" = 1.76, "20" = 1.4, "40" = 0.97, "50" = 0.8, "60" = 0.64,
                    "70" = 0.5, "80" = 0.35, "90" = 0.2)

  if (!is.null(P) && !(P %in% names(coeficientes))) {
    stop("Invalid P value. Choose between: ", paste(names(coeficientes),
                                                    collapse = ", "))
  }

  GS <- switch(method,
               "pressure" = {
                 if (is.null(VF)) stop("VF is required for the 'pressure'
                                       method.")
                 coef <- coeficientes[P]
                 GS <- h * coef * sqrt(VF)
                 desc <- paste("Selection Gain with", P, "% pressure")
                 list(GS = GS, desc = desc, coef = coef)
               },
               "differential" = {
                 if (is.null(DS)) stop("DS is required for the 'differential'
                                       method.")
                 GS <- h * DS
                 desc <- "Selection Gain using Selection Differential"
                 list(GS = GS, desc = desc, coef = NA)
               },
               "genitor_control" = {
                 if (is.null(VF)) stop("VF is required for the 'genitor_control'
                                       method.")
                 coef <- coeficientes[P]
                 GS <- h * coef * 0.5 * sqrt(VF)
                 desc <- paste("Selection Gain with", P, "% pressure and Genitor
                               Control")
                 list(GS = GS, desc = desc, coef = coef)
               },
               "year_weighted" = {
                 if (is.null(VF) || is.null(Year)) stop("VF and Year are
                                                        required for the
                                                        'year_weighted' method.")
                 coef <- coeficientes[P]
                 GS <- (h * coef * sqrt(VF)) / Year
                 desc <- paste("Selection Gain with", P, "% pressure weighted
                               by Year")
                 list(GS = GS, desc = desc, coef = coef)
               },
               stop("Invalid method. Choose between 'pressure', 'differential',
                    'genitor_control' or 'year_weighted'.")
  )

  final_list <- list(
    Variable = Var,
    SG = GS$GS,
    H2 = h,
    PV = if (!is.null(VF)) VF else NULL,
    SP = if (method %in% c("pressure", "genitor_control", "year_weighted"))
      P else NULL,
    SD = if (method == "differential") DS else NULL,
    Year = if (method == "year_weighted") Year else NULL
  )

  final_list <- final_list[!sapply(final_list, is.null)]
  final <- suppressWarnings(data.frame(final_list, stringsAsFactors = FALSE,
                                       row.names = NULL))

  cat("\n", strrep("-", 40), "\n", sep = "")
  cat(paste("Selection Gain using", method, "\n"))
  cat(strrep("-", 40), "\n", sep = "")
  return(final)
}

####
#'Selection by Selection Differential (Mean and Deviations)
#'@description
#'Selection of Transgressive Genotypes - Selection Differential (SD)
#'@param Gen The column with the genotype name
#'@param Var The column with the variable of interest
#'@param Control The column with the value of the variable 'X' for the witnesses
#'@param ylab The name of the Y axis.
#'@param xlab The name of the X axis.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

transgressivos <- function(Gen, Var, Control,ylab="Selection",xlab="Genotypes"){

  Gen <- as.factor(Gen)
  Var <- Var
  Testemunha <- Control

  Media<-mean(Var)
  DSg<-mean(Testemunha)
  Desvio<-sd(Var)
  DS1S<-Media+Desvio
  DS2S<-Media+(2*Desvio)
  DS3S<-Media+(3*Desvio)

  parametros <- list(Media=Media,DSg=DSg,Desvio=Desvio,DS1S=DS1S,DS2S=DS2S,DS3S=DS3S)
  dados <- data.frame(Gen,Var, Testemunha)

  grafico <- ggplot(dados, aes(x = Gen, y = Var))+
    geom_text(
      label=rownames(dados),
      nudge_x =0, nudge_y = 0, color = "red",hjust =3,
      size = 10,
      check_overlap = F)+ylab(ylab)+xlab(xlab)+theme_classic()+

    geom_segment(aes(x = 0, y =Media, xend =Gen, yend = Media), linetype = 1,
                 color = "darkred")+
    geom_label(aes(x=0.5, y=Media, label="Mean"))+

    geom_segment(aes(x = 0, y =DSg, xend =Gen, yend =DSg), linetype = 2,
                 color = "darkgray")+
    geom_label(aes(x=0.5, y=DSg, label="DS T"))+

    geom_segment(aes(x = 0, y =DS1S, xend =Gen, yend =DS1S), linetype = 3,
                 color = "blue")+
    geom_label(aes(x=0.5, y=DS1S, label="DS1S"))+

    geom_segment(aes(x = 0, y =DS2S, xend =Gen, yend =DS2S), linetype = 4,
                 color = "darkgreen")+
    geom_label(aes(x=0.5, y=DS2S, label="DS2S"))+

    geom_segment(aes(x = 0, y =DS3S, xend =Gen, yend =DS3S), linetype = 5,
                 color = "darkorange")+
    geom_label(aes(x=0.5, y=DS3S, label="DS3S"))+
    ggtitle("Selection of Transgressive Genotypes - Selection Differential (SD)")

  print(grafico)

  cat("\n-----------------------------------------------------------------\n")
  cat("Selection of Transgressive Genotypes - Selection Differential (SD)")
  cat("\n-----------------------------------------------------------------\n")
  cat("Parameters")
  cat("\n-----------------------------------------------------------------\n")
  print(parametros)
  suppressWarnings()
}

###
#'Standard Segregation
#'@description
#'Didactic table of standard segregation by generation
#'@param MELHORAMENTO Base parameter for the print table function
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

default_seg <- function(MELHORAMENTO){

  AUTOGAM<-c("Genitores","F1","F2","F3","F4","F5","F6","F7","F8","F9")
  HETEROZIGOSITY<-c(0,100,50,25,12.5,6.25,3.12,1.56,0.78,0.39)
  ALOGAM<-c("-","-","S0","S1","S2","S3","S4","S5","S6","S7")
  HOMOZIGOSITY<-c(100, 0,50,75,87.5,93.75,96.88,98.44,99.22, 99.61)
  MUTANTS<-c("-", "M0", "M1","M2", "M3","M4","M5","M6", "M7", "M8")
  SELECTION<-c("N","N","Quali","Quali",
             "Quant","Quanti","Quanti","Quanti",
             "Quanti","Quanti")
  N_GENES<-c("-","-","1 a 2",
             "1 a 2", "3 ou +", "3 ou +","3 ou +","3 ou +","3 ou +","3 ou +")
  ENV_EF<-c("-","-","low",
                "low", "high", "high","high","high","high","high")
  COEF_F<-c(0.000,0.500,0.750,0.875,0.937,0.969,0.984,0.992,0.996,0.998)
  TABELA<-data.frame(AUTOGAM,ALOGAM,HETEROZIGOSITY,HOMOZIGOSITY,MUTANTS,
                     SELECTION,N_GENES, ENV_EF, COEF_F)
  return(TABELA)
}

###
#'Inbreeding coefficient
#'@description
#'Function for calculating the inbreeding coefficient
#'@param var Column with the variable name
#'@param VG Column with genotypic variance
#'@param VF Column with phenotypic variance
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

Coeficiente_endogamia<-function(var, VG, VF){

  var <- as.factor(var)
  VG <- VG
  VF <- VF
  ################################
  VA_total_F3<-VG/1.5
  VD_total_F3<-VG/0.75
  VA_Entre_F3<-VG/1
  VD_Entre_F3<-VG/0.25
  VA_Dentro_F3<-VG/0.5
  VD_Dentro_F3<-VG/0.5
  ###############################
  VA_total_F4<-VG/0.875
  VD_total_F4<-VG/0.438
  VA_Entre_F4<-VG/1.5
  VD_Entre_F4<-VG/0.188
  VA_Dentro_F4<-VG/0.250
  VD_Dentro_F4<-VG/0.250
  ###############################
  VA_total_F5<-VG/1.875
  VD_total_F5<-VG/0.234
  VA_Entre_F5<-VG/1.750
  VD_Entre_F5<-VG/0.109
  VA_Dentro_F5<-VG/0.125
  VD_Dentro_F5<-VG/0.125
  ###############################
  VA_total_F6<-VG/1.938
  VD_total_F6<-VG/0.121
  VA_Entre_F6<-VG/1.875
  VD_Entre_F6<-VG/0.059
  VA_Dentro_F6<-VG/0.063
  VD_Dentro_F6<-VG/0.063
  ###############################
  F3<-data.frame(var,
                 VA_total_F3,
                 VD_total_F3,
                 VA_Entre_F3,
                 VD_Entre_F3,
                 VA_Dentro_F3,
                 VD_Dentro_F3)

  F4<-data.frame(var,
                 VA_total_F4,
                 VD_total_F4,
                 VA_Entre_F4,
                 VD_Entre_F4,
                 VA_Dentro_F4,
                 VD_Dentro_F4)

  F5<-data.frame(var,
                 VA_total_F5,
                 VD_total_F5,
                 VA_Entre_F5,
                 VD_Entre_F5,
                 VA_Dentro_F5,
                 VD_Dentro_F5)
  F6<-data.frame(var,
                 VA_total_F6,
                 VD_total_F6,
                 VA_Entre_F6,
                 VD_Entre_F6,
                 VA_Dentro_F6,
                 VD_Dentro_F6)
  #################################################################
  hVaTF3<-(VA_total_F3<-VG/1.5)/VF
  hVdTF3<-(VD_total_F3<-VG/0.75)/VF
  hVaEF3<-(VA_Entre_F3<-VG/1)/VF
  hVdEF3<-(VD_Entre_F3<-VG/0.25)/VF
  hVaDF3<-(VA_Dentro_F3<-VG/0.5)/VF
  hVdDF3<-(VD_Dentro_F3<-VG/0.5)/VF

  hF3<-data.frame(var,
                  hVaTF3,
                  hVdTF3,
                  hVaEF3,
                  hVdEF3,
                  hVaDF3,
                  hVdDF3)
  #################################################################
  hVaTF4<-(VA_total_F4<-VG/0.875)/VF
  hVdTF4<-(VD_total_F4<-VG/0.438)/VF
  hVaEF4<-(VA_Entre_F4<-VG/1.5)/VF
  hVdEF4<-(VD_Entre_F4<-VG/0.188)/VF
  hVaDF4<-(VA_Dentro_F4<-VG/0.250)/VF
  hVdDF4<-(VD_Dentro_F4<-VG/0.250)/VF

  hF4<-data.frame(var,
                  hVaTF4,
                  hVdTF4,
                  hVaEF4,
                  hVdEF4,
                  hVaDF4,
                  hVdDF4)
  #################################################################
  hVaTF5<-(VA_total_F5<-VG/1.875)/VF
  hVdTF5<-(VD_total_F5<-VG/0.234)/VF
  hVaEF5<-(VA_Entre_F5<-VG/1.750)/VF
  hVdEF5<-(VD_Entre_F5<-VG/0.109)/VF
  hVaDF5<-(VA_Dentro_F5<-VG/0.125)/VF
  hVdDF5<-(VD_Dentro_F5<-VG/0.125)/VF

  hF5<-data.frame(var,
                  hVaTF5,
                  hVdTF5,
                  hVaEF5,
                  hVdEF5,
                  hVaDF5,
                  hVdDF5)

  #################################################################
  hVaTF6<-(VA_total_F6<-VG/1.938)/VF
  hVdTF6<-(VD_total_F6<-VG/0.121)/VF
  hVaEF6<-(VA_Entre_F6<-VG/1.875)/VF
  hVdEF6<-(VD_Entre_F6<-VG/0.059)/VF
  hVaDF6<-(VA_Dentro_F6<-VG/0.063)/VF
  hVdDF6<-(VD_Dentro_F6<-VG/0.063)/VF

  hF6<-data.frame(var,
                  hVaTF6,
                  hVdTF6,
                  hVaEF6,
                  hVdEF6,
                  hVaDF6,
                  hVdDF6)

  final <- bind_rows(F3,F4,F5,F6,hF3,hF4,hF5,hF6)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F3 - Corrected Variances")
  cat("\n---------------------------------------------------------------------------------\n")
  print(F3)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F3- Heritability in the narrow sense")
  cat("\n---------------------------------------------------------------------------------\n")
  print(hF3)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F4 - Corrected Variances")
  cat("\n---------------------------------------------------------------------------------\n")
  print(F4)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F4- Heritability in the narrow sense")
  cat("\n---------------------------------------------------------------------------------\n")
  print(hF4)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F5 - Corrected Variances")
  cat("\n---------------------------------------------------------------------------------\n")
  print(F5)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F5- Heritability in the narrow sense")
  cat("\n---------------------------------------------------------------------------------\n")
  print(hF5)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F6 - Corrected Variances")
  cat("\n---------------------------------------------------------------------------------\n")
  print(F6)

  cat("\n---------------------------------------------------------------------------------\n")
  cat("F6- Heritability in the narrow sense")
  cat("\n---------------------------------------------------------------------------------\n")
  print(hF6)

}

###
#'Regression Genitor Progeny
#'@description
#'Estimation of Genitor x Progeny Regression
#'@param ind description
#'@param Genitor description
#'@param Progenie description
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

reg_GP <- function(ind, Genitor, Progenie) {

  ind <- as.factor(ind)
  Genitor <- Genitor
  Progenie <- Progenie

  model <- lm(Progenie ~ Genitor)

  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  Progenie_umPai<-0.5*slope
  Progenie_MPais<-slope
  Meio_Irmao<-slope*0.25
  Irmao_Completo<-slope*0.50

  equation <- paste("y =", round(intercept, 2), "+", round(slope, 2), "* x")

  dados<-data.frame(Genitor, Progenie)

  grafico<-ggplot(dados, aes(x=Genitor, y=Progenie)) +
    geom_line()+
    geom_point()+geom_smooth(method = "lm", se = T, color = "blue")+
    theme_minimal()

  print(grafico)

  result<-list(
    equation = equation,
    coefficients = coef(model),
    Progenie_umPai=Progenie_umPai,
    Progenie_MPais=Progenie_MPais,
    Meio_Irmao=Meio_Irmao,
    Irmao_Completo=Irmao_Completo,
    grafico=grafico)

  cat("\n-----------------------------------------------------------------\n")
  cat("Parameters")
  cat("\n-----------------------------------------------------------------\n")
  print(result)
}

###
#'Allelic interactions
#'@description
#'Examples of allelic and gene interactions
#'@param type Type of allelic interaction. Use 'ad' for additivity, 'dom'
#'for complete dominance, 'domp' for partial dominance and 'sob' for
#'overdominance.
#'@param ge Type of GxE interaction. Use 'aus' for no interaction,
#''simple' for simple interaction and 'complex' for complex interaction.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

ALELIC <- function(type=NULL,ge=NULL){

  if(!is.null(type)){
  if(type=="ad"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 70, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Additive Genetic Interaction",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="dom"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 120, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Genetic Interaction of Dominance",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="domp"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 90, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Partial Dominance Genetic Interaction",
           x = "Genotpe", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="sob"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 160, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
      labs(title = "Genetic Interaction of Overdominance",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  }
  }
  if(!is.null(ge)){
    if(ge=="aus"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 70)
      efeito_ambiente_2 <- c(95, 105)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "No Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
    } else if(ge=="simple"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 80)
      efeito_ambiente_2 <- c(85, 125)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Simple Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
    } else if(ge=="complex"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 100)
      efeito_ambiente_2 <- c(110, 60)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Complex Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
    }
  }
}

#'Additive Genetic Gain
#'@description
#'Estimates the additive genetic gain, as described by Falconer (1987).
#'@param GEN Vector or dataframe containing the genotypes to be selected.
#'@param VAR Variable of interest for analysis.
#'@param h2 Heritability of the character (a value between 0 and 1).
#'@param P Performance of the progeny (a numerical value or vector).
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

gga <- function(GEN, VAR, h2, P) {

  data <- data.frame(GEN,VAR,h2,P)
  data <- data %>%
    group_by(VAR) %>%
    mutate(u=(mean(P))+(mean(P)*0.1))
  ganho <- data %>%
    group_by(GEN) %>%
    mutate(AGV=h2*(P-u))
  return(ganho)
}

#'General parameters for selection
#'@description
#'Function for determining selection parameters, based on an experiment
#'carried out on the rice crop. Intended for isolated evaluation of the performance
#'of strains within a given population.
#'@param POP The column with the population under improvement.
#'@param GEN The column with the selected genotypes within the population.
#'@param REP The column with the repetitions (if any).
#'@param vars The column with the variable of interest.
#'@param K Selection pressure (Default 0.05).
#'@param type Type of experiment (balanced or unbalanced). Use
#''balanced' for balanced and 'unb' for unbalanced.
#'@param check Logical argument. Checks the model's assumptions
#'statistical if the value is equal to TRUE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Yadav, S. P. S., Bhandari, S., Ghimire, N. P., Mehata, D. K., Majhi, S. K.,
#'Bhattarai, S., Shrestha, S., Yadav, B., Chaudhary, P., & Bhujel, S. (2024).
#'Genetic variability, character association, path coefficient, and diversity
#'analysis of rice (Oryza sativa L.) genotypes based on agro-morphological
#'traits. International Journal of Agronomy, 2024, Article ID 9946332.
#'https://doi.org/10.1155/2024/9946332
#'@export

genpar <- function(POP, GEN, REP = NULL, vars, K = 0.05, type = "balanced",
                   check = FALSE) {

  if (is.null(REP)) {
    stop("Please infomr the replications", call. = FALSE)
  }

  varc <- data.frame(POP, GEN, REP)
  varc$POP <- as.factor(varc$POP)
  varc$GEN <- as.factor(varc$GEN)
  varc$REP <- as.factor(varc$REP)

  for (var_name in vars) {
    if (!(var_name %in% colnames(dados))) {
      stop(paste("The variable", var_name, "does not exist in the dataset."),
           call. = FALSE)
    }

    varc <- varc %>% mutate(!!var_name := dados[[var_name]])
    X <- tapply(varc[[var_name]], POP, mean)

    if (type == "balanced") {
      repet <- length(unique(varc$REP))
      model <- aov(varc[[var_name]] ~ GEN + REP, data = varc)
      Yearva_table <- summary(model)[[1]]
      MSg <- Yearva_table["GEN", "Mean Sq"]
      MSe <- Yearva_table["Residual", "Mean Sq"]
      pvalue <- Yearva_table["GEN", "Pr(>F)"]

    } else if (type=="unb"){
      #Modelo Linear Misto ou YearVA tipo 3
    }

    if (pvalue >= 0.05) {
      print(paste("Results for the variable:", var_name))
      print(Yearva_table)
      warning("Effect of genotypes not significant for the variable ", var_name, "!", call. = FALSE)
    } else {
      result <- data.frame(
        Parameters = c("sigmaE", "sigmaG", "sigmaP", "ECV", "GCV", "PCV", "H2", "GA", "GAM"),
        Valor = c(sigmaE, sigmaG, sigmaP, ECV, GCV, PCV, H2, GA, GAM)
      )
      colnames(result)[which(colnames(result) == "Valor")] <- var_name
      print(paste("Results for the variable:", var_name))
      print(Yearva_table)
      print(result)
    }
  }
}

#'Effective Population Size
#'@description
#'Estimated effective population size adapted from Morais (1997).
#'@param GEN The column with the name of the genotype (progeny).
#'@param SI The column with the number of individuals selected
#'@param NE Number of individuals conducted during the selection period.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@export

tamef <- function(GEN,SI,NE){

  data <- data.frame(GEN,SI,NE)
  Sum_SI <- (sum(data$SI))^2
  data <- data %>%
    mutate(Gen_SI=SI^2)
  Pond_SI <- sum(data$Gen_SI/data$NE)
  Nej <- Sum_SI/Pond_SI
  return(Nej)
}

#'Jinks and Pooni method
#'@description
#'Function for estimating the probability of extracting superior lines from
#'populations by the Jinks and Pooni method
#'@param Pop The column with the population name
#'@param Var The column with the variable name
#'@param VG The column with the genotypic variance values
#'@param Test The column with the witnesses' names
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalo
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Port, E. D., Carvalho, I. R., Pradebon, L. C., Loro, M. V., Colet, C. D. F.,
#'Silva, J. A. G. D., & Sausen, N. H. (2024).
#'Early selection of resilient progenies to seed yield in soybean populations.
#'Ciencia Rural, 54, e20230287.
#'@export

Jinks_Pooni <- function(Pop, Var, VG, Test){
#Finalizar
  Population <- Pop
  Var <- Var
  VG <- VG
  Testemunhas <- Test

  Z<-((Testemunhas-Var)/(sqrt(VG)))
  P<-((1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE))*100)
  Gen_Value <- ifelse(P > 50, "High", "Low")

  Parameters <- data.frame(Population, Z, P, Gen_Value)

  cat("\n-----------------------------------------------------------------\n")
  cat("Probability of extracting superior strains from populations
                   - Jinks and Pooni method")
  cat("\n-----------------------------------------------------------------\n")
  cat("Parameters")
  cat("\n-----------------------------------------------------------------\n")
  print(Parameters)
  cat("\n-----------------------------------------------------------------\n")
}
