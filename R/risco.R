#'Risk of Disease Occurrence in Soybeans
#'@description
#'Calculation of the Risk of Disease Occurrence in Soybeans as a Function of
#'Variables meteorological variables (Engers et al., 2024).
#'@param DAY The column for the day of the month.
#'@param MONTH The column for the month of the year (numeric value).
#'@param TEMP A coluna da temperatura média do ar (em ºC).
#'@param RH The relative humidity column (in \%).
#'@param disease Define the soybean disease (Standard = “rust”).
#'@param plot Plot a graph of the accumulation (Default is F (FALSE)).
#'@references
#'de Oliveira Engers, L.B., Radons, S.Z., Henck, A.U. et al.
#'Evaluation of a forecasting system to facilitate decision-making for the
#'chemical control of Asian soybean rust. Trop. plant pathol. 49, 539–546 (2024).
#'https://doi.org/10.1007/s40858-024-00649-1
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'# Rust Risk Prediction
#'data("clima")
#'with(clima, risco(DY, MO, TMED, RH, disease = "rust"))
#'}
#'@export

risco <- function(DAY,MONTH,TEMP,RH,disease="rust",plot=F){
  #DIA <- DIA
  #MES <- as.factor(MES)
  #TEMP <- TEMP
  #UR <- UR
  dados <- data.frame(DAY,MONTH,TEMP,RH)
  if(disease=="rust"){
  alfa <- log(2)/log(2.30508474576)
  umidade <- aggregate(RH~DAY+MONTH,data=dados,FUN=function(x)sum(x>85))
  colnames(umidade) <- c("Day","Month","W")
  umidade <- as.data.frame(umidade)
  umidade$RHrisk <- 1/1+(2.72^(umidade$W-12))
  umidade$RHrisk <- ifelse(umidade$RHrisk > 100, 100, umidade$RHrisk)
  mediaUR <- aggregate(RHrisk~Month,data=umidade,FUN=mean)

  Temp_f <- subset(dados,RH>85)
  Temp <- aggregate(TEMP~DAY+MONTH,data=Temp_f,FUN=mean)
  colnames(Temp) <- c("Day","Month","TMed")
  Temp <- as.data.frame(Temp)
  Temp$TEMPrisk <- (2*(Temp$TMed-8)*alfa*(22.75-8)*alfa-(Temp$TMed-8)*2*alfa)/(22.75-8)*2*alfa
  mediaTemp <- aggregate(TEMPrisk~Month,data=Temp,FUN=mean)
  riscofinal <- merge(mediaUR,mediaTemp)
  riscofinal <- as.data.frame(riscofinal)
  riscofinal$TOTALrisk <- riscofinal$RHrisk * riscofinal$TEMPrisk
  max_val <- max(riscofinal$TOTALrisk)
  min_val <- min(riscofinal$TOTALrisk)
  riscofinal$RELrisk <- (riscofinal$TOTALrisk-min_val)/(max_val-min_val)*100
  diarioGeral <- merge(umidade,Temp)
  diarioGeral$TOTALrisk <- diarioGeral$RHrisk * diarioGeral$TEMPrisk
  diarioGeral$TOTALrisk <- ifelse(diarioGeral$TOTALrisk > 100, 100, diarioGeral$TOTALrisk)
  cat("\n--------------------------------------------------\n")
  cat("Risk of Asian Rust Occurrence")
  cat("\n--------------------------------------------------\n")
  print(riscofinal)
    if(plot==T){
      layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE))
      boxplot(Temp$riscoTEMP ~ Temp$Month, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Temperature Risk (%)",xlab="Month",outline=F)
      boxplot(umidade$riscoUR ~ umidade$Mponth, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Relative Humidity Risk (%)",xlab="Month",outline=F)
      boxplot(diarioGeral$TOTALrisk ~ diarioGeral$Month, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Total Risk (%)",xlab="Month",outline=F)
    }
  }#ELSEIF - ###ENVARG
  }
