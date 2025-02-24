#'Risk of Disease Occurrence in Soybeans
#'@description
#'Calculation of the Risk of Disease Occurrence in Soybeans as a Function of
#'Variables meteorological variables (Engers et al., 2024).
#'@param DIA The column for the day of the month.
#'@param MES The column for the month of the year (numeric value).
#'@param TEMP A coluna da temperatura média do ar (em ºC).
#'@param UR The relative humidity column (in \%).
#'@param doença Define the soybean disease (Standard = “rust”).
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
#'library(Breeding)
#'
#'# Rust Risk Prediction
#'data("clima")
#'with(clima, risco(DY, MO, TMED, RH, doença = "ferrugem"))
#'}
#'@export

risco <- function(DIA,MES,TEMP,UR,doença="rust",plot=F){
  DIA <- DIA
  MES <- as.factor(MES)
  TEMP <- TEMP
  UR <- UR
  dados <- data.frame(DIA,MES,TEMP,UR)
  if(doença=="rust"){
  alfa <- log(2)/log(2.30508474576)
  umidade <- aggregate(UR~DIA+MES,data=dados,FUN=function(x)sum(x>85))
  print(umidade)
  colnames(umidade) <- c("Dia","Mês","W")
  umidade <- as.data.frame(umidade)
  umidade$riscoUR <- 1/1+(2.72^(umidade$W-12))
  umidade$riscoUR <- ifelse(umidade$riscoUR > 100, 100, umidade$riscoUR)
  mediaUR <- aggregate(riscoUR~Mês,data=umidade,FUN=mean)

  Temp_f <- subset(dados,UR>85)
  Temp <- aggregate(TEMP~DIA+MES,data=Temp_f,FUN=mean)
  colnames(Temp) <- c("Dia","Mês","TMed")
  Temp <- as.data.frame(Temp)
  Temp$riscoTEMP <- (2*(Temp$TMed-8)*alfa*(22.75-8)*alfa-(Temp$TMed-8)*2*alfa)/(22.75-8)*2*alfa
  mediaTemp <- aggregate(riscoTEMP~Mês,data=Temp,FUN=mean)
  riscofinal <- merge(mediaUR,mediaTemp)
  riscofinal <- as.data.frame(riscofinal)
  riscofinal$riscoTotal <- riscofinal$riscoUR * riscofinal$riscoTEMP
  max_val <- max(riscofinal$riscoTotal)
  min_val <- min(riscofinal$riscoTotal)
  riscofinal$riscoRel <- (riscofinal$riscoTotal-min_val)/(max_val-min_val)*100
  diarioGeral <- merge(umidade,Temp)
  diarioGeral$riscoTotal <- diarioGeral$riscoUR * diarioGeral$riscoTEMP
  diarioGeral$riscoTotal <- ifelse(diarioGeral$riscoTotal > 100, 100, diarioGeral$riscoTotal)
  cat("\n--------------------------------------------------\n")
  cat("Risk of Asian Rust Occurrence")
  cat("\n--------------------------------------------------\n")
  print(riscofinal)
    if(plot==T){
      layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE))
      boxplot(Temp$riscoTEMP ~ Temp$Mês, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Temperature Risk (%)",xlab="Mês",outline=F)
      boxplot(umidade$riscoUR ~ umidade$Mês, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Relative Humidity Risk (%)",xlab="Mês",outline=F)
      boxplot(diarioGeral$riscoTotal ~ diarioGeral$Mês, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Total Risk (%)",xlab="Mês",outline=F)
    }
  }#ELSEIF - ###ENVARG
  }
