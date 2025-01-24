#'Cálculo da Soma Térmica
#'@description
#'Cálculo da Soma Térmica de Culturas
#'@param TMED Coluna da temperatura média
#'@param cultura Definir a cultura (Padrão = "milho")
#'@param plot Plotar um gráfico do acúmulo (Padrão é T (TRUE))
#'@export
somatermica <- function(TMED,MONTH,cultura="milho",plot=T){
  require(ggplot2)
  TMED = TMED
  MONTH = MONTH
  if(cultura=="milho"){
    TBi=10
    ST=TMED-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(TMED)/mean(TMED))*100
    VMax <- max(TMED)
    VMin <- min(TMED)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    cat("\n----------------------------\n")
    cat("Soma Térmica para o Milho")
    cat("\n----------------------------\n")
    cat("Ciclo Total =",tail(acumulado$Ciclo, n = 1),"Dias\n")
    cat("ST =",paste(STot),"GDD\n")
    cat("TBi =",paste(TBi),"ºC\n")
    cat("Valor Máximo =",paste(VMax),"ºC\n")
    cat("Valor Mínimo =",paste(VMin),"ºC\n")
    cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    if(plot==T){
        grafico=ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("ST Acumulada (ºC)")+xlab("Ciclo do Milho")+theme_classic()+
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
  }
  }
  else if(cultura=="soja"){
    TBi=5
    ST=TMED-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(TMED)/mean(TMED))*100
    VMax <- max(TMED)
    VMin <- min(TMED)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    cat("\n----------------------------\n")
    cat("Soma Térmica para a Soja")
    cat("\n----------------------------\n")
    cat("Ciclo Total =",tail(acumulado$Ciclo, n = 1),"Dias\n")
    cat("ST =",paste(STot),"GDD\n")
    cat("TBi =",paste(TBi),"ºC\n")
    cat("Valor Máximo =",paste(VMax),"ºC\n")
    cat("Valor Mínimo =",paste(VMin),"ºC\n")
    cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    if(plot==T){
      grafico=ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("ST Acumulada (ºC)")+xlab("Ciclo da Soja")+theme_classic()+
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
  }
  else if (cultura=="linhaça"){
    TBi=-4
    ST=TMED-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(TMED)/mean(TMED))*100
    VMax <- max(TMED)
    VMin <- min(TMED)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    cat("\n----------------------------\n")
    cat("Soma Térmica para a Linhaça")
    cat("\n----------------------------\n")
    cat("Ciclo Total =",tail(acumulado$Ciclo, n = 1),"Dias\n")
    cat("ST =",paste(STot),"GDD\n")
    cat("TBi =",paste(TBi),"ºC\n")
    cat("Valor Máximo =",paste(VMax),"ºC\n")
    cat("Valor Mínimo =",paste(VMin),"ºC\n")
    cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    if(plot==T){
      grafico=ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("ST Acumulada (ºC)")+xlab("Ciclo da Linhaça")+theme_classic()+
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
  }
  else if (cultura=="trigo"){
    TBi=-4
    ST=TMED-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(TMED)/mean(TMED))*100
    VMax <- max(TMED)
    VMin <- min(TMED)
    acumulado <- data.frame(ST,STAc,MONTH)
    acumulado$Ciclo <- 1:nrow(acumulado)
    print(acumulado)
    cat("\n----------------------------\n")
    cat("Soma Térmica para o Trigo")
    cat("\n----------------------------\n")
    cat("Ciclo Total =",tail(acumulado$Ciclo, n = 1),"Dias\n")
    cat("ST =",paste(STot),"GDD\n")
    cat("TBi =",paste(TBi),"ºC\n")
    cat("Valor Máximo =",paste(VMax),"ºC\n")
    cat("Valor Mínimo =",paste(VMin),"ºC\n")
    cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    if(plot==T){
      grafico=ggplot(acumulado,aes(x=Ciclo,y=STAc))+
        geom_point(aes(colour=factor(MONTH)),size=5)+
        geom_line(color = "blue", linewidth = 1) + theme_bw()+
        scale_x_continuous(breaks = seq(0,250, by = 15))+
        ggtitle("")+ylab("Accumulated Thermal Sum (ºC)")+xlab("Cycle")+
        theme(text = element_text(size = 18))+
        guides(color = guide_legend(title = "Months"))+
        ylab("Accumulated Thermal Sum (ºC)")+xlab("Cycle")+theme_classic()+
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))+
        scale_y_continuous(breaks=seq(0,tail(acumulado$STAc, n = 1)+125,250))
      plot(grafico)
    }
  else if (cultura=="aveia"){
    TBi=-4
    ST=TMED-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(TMED)/mean(TMED))*100
    VMax <- max(TMED)
    VMin <- min(TMED)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    cat("\n----------------------------\n")
    cat("Soma Térmica para a Aveia Branca")
    cat("\n----------------------------\n")
    cat("Ciclo Total =",tail(acumulado$Ciclo, n = 1),"Dias\n")
    cat("ST =",paste(STot),"GDD\n")
    cat("TBi =",paste(TBi),"ºC\n")
    cat("Valor Máximo =",paste(VMax),"ºC\n")
    cat("Valor Mínimo =",paste(VMin),"ºC\n")
    cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    if(plot==T){
      grafico=ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("ST Acumulada (ºC)")+xlab("Ciclo da Linhaça")+theme_classic()+
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
  }
  }
  }
}

#' Plotando as temperaturas bases e ótimas para as culturas
#'
#'
#' @param DAS descrption
#' @param Var desc
#' @param Cultura Soja, Milho, Trigo
#' @param ylab desc
#' @param xlab description
#'
#'
#'
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#' @export





TEMP_BASE<-function(DAS,
                    Var,
                    Cultura = "Soja",
                    ylab = "Atributo meteorológico", xlab = "Dias após Semeadura"){

  require(dplyr)
  require(ggplot2)

  DAS = as.numeric(DAS)
  Var = Var
  Cultura=Cultura

  if(Cultura=="Soja"){
    TbInferior<-10
    TbSuperior<-35
    ToInferior<-20
    ToSuperior<-30
    TGeral<-mean(Var)
    Tmax<-max(Var)
    Tmin<-min(Var)

    dados<-data.frame(DAS, Var)


    grafico=ggplot(dados, aes(x = DAS, y = Var))+
      geom_line(col = "red", size =0.8, linetype = 2,group=1)+ylab(ylab)+xlab(xlab)+theme_classic()+
      geom_segment(aes(x = 0, y =TbInferior, xend =DAS, yend = TbInferior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbInferior, label="Temperatura base inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =TbSuperior, xend = DAS, yend =TbSuperior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbSuperior, label="Temperatura base superior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToInferior, xend =DAS, yend =ToInferior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToInferior, label="Temperatura ótima inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToSuperior, xend =DAS, yend =ToSuperior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToSuperior, label="Temperatura ótima superior"))+theme_classic()


    parâmetros<-list(

      TbInferior=TbInferior,
      TbSuperior=TbSuperior,
      ToInferior=ToInferior,
      ToSuperior=ToSuperior,
      TGeral=TGeral,
      Tmax=Tmax,
      Tmin=Tmin
    )

    print(grafico)

    cat("\n-----------------------------------------------------------------\n")
    cat("Parâmetros gerais - SOJA")
    cat("\n-----------------------------------------------------------------\n")
    print(parâmetros)
  }
  else if (Cultura=="Milho"){
    TbInferior<-10
    TbSuperior<-34
    ToInferior<-18
    ToSuperior<-30
    TGeral<-mean(Var)
    Tmax<-max(Var)
    Tmin<-min(Var)

    dados<-data.frame(DAS, Var)
    grafico=ggplot(dados, aes(x = DAS, y = Var))+
      geom_line(col = "red", size =0.8, linetype = 2,group=1)+ylab(ylab)+xlab(xlab)+theme_classic()+
      geom_segment(aes(x = 0, y =TbInferior, xend =DAS, yend = TbInferior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbInferior, label="Temperatura base inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =TbSuperior, xend = DAS, yend =TbSuperior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbSuperior, label="Temperatura base superior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToInferior, xend =DAS, yend =ToInferior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToInferior, label="Temperatura ótima inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToSuperior, xend =DAS, yend =ToSuperior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToSuperior, label="Temperatura ótima superior"))+theme_classic()

    parâmetros<-list(
      TbInferior=TbInferior,
      TbSuperior=TbSuperior,
      ToInferior=ToInferior,
      ToSuperior=ToSuperior,
      TGeral=TGeral,
      Tmax=Tmax,
      Tmin=Tmin
    )

    print(grafico)
    cat("\n-----------------------------------------------------------------\n")
    cat("Parâmetros gerais - MILHO")
    cat("\n-----------------------------------------------------------------\n")
    print(parâmetros)
  }

  else if (Cultura=="Trigo"){
    TbInferior<-1.5
    TbSuperior<-30
    ToInferior<-17.2
    ToSuperior<-26
    TGeral<-mean(Var)
    Tmax<-max(Var)
    Tmin<-min(Var)

    dados<-data.frame(DAS, Var)
    grafico=ggplot(dados, aes(x = DAS, y = Var))+
      geom_line(col = "red", size =0.8, linetype = 2,group=1)+ylab(ylab)+xlab(xlab)+theme_classic()+
      geom_segment(aes(x = 0, y =TbInferior, xend =DAS, yend = TbInferior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbInferior, label="Temperatura base inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =TbSuperior, xend = DAS, yend =TbSuperior), linetype = 1, color = "blue")+
      geom_label(aes(x=15, y=TbSuperior, label="Temperatura base superior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToInferior, xend =DAS, yend =ToInferior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToInferior, label="Temperatura ótima inferior"))+theme_classic()+
      geom_segment(aes(x = 0, y =ToSuperior, xend =DAS, yend =ToSuperior), linetype = 2, color = "darkgreen")+
      geom_label(aes(x=15, y=ToSuperior, label="Temperatura ótima superior"))+theme_classic()

    parâmetros<-list(
      TbInferior=TbInferior,
      TbSuperior=TbSuperior,
      ToInferior=ToInferior,
      ToSuperior=ToSuperior,
      TGeral=TGeral,
      Tmax=Tmax,
      Tmin=Tmin
    )

    print(grafico)
    cat("\n-----------------------------------------------------------------\n")
    cat("Parâmetros gerais - Trigo")
    cat("\n-----------------------------------------------------------------\n")
    print(parâmetros)
  }
}

#' Estimativa do plastocrono da soja
#' @description
#' Estimativa do plastocrono da soja por meio da Tmed
#'
#' @param GEN Coluna referente ao genótipo
#' @param TMED Coluna referente a temperatura média
#' @param NNOS Número de nós mensurados a campo
#' @param habito Hábito de crescimento do genótipo (padrão="ind")
#' @param plot Imprimir o gráfico (padrão=T)
#' @export

#Função incompleta, finalizar
plastocrono <- function(GEN,TMED,NNOS,habito="ind",plot=F){
  require(ggplot2)
  require(hrbrthemes)
  GEN <- as.factor(GEN)
  Tmed <- TMED
  NNOS <- NNOS
  Tb = 7.6
  Tot = 31
  TB = 40
  if(Tb<Tmed && Tmed<Tot){
    TTd1 = (Tot-Tb)*((Tmed-Tb)/(Tot-Tb))*1
    if (Tot<Tmed && Tmed<TB){
      TTd2 = (Tot-Tb)*((Tmed-TB)/(Tot-TB))*1
    }
    if (Tmed<Tb && Tmed>TB){
      TTd3=0
      ATT=TTd1+TTd2+TTd3
    }}
  Plast = AAT/NNOS
  Final <- data.frame(GEN,Plast)
  Graf <- data.frame(NNOS,AAT)
  cat("\n-------------------------------------------\n")
  cat("Plastocrono")
  cat("\n-------------------------------------------\n")
  return(Final)
  if(plot==T){
    linear <- ggplot(Graf, aes(x=AAT, y=NNOS)) +
      geom_point() +
      geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
      theme_ipsum()
  }}

#'Determinação do índice fototermal
#'@description
#'Cálculo do índice fototermal
#'@param DIA A coluna com o dia de ciclo
#'@param TMED A coluna com os valores de temperatura média
#'@param N A coluna com os valores de fotoperíodo
#'@param plot Argumento lógico. Plotar um gráfico se TRUE
#'@export

fototermal <- function(DIA, TMED, N,plot=F) {
  require(dplyr)

  # Verificação de entradas
  if (length(DIA) != length(TMED)) {
    stop("O comprimento de 'DIA' deve ser igual ao comprimento de 'TMED'.")
  }
  if (length(DIA) != length(N) && length(N) != 1) {
    stop("O comprimento de 'N' deve ser igual ao comprimento de 'DIA' ou 'N'
         deve ser um valor constante.")
  }
  if (!is.numeric(TMED) || any(TMED < 0)) {
    stop("Os valores de 'TMED' devem ser numéricos e positivos.")
  }
  if (!is.numeric(N) || any(N <= 0)) {
    stop("Os valores de 'N' (fotoperíodo) devem ser numéricos e positivos.")
  }

  data <- data.frame(DIA, TMED, N)
  data <- data %>%
    mutate(Tef = TMED - 10,
           IFTd = Tef * N)
  if (any(data$Tef < 0)) {
    stop("Temperatura média (TMED) deve ser maior que 10°C para garantir uma
         temperatura efetiva positiva.")
  }
  data <- data %>%
    mutate(IFTac = cumsum(IFTd))
  return(data)
}

#' Balanço hídrico
# Instalar pacotes necessários
# install.packages("climate", dependencies = TRUE)
# install.packages("meteoland", dependencies = TRUE)
# install.packages("rnoaa", dependencies = TRUE)
# install.packages("soiltexture", dependencies = TRUE)

#library(climate)
#library(meteoland)
#library(rnoaa)
#library(soiltexture)

# Função para cálculo do balanço hídrico complexo com automação de parâmetros
balanco_hidrico_complexo_auto <- function(P, ET, R, D, I, lat, lon, altitude, S_max, porosidade, capacidade_infiltracao, data_inicio, data_fim) {

  # Obter dados climáticos (temperatura média, umidade, etc.) utilizando o pacote 'climate'
  clima <- climate_data(lat = lat, lon = lon, start_date = data_inicio, end_date = data_fim, variables = c("temperature", "humidity", "wind_speed", "radiation"))

  # Obter dados de temperatura e umidade
  T <- clima$temperature  # Temperatura média (°C)
  UR <- clima$humidity    # Umidade relativa (%)
  u2 <- clima$wind_speed  # Velocidade do vento (m/s)
  Rn <- clima$radiation   # Radiação líquida (MJ/m²/dia)

  # Calcular a pressão atmosférica (Pₐ) com base na altitude (estimativa)
  P_a <- 101.3 * ((1 - 2.25577e-5 * altitude) ^ 5.2559)  # Fórmula para pressão atmosférica em função da altitude

  # Funções internas para calcular eₛ, eₐ, Δ e γ
  calcular_es <- function(T) {
    return(0.6108 * exp((17.27 * T) / (T + 237.3)))  # Pressão de vapor de saturação (kPa)
  }

  calcular_ea <- function(es, UR) {
    return(es * (UR / 100))  # Pressão de vapor atual (kPa)
  }

  calcular_delta <- function(T) {
    return((4098 * (0.6108 * exp((17.27 * T) / (T + 237.3)))) / ((T + 237.3)^2))  # Declive da curva de saturação (kPa/°C)
  }

  calcular_gamma <- function(P_a) {
    return(0.665 * 10^(-3) * P_a)  # Constante psicrométrica (kPa/°C)
  }

  # Calculando os parâmetros
  es <- sapply(T, calcular_es)  # Pressão de vapor de saturação
  ea <- mapply(calcular_ea, es, UR)  # Pressão de vapor atual
  delta <- sapply(T, calcular_delta)  # Declive da curva de saturação
  gamma <- calcular_gamma(P_a)  # Constante psicrométrica

  # Definir o armazenamento inicial de água no solo (geralmente começa com zero)
  S <- 0  # Variação do armazenamento de água no solo

  # Armazenamento no solo ao longo do tempo
  S_acumulado <- numeric(length(P))  # Vetor para armazenar valores de S ao longo do tempo

  # Iterar sobre cada período (dia, mês, etc.)
  for (t in 1:length(P)) {
    # 1. Precipitação (P) e Irrigação (I)
    agua_adicionada <- P[t] + I[t]

    # 2. Evapotranspiração (ET) com método Penman-Monteith
    ET_calculado <- (0.408 * delta[t] * (Rn[t] - G[t]) + gamma * (900 / (T[t] + 273)) * u2[t] * (es[t] - ea[t])) /
      (delta[t] + gamma * (1 + 0.34 * u2[t]))  # Evapotranspiração com método Penman-Monteith

    # 3. Escoamento (R)
    escoamento <- min(R[t], agua_adicionada)

    # 4. Infiltração no solo
    agua_infiltrada <- min(agua_adicionada - escoamento, capacidade_infiltracao)

    # 5. Drenagem para o lençol freático (D)
    drenagem <- min(agua_infiltrada, S[t])  # A drenagem não pode ultrapassar o armazenamento

    # Atualizando o armazenamento de água no solo (S)
    S[t] <- S[t] + agua_adicionada - ET_calculado - escoamento - drenagem

    # Verificando se o armazenamento ultrapassou a capacidade máxima do solo (S_max)
    if (S[t] > S_max) {
      S[t] <- S_max  # O armazenamento no solo não pode ser maior que a capacidade de retenção
    }

    # Armazenar a variação do armazenamento de água no solo ao longo do tempo
    S_acumulado[t] <- S[t]
  }

  return(S_acumulado)  # Retorna a variação do armazenamento de água no solo ao longo do tempo
}
