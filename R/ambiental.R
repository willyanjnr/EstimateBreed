#' Cálculo da Soma Térmica
#' @description
#'
#' Cálculo da Soma Térmica de Culturas
#'
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

#' Fototermal
