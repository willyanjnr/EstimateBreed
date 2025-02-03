#'Pressão de Seleção
#'@description
#'Resposta a seleção ponderada pela pressão de seleção
#'@param Var A coluna com o nome das variáveis de interesse
#'@param h A coluna com os valores de herdabilidade no sentido restrito
#'@param VF A coluna com os valores de variância fenotípica
#'@param P A coluna com os valores observados para as progênies
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

GS<-function(Var, h, VF, P = "1"){
  require(dplyr)

  Var = as.factor(Var)
  h = h
  VF = VF
  if(P=="1"){
    GS = (h*2.7*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 1% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 2% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 3% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 4% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 5% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 10% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 20% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 40% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 50% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 60% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 70% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 80% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 90% de Pressão")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

#######
#'Diferencial de Seleção Simples
#'@description
#'Resposta a seleção ponderada pelo Diferencial de Seleção Simples
#'@param Var A coluna com o nome das variáveis de interesse
#'@param h A coluna com os valores de herdabilidade no sentido restrito
#'@param DS A coluna com o valor do diferencial de seleção a ser aplicado para
#'cada variável
#'@export

GS2<-function(Var, h, DS){
  require(dplyr)

  Var = as.factor(Var)
  h = h
  DS = DS
  GS = h*DS
  final <- data.frame(Var,GS)
  cat("\n-----------------------------------------------------------------\n")
  cat("Ganho de Seleção Ponderado pelo Diferencial de Seleção")
  cat("\n-----------------------------------------------------------------\n")
  print(final)
}

####
#'Resposta a Seleção pelo Controle de Genitores
#'@description
#'Considera conhecer apenas o genitor materno, sem controle do polinizador,
#'ou uma seleção direta sem parentais
#'@param Var A coluna com as variáveis de interesse
#'@param h A coluna com os valores de herdabilidade no sentido restrito
#'@param VF A coluna com os valores de variância fenotípica
#'@param P A coluna com os valores das progênies
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

GS3<-function(Var, h, VF, P = "1"){
  require(dplyr)

  Var = as.factor(Var)
  h = h
  VF = VF
  if(P=="1"){
    GS = (h*2.7*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 1% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 2% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 3% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 4% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 5% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 10% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 20% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 40% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 50% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 60% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 70% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 80% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 90% de Pressão e Controle de Genitores")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

###########
#'Resposta Seleção pelo Ano
#'@description
#'Resposta a seleção ponderada pela Pressão Ponderado pelo Ano de seleção e Ano
#'@param Var A coluna com a variáveis de interesse
#'@param h A coluna com os valores de herdabilidade no sentido restrito
#'@param VF A coluna com os valores de variância fenotípica
#'@param P A coluna com o valor obtido para as progênies
#'@param Ano A coluna com o ano de seleção
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

GS4<-function(Var, h, VF, P = "1", Ano){
  require(dplyr)

  Var = as.factor(Var)
  h = h
  VF = VF
  Ano = Ano
  if(P=="1"){
    GS = (h*2.7*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 1% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 2% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 3% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 4% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 5% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 10% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 20% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 40% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 50% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 60% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 70% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 80% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Ganho de Seleção 90% de Pressão Ponderada pelo Ano")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

####
#'Seleção pelo Diferencial de Seleção (Média e Desvios)
#'@description
#'Seleção de Genótipos Transgressívos - Diferencial de Seleção (DS)
#'@param Gen A coluna com o nome do genótipo
#'@param Var A coluna com a variável de interesse
#'@param Testemunha A coluna com o valor da variável 'X' para as testemunhas
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

transgressivos<-function(Gen, Var, Testemunha,ylab="Seleção",xlab="Genótipos"){
  require(dplyr)
  require(ggplot2)

  Gen = as.factor(Gen)
  Var = Var
  Testemunha = Testemunha

  Média<-mean(Var)
  DSg<-mean(Testemunha)
  Desvio<-sd(Var)
  DS1S<-Média+Desvio
  DS2S<-Média+(2*Desvio)
  DS3S<-Média+(3*Desvio)

  parametros <- list(Média=Média,DSg=DSg,Desvio=Desvio,DS1S=DS1S,DS2S=DS2S,DS3S=DS3S)
  dados=data.frame(Gen,Var, Testemunha)


  grafico=ggplot(dados, aes(x = Gen, y = Var))+
    geom_text(
      label=rownames(dados),
      nudge_x =0, nudge_y = 0, color = "red",hjust =3,
      size = 10,
      check_overlap = F)+ylab(ylab)+xlab(xlab)+theme_classic()+

    geom_segment(aes(x = 0, y =Média, xend =Gen, yend = Média), linetype = 1, color = "darkred")+
    geom_label(aes(x=0.5, y=Média, label="Média"))+

    geom_segment(aes(x = 0, y =DSg, xend =Gen, yend =DSg), linetype = 2, color = "darkgray")+
    geom_label(aes(x=0.5, y=DSg, label="DS T"))+

    geom_segment(aes(x = 0, y =DS1S, xend =Gen, yend =DS1S), linetype = 3, color = "blue")+
    geom_label(aes(x=0.5, y=DS1S, label="DS1S"))+

    geom_segment(aes(x = 0, y =DS2S, xend =Gen, yend =DS2S), linetype = 4, color = "darkgreen")+
    geom_label(aes(x=0.5, y=DS2S, label="DS2S"))+

    geom_segment(aes(x = 0, y =DS3S, xend =Gen, yend =DS3S), linetype = 5, color = "darkorange")+
    geom_label(aes(x=0.5, y=DS3S, label="DS3S"))+
    ggtitle("Seleção de Genótipos Transgressívos - Diferencial de Seleção (DS)")

  print(grafico)

  cat("\n-----------------------------------------------------------------\n")
  cat("Seleção de Genótipos Transgressívos - Diferencial de Seleção (DS)")
  cat("\n-----------------------------------------------------------------\n")
  cat("Parâmetros")
  cat("\n-----------------------------------------------------------------\n")
  print(parametros)
  suppressWarnings()
}

###
#'Segregação Padrão
#'@description
#'Tabela didática da segregação padrão por geração
#'@param MELHORAMENTO Parâmetro base para a função imprimir a tabela
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export
SEGREGAÇÃO_PADRÃO<-function(MELHORAMENTO){

  require("dplyr")
  require("crayon")

  AUTÓGAMAS<-c("Genitores","F1","F2","F3","F4","F5","F6","F7","F8","F9")
  HETEROZIGOSE<-c(0,100,50,25,12.5,6.25,3.12,1.56,0.78,0.39)
  ALÓGAMAS<-c("-","-","S0","S1","S2","S3","S4","S5","S6","S7")
  HOMOZIGOSE<-c(100, 0,50,75,87.5,93.75,96.88,98.44,99.22, 99.61)
  MUTANTES<-c("-", "M0", "M1","M2", "M3","M4","M5","M6", "M7", "M8")
  SELEÇÃO<-c("Não","Não","Qualitativo","Qualitativo",
             "Quantitativo","Quantitativo","Quantitativo","Quantitativo",
             "Quantitativo","Quantitativo")
  N_GENES<-c("-","-","1 a 2",
             "1 a 2", "3 ou +", "3 ou +","3 ou +","3 ou +","3 ou +","3 ou +")
  E_AMBIENTE<-c("-","-","baixo",
                "baixo", "alto", "alto","alto","alto","alto","alto")

  TABELA<-data.frame(AUTÓGAMAS,ALÓGAMAS,HETEROZIGOSE,HOMOZIGOSE,MUTANTES,
                     SELEÇÃO,N_GENES, E_AMBIENTE)

  return(TABELA)
}

###
#'Coeficiente de endogamia
#'@description
#'Função para Cálculo do coeficiente de endogamia
#'@param var Coluna com o nome da variável
#'@param VG Coluna com a variância genotípica
#'@param VF Coluna com a variância fenotípica
#'@author
#'Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

Coeficiente_endogamia<-function(var, VG, VF){

  require("dplyr")
  require("crayon")
  require("ggplot2")
  require("knitr")
  require("cowplot")

  var = as.factor(var)
  VG = VG
  VF = VF
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

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F3 - Variâncias Corrigidas")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F3)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F3- Herdabilidade com sentido Restrito")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF3)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F4 - Variâncias Corrigidas")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F4)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F4- Herdabilidade com sentido Restrito")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF4)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F5 - Variâncias Corrigidas")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F5)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F5- Herdabilidade com sentido Restrito")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF5)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F6 - Variâncias Corrigidas")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F6)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F6- Herdabilidade com sentido Restrito")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF6)

}

###
#' Regressão Genitor Progênie
#' @description
#' Estimativa da Regressão Genitor x Progênie
#' @param ind description
#' @param Genitor description
#' @param Progenie description
#' @author
#' Willyan Jr. A. Bandeira, Ivan R. Carvalho
#' @export

reg_GP <- function(ind, Genitor, Progenie) {

  require("dplyr")
  require("ggplot2")

  ind = as.factor(ind)
  Genitor = Genitor
  Progenie = Progenie

  model <- lm(Progenie ~ Genitor)

  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  Progenie_umPai<-0.5*slope
  Progenie_MPais<-slope
  Meio_Irmão<-slope*0.25
  Irmão_Completo<-slope*0.50

  equation <- paste("y =", round(intercept, 2), "+", round(slope, 2), "* x")

  dados=data.frame(Genitor, Progenie)

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
    Meio_Irmão=Meio_Irmão,
    Irmão_Completo=Irmão_Completo,
    grafico=grafico)

  cat("\n-----------------------------------------------------------------\n")
  cat("Parâmetros")
  cat("\n-----------------------------------------------------------------\n")
  print(result)
}

###
#'Interações alélicas
#'@description
#'Exemplos didáticos de interações alélicas e gênicas
#'@param type Tipo de interação alélica. Utilize "ad" para aditividade, "dom"
#'para dominância completa, "domp" para dominância parcial e "sob" para
#'sobredominância.
#'@param ge Tipo de interação GxE. Utilize "aus" para ausência de interação,
#'"simples" para interação simples e "complex" para interação complexa.
#'@author
#'Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

ALELICA<-function(type=NULL,ge=NULL){
  require(ggplot2)

  if(!is.null(type)){
  if(type=="ad"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 70, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)), altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Interação Genética Aditiva",
           x = "Genótipo", y = "Altura de Planta (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="dom"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 120, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)), altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Interação Genética de Dominância",
           x = "Genótipo", y = "Altura de Planta (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="domp"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 90, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)), altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Interação Genética de Dominância Parcial",
           x = "Genótipo", y = "Altura de Planta (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  } else if(type=="sob"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 160, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)), altura = rep(altura, each = 10))
    ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
      labs(title = "Interação Genética de Sobredominância",
           x = "Genótipo", y = "Altura de Planta (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
  }
  }
  if(!is.null(ge)){
    if(ge=="aus"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Ambiente 1", "Ambiente 2")
      efeito_ambiente_1 <- c(60, 70)
      efeito_ambiente_2 <- c(95, 105)
      dados <- data.frame(
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Ausência de Interação Genótipo-Ambiente",
             x = "Ambiente", y = "Altura de Planta (cm)") +
        theme_minimal()
    } else if(ge=="simples"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Ambiente 1", "Ambiente 2")
      efeito_ambiente_1 <- c(60, 80)
      efeito_ambiente_2 <- c(85, 125)
      dados <- data.frame(
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Ausência de Interação Genótipo-Ambiente",
             x = "Ambiente", y = "Altura de Planta (cm)") +
        theme_minimal()
    } else if(ge=="complex"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Ambiente 1", "Ambiente 2")
      efeito_ambiente_1 <- c(60, 100)
      efeito_ambiente_2 <- c(110, 60)
      dados <- data.frame(
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
      )
      ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Ausência de Interação Genótipo-Ambiente",
             x = "Ambiente", y = "Altura de Planta (cm)") +
        theme_minimal()
    }
  }
}

#' Ganho Genético Aditivo
#' @description
#' Esta função estima o ganho genético aditivo, conforme descrito por Falconer (1987).
#' @param GEN Vetor ou dataframe contendo os genótipos a serem selecionados.
#' @param VAR Variável de interesse para análise.
#' @param h2 Herdabilidade do caráter (um valor entre 0 e 1).
#' @param P Performance da progênie (um valor numérico ou vetor).
#' @author
#' Willyan Jr. A. Bandeira, Ivan R. Carvalho
#' @export

gga <- function(GEN, VAR, h2, P) {
  require(dplyr)
  data <- data.frame(GEN,VAR,h2,P)
  data <- data %>%
    group_by(VAR) %>%
    mutate(u=(mean(P))+(mean(P)*0.1))
  ganho <- data %>%
    group_by(GEN) %>%
    mutate(AGV=h2*(P-u))
  return(ganho)
}

#'Número de indivíduos a serem selecionados em cada família
#'@description
#'Definição do número de indivíduos a serem selecionados pelo BLUPis
#'@param y Variável resposta
#'@param r A coluna com o efeito de repetição (fixos)
#'@param p A couna com o efeito de parcela (aleatório)
#'@param f A coluna com o efeito de genético de dominância associado a famílias
#'de irmãos-completos (aleatório)
#'@param b A coluna com o efeito dos blocos incompletos (aleatório)
#'@param A Matriz de parentesco obtida com o AGHmatrix
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Oliveira, R. A., Daros, E., Resende, M. D. V., Bespalhok-Filho, J. C.,
#'Zambon, J. L. C., Souza, T. R., & Lucius, A. S. F. (2011). Procedimento
#'Blupis e seleção massal em cana-de-açúcar. Bragantia, 70(4), 796–800.
#'Model 35 (SELEGEN)
#'@export

blupis <- function(){
  #Verificações
  #Função incompleta, finalizar###
  if(!is.matrix(A) || nrow(A) != ncol(A)){
    stop("Kinship Matrix must be square!")
  }

  #Alterar "dados" conforme a função
  if(!all(dados[[id_col]] %in% rownames(A))){
    stop("IDs in dataframe need to be the same of the Kinship Matrix")
    #Ver a necessidade de utilizar stop em vez de warning
  }
  #Processo de filtragem dos genótipos que estão na matriz A mas não estão no DF
  A <- A[as.character(dados[[id_col]]), as.character(dados[[id_col]])]
  require(sommer)
  #Fazer verificação do modelo
  #Quais procedimentos precisam ser realizados para o modelo?
  #Fazer o modelo com o sommer, requer matriz de parentesco
  modelo <- mmer(
    formula = as.formula(paste(y_col,"~1")),
    random = ~vs(ID,Gu=A),
    data = dados
  )
  summary(modelo)
  modelo$sigma

  #Ajustar o modelo para o sommer
  modelo <- lmer(y~r+(1|a)+(1|p)+(1|f)+(1|b),data=dados)
}

#'Parâmetros gerais para seleção
#'@description
#'Função para determinar parâmetros de seleção, com base em um experimento
#'realizado na cultura do arroz. Destinada para avaliação isolada do desempenho
#'de linhagens dentro de uma determinada população.
#'@param POP A coluna com a população em melhoramento.
#'@param GEN A coluna com os genótipos selecionados dentro da população.
#'@param VAR A coluna com a variável de interesse.
#'@param REP A coluna com as repetições (se houver).
#'@param K Pressão de seleção (Padrão 0.05).
#'@param type Tipo de experimento (balanceado ou desbalanceado). Utilize
#'"balanced" para balanceado e "unb" para desbalanceado.
#'@param check Argumento lógico. Faz as verificações do pressuposto do modelo
#'estatístico se o valor for igual a TRUE.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Yadav, S. P. S., Bhandari, S., Ghimire, N. P., Mehata, D. K., Majhi, S. K.,
#'Bhattarai, S., Shrestha, S., Yadav, B., Chaudhary, P., & Bhujel, S. (2024).
#'Genetic variability, character association, path coefficient, and diversity
#'analysis of rice (Oryza sativa L.) genotypes based on agro-morphological
#'traits. International Journal of Agronomy, 2024, Article ID 9946332.
#'https://doi.org/10.1155/2024/9946332
#'@export

genpar <- function(POP, GEN, REP = NULL, vars, K = 0.05, type = "balanced", check = FALSE) {
  require(dplyr)
  require(lme4)

  if (is.null(REP)) {
    stop("Informe as repetições", call. = FALSE)
  }

  varc <- data.frame(POP, GEN, REP)
  varc$POP <- as.factor(varc$POP)
  varc$GEN <- as.factor(varc$GEN)
  varc$REP <- as.factor(varc$REP)

  for (var_name in vars) {
    if (!(var_name %in% colnames(dados))) {
      stop(paste("A variável", var_name, "não existe no conjunto de dados."), call. = FALSE)
    }

    varc <- varc %>% mutate(!!var_name := dados[[var_name]])
    X <- tapply(varc[[var_name]], POP, mean)

    if (type == "balanced") {
      repet <- length(unique(varc$REP))
      model <- aov(varc[[var_name]] ~ GEN + REP, data = varc)
      anova_table <- summary(model)[[1]]
      MSg <- anova_table["GEN", "Mean Sq"]
      MSe <- anova_table["Residual", "Mean Sq"]
      pvalue <- anova_table["GEN", "Pr(>F)"]

    } else if (type=="unb"){
      #Modelo Linear Misto ou ANOVA tipo 3
    }

    if (pvalue >= 0.05) {
      print(paste("Resultados para a variável:", var_name))
      print(anova_table)
      warning("Efeito dos genótipos não significativo para a variável ", var_name, "!", call. = FALSE)
    } else {
      result <- data.frame(
        Parameters = c("sigmaE", "sigmaG", "sigmaP", "ECV", "GCV", "PCV", "H2", "GA", "GAM"),
        Valor = c(sigmaE, sigmaG, sigmaP, ECV, GCV, PCV, H2, GA, GAM)
      )
      colnames(result)[which(colnames(result) == "Valor")] <- var_name
      print(paste("Resultados para a variável:", var_name))
      print(anova_table)
      print(result)
    }
  }
}

#'Número de genes
#'@description
#'Determinação do número de genes
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

#Três tipos
#1 - Herança
#2 - Máximo e Mínimo
#3 - Artigo Arroz
