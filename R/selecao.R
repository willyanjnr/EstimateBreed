#'Selection pressure
#'@description
#'Response to selection weighted by selection pressure
#'@param Var The column with the name of the variables of interest
#'@param h The column with the restricted heritability values
#'@param VF The column with the phenotypic variance values
#'@param P The column with the values observed for the progenies
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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
    cat("Selection Gain 1% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 2% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 3% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 4% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 5% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 10% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 20% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 40% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 50% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 60% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 70% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 80% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 90% Pressure")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

#######
#'Single Selection Differential
#'@description
#'Response to selection weighted by the Simple Selection Differential
#'@param Var The column with the name of the variables of interest
#'@param h The column with the heritability values in the strict sense
#'@param DS The column with the value of the selection differential to be applied
#'to each variable
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

GS2<-function(Var, h, DS){
  require(dplyr)

  Var = as.factor(Var)
  h = h
  DS = DS
  GS = h*DS
  final <- data.frame(Var,GS)
  cat("\n-----------------------------------------------------------------\n")
  cat("Selection Gain Weighted by Selection Differential")
  cat("\n-----------------------------------------------------------------\n")
  print(final)
}

####
#'Response to Selection by Control of Genitors
#'@description
#'Consider knowing only the maternal parent, without controlling the pollinator,
#'or direct selection without parents
#'@param Var The column with the variables of interest
#'@param h The column with the restricted heritability values
#'@param VF The column with the phenotypic variance values
#'@param P The column with the progeny values
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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
    cat("Selection Gain 1% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 2% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 3% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 4% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 5% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 10% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 20% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 40% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 50% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 60% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 70% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 80% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*0.5*(sqrt(VF)))

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 90% of Pressure and Control of Genitors")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

###########
#'Response to Selection by Year
#'@description
#'Response to selection weighted by Pressure Weighted by Year of selection and
#'Year
#'@param Var The column with the variables of interest
#'@param h The column with the restricted heritability values
#'@param VF The column with the phenotypic variance values
#'@param P The column with the value obtained for the progenies
#'@param Year The column with the year of selection
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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
    cat("Selection Gain 1% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="2"){
    GS = (h*2.44*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 2% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="3"){
    GS = (h*2.27*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 3% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="4"){
    GS = (h*2.16*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 4% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="5"){
    GS = (h*2.08*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 5% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="10"){
    GS = (h*1.76*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 10% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="20"){
    GS = (h*1.4*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 20% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="40"){
    GS = (h*0.97*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 40% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="50"){
    GS = (h*0.8*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 50% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="60"){
    GS = (h*0.64*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 60% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="70"){
    GS = (h*0.5*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 70% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="80"){
    GS = (h*0.35*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 80% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)
  }else if(P=="90"){
    GS = (h*0.2*(sqrt(VF)))/Ano

    final <- data.frame(Var,GS)
    cat("\n-----------------------------------------------------------------\n")
    cat("Selection Gain 90% Pressure Weighted by Year")
    cat("\n-----------------------------------------------------------------\n")
    print(final)}}

####
#'Seleção pelo Diferencial de Seleção (Média e Desvios)
#'@description
#'Selection of Transgressive Genotypes - Selection Differential (SD)
#'@param Gen The column with the genotype name
#'@param Var The column with the variable of interest
#'@param Witness The column with the value of the variable 'X' for the witnesses
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

transgressivos<-function(Gen, Var, Testemunha,ylab="Selection",xlab="Genotypes"){
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
    geom_label(aes(x=0.5, y=Média, label="Mean"))+

    geom_segment(aes(x = 0, y =DSg, xend =Gen, yend =DSg), linetype = 2, color = "darkgray")+
    geom_label(aes(x=0.5, y=DSg, label="DS T"))+

    geom_segment(aes(x = 0, y =DS1S, xend =Gen, yend =DS1S), linetype = 3, color = "blue")+
    geom_label(aes(x=0.5, y=DS1S, label="DS1S"))+

    geom_segment(aes(x = 0, y =DS2S, xend =Gen, yend =DS2S), linetype = 4, color = "darkgreen")+
    geom_label(aes(x=0.5, y=DS2S, label="DS2S"))+

    geom_segment(aes(x = 0, y =DS3S, xend =Gen, yend =DS3S), linetype = 5, color = "darkorange")+
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
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

SEGREGAÇÃO_PADRÃO<-function(MELHORAMENTO){
  require("dplyr")
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
  COEF_F<-c(0.000,0.500,0.750,0.875,0.937,0.969,0.984,0.992,0.996,0.998)
  TABELA<-data.frame(AUTÓGAMAS,ALÓGAMAS,HETEROZIGOSE,HOMOZIGOSE,MUTANTES,
                     SELEÇÃO,N_GENES, E_AMBIENTE, COEF_F)
  return(TABELA)
}

###
#'Inbreeding coefficient
#'@description
#'Function for calculating the inbreeding coefficient
#'@param var Column with the variable name
#'@param VG Column with genotypic variance
#'@param VF Column with phenotypic variance
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

Coeficiente_endogamia<-function(var, VG, VF){

  require("dplyr")
  require("ggplot2")
  require("knitr")
  require("crayon")
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
  green(italic(cat("F3 - Corrected Variances")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F3)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F3- Heritability in the narrow sense")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF3)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F4 - Corrected Variances")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F4)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F4- Heritability in the narrow sense")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF4)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F5 - Corrected Variances")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F5)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F5- Heritability in the narrow sense")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF5)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F6 - Corrected Variances")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(F6)

  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  green(italic(cat("F6- Heritability in the narrow sense")))
  cat(crayon::white(bold("\n---------------------------------------------------------------------------------\n")))
  print(hF6)

}

###
#'Regression Genitor Progeny
#'@description
#'Estimation of Genitor x Progeny Regression
#'@param ind description
#'@param Genitor description
#'@param Progenie description
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

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
  cat("Parameters")
  cat("\n-----------------------------------------------------------------\n")
  print(result)
}

###
#'Allelic interactions
#'@description
#'Examples of allelic and gene interactions
#'@param type Type of allelic interaction. Use “ad” for additivity, “dom”
#'for complete dominance, “domp” for partial dominance and “sob” for
#'overdominance.
#'@param ge Type of GxE interaction. Use “aus” for no interaction,
#'“simple” for simple interaction and “complex” for complex interaction.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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
      labs(title = "Additive Genetic Interaction",
           x = "Genotype", y = "Plant Height (cm)") +
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
      labs(title = "Genetic Interaction of Dominance",
           x = "Genotype", y = "Plant Height (cm)") +
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
      labs(title = "Partial Dominance Genetic Interaction",
           x = "Genotpe", y = "Plant Height (cm)") +
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
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
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
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
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
        genotipo = rep(genotipo, times = 2),
        ambiente = rep(ambientes, each = 2),
        altura = c(efeito_ambiente_1, efeito_ambiente_2)
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
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

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

#'Number of individuals to be selected in each family
#'@description
#'Definition of the number of individuals to be selected by BLUPis
#'@param y Response variable
#'@param r The column with the repetition effect (fixed)
#'@param p The column with the plot effect (random)
#'@param f The column with the genetic effect of dominance associated with
#'families of full siblings (random)
#'@param b The column with the effect of incomplete blocks (random)
#'@param A Kinship matrix obtained with AGHmatrix
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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

#'General parameters for selection
#'@description
#'Function for determining selection parameters, based on an experiment
#'carried out on the rice crop. Intended for isolated evaluation of the performance
#'of strains within a given population.
#'@param POP The column with the population under improvement.
#'@param GEN The column with the selected genotypes within the population.
#'@param VAR The column with the variable of interest.
#'@param REP The column with the repetitions (if any).
#'@param K Selection pressure (Default 0.05).
#'@param type Type of experiment (balanced or unbalanced). Use
#'“balanced” for balanced and “unb” for unbalanced.
#'@param check Logical argument. Checks the model's assumptions
#'statistical if the value is equal to TRUE.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
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
    stop("Please infomr the replications", call. = FALSE)
  }

  varc <- data.frame(POP, GEN, REP)
  varc$POP <- as.factor(varc$POP)
  varc$GEN <- as.factor(varc$GEN)
  varc$REP <- as.factor(varc$REP)

  for (var_name in vars) {
    if (!(var_name %in% colnames(dados))) {
      stop(paste("The variable", var_name, "does not exist in the dataset."), call. = FALSE)
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
      print(paste("Results for the variable:", var_name))
      print(anova_table)
      warning("Effect of genotypes not significant for the variable ", var_name, "!", call. = FALSE)
    } else {
      result <- data.frame(
        Parameters = c("sigmaE", "sigmaG", "sigmaP", "ECV", "GCV", "PCV", "H2", "GA", "GAM"),
        Valor = c(sigmaE, sigmaG, sigmaP, ECV, GCV, PCV, H2, GA, GAM)
      )
      colnames(result)[which(colnames(result) == "Valor")] <- var_name
      print(paste("Results for the variable:", var_name))
      print(anova_table)
      print(result)
    }
  }
}
