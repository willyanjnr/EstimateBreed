#'índices de Estresse para Seleção
#'@description
#'Índices de seleção de genótipos conduzidos sob condições de estresse citados
#'por Ghazvini et al. (2024).
#'@param GEN Genótipos a serem selecionados.
#'@param YS Produtividade do genótipo sem condições estressoras.
#'@param YC Prdotuvidade do genótipo sob condições estressoras.
#'@param index Índice a ser calculado (Padrão "ALL"). Os índices a serem utilizados
#'são: "STI" - Stress Tolerance Index, "YI" - Yield Index, "GMP" - Geometric Mean Productivity,
#'"MP" - Mean Productivity, "MH" - Harmonic Mean, "SSI" - Stress Stability Index,
#'"YSI" - Yield Stability Index, "RSI" - Relative Stress Index.
#'@param bygen Retorna a média de cada genótipo se "TRUE". Somente desta forma
#'será possível plotar gráficos.
#'@param plot Plotar gráfico se igual a "TRUE" (Padrão "F").
#'@param xlab Ajustar o título do eixo x no gráfico.
#'@param ylab Ajustar o título do eixo y no gráfico.
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Ghazvini, H., Pour-Aboughadareh, A., Jasemi, S.S. et al.
#'A Framework for Selection of High-Yielding and Drought-tolerant
#'Genotypes of Barley: Applying Yield-Based Indices and Multi-index
#'Selection Models. Journal of Crop Health 76, 601–616 (2024).
#'https://doi.org/10.1007/s10343-024-00981-1
#'@export
#'@examples
#'\donttest{
#'library(Breeding)
#'
#'data("aveia")
#'with(aveia,estresse(GEN,MC,MG,index = "ALL",bygen=T))
#'
#'}

estresse <- function(GEN,YS,YC,index="ALL",bygen=T,plot=F,xlab="Genótipo",ylab="Valores",...){
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  require(viridis)
  GEN1 = as.factor(GEN)
  YS1 = YS
  YC1 = YC
  verif <- data.frame(YS1,YC1)
  if (any(is.na(verif))) {
    warning("O data frame contém valores NA!")
  }

  if(bygen==T){
    media <- data.frame(GEN1,YS1,YC1)
    media_gen <- aggregate(cbind(YS1, YC1) ~ GEN1,
                           data = media,
                           FUN = mean,
                           na.rm = TRUE)
    GEN = media_gen$GEN1;YS = media_gen$YS;YC = media_gen$YC
    if(index=="STI"){
      STI <- ((YS*YC)/(YC^2))
      STI <- data.frame(GEN,STI)
      cat("STI Index")
      cat("\n----------------------------\n")
      print(STI)
      if(plot==T){
        ggplot(STI, aes(x = GEN, y = STI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "STI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="YI"){
      YI = YS/YC
      YI = data.frame(GEN,YI)
      cat("YI Index")
      cat("\n----------------------------\n")
      print(YI)
      if(plot==T){
        ggplot(YI, aes(x = GEN, y = YI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "YI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="GMP"){
      GMP = sqrt(YS*YC)
      GMP = data.frame(GEN,GMP)
      cat("GMP Index")
      cat("\n----------------------------\n")
      print(GMP)
      if(plot==T){
        ggplot(GMP, aes(x = GEN, y = GMP, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "GMP Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="MP"){
      MP = (YS*YC)/2
      MP = data.frame(GEN,MP)
      cat("MP Index")
      cat("\n----------------------------\n")
      print(MP)
      if(plot==T){
        ggplot(MP, aes(x = GEN, y = MP, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "MP Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="MH"){
      MH = (2*(YC-YS))/(YC+YS)
      MH = data.frame(GEN,MH)
      cat("MH Index")
      cat("\n----------------------------\n")
      print(MH)
      if(plot==T){
        ggplot(MH, aes(x = GEN, y = MH, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "MH Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="SSI"){
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      SSI = (1-(YC/YS))/(1-(xYC/xYS))
      SSI = data.frame(GEN,SSI)
      cat("SSI Index")
      cat("\n----------------------------\n")
      print(SSI)
      if(plot==T){
        ggplot(SSI, aes(x = GEN, y = SSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "SSI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="YSI"){
      YSI = YS/YC
      YSI = data.frame(GEN,YSI)
      cat("YSI Index")
      cat("\n----------------------------\n")
      print(YSI)
      if(plot==T){
        ggplot(YSI, aes(x = GEN, y = YSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "YSI Index", x = xlab, y = ylab)+coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="RSI"){
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      RSI = (YC/YS)/(xYC/xYS)
      RSI = data.frame(GEN,RSI)
      cat("RSI Index")
      cat("\n----------------------------\n")
      print(RSI)
      if(plot==T){
        ggplot(RSI, aes(x = GEN, y = RSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "RSI Index", x = xlab, y = ylab)+coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="ALL"){
      STI = ((YS*YC)/(YC^2))
      YI = YS/YC
      GMP = sqrt(YS*YC)
      MP = (YS*YC)/2
      MH = (2*(YC-YS))/(YC+YS)
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      SSI = (1-(YC/YS))/(1-(xYC/xYS))
      YSI = YS/YC
      RSI = (YC/YS)/(xYC/xYS)
      final <- data.frame(GEN,STI,YI,GMP,MP,MH,SSI,YSI,RSI)
      cat("\n---------------------------------------------------------------------------------------------\n")
      cat("Índices de Estresse")
      cat("\n---------------------------------------------------------------------------------------------\n")
      print(final)
      if(plot==T){
        #Mapping
        final <- final %>%
          mutate(across(starts_with("GEN"), as.factor),
                 across(c(STI, YI, GMP, MP, MH, SSI, YSI, RSI), as.numeric))

        #Agrupar todos os índices na mesma coluna
        dados_long <- pivot_longer(final, cols = c(STI, YI, GMP, MP, MH, SSI, YSI, RSI),
                                   names_to = "indice", values_to = "valores")

        #Gráfico com FW
        ggplot(dados_long, aes(x = GEN, y = valores, fill = GEN)) +
          geom_bar(stat = "identity") +
          facet_wrap(~ indice, ncol = 4) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "Valores dos Índices por Genótipo", x = xlab, y = ylab) +
          scale_fill_viridis_d()
      }
    }
  }
  else if(index=="STI"){
    STI <- ((YS*YC)/(YC^2))
    STI <- data.frame(GEN,STI)
    cat("STI Index")
    cat("\n----------------------------\n")
    print(STI)
  }
  else if(index=="YI"){
    YI = YS/YC
    YI = data.frame(GEN,YI)
    cat("YI Index")
    cat("\n----------------------------\n")
    print(YI)
  }
  else if(index=="GMP"){
    GMP = sqrt(YS*YC)
    GMP = data.frame(GEN,GMP)
    cat("GMP Index")
    cat("\n----------------------------\n")
    print(GMP)
  }
  else if(index=="MP"){
    MP = (YS*YC)/2
    MP = data.frame(GEN,MP)
    cat("MP Index")
    cat("\n----------------------------\n")
    print(MP)
  }
  else if(index=="MH"){
    MH = (2*(YC-YS))/(YC+YS)
    MH = data.frame(GEN,MH)
    cat("MH Index")
    cat("\n----------------------------\n")
    print(MH)
  }
  else if(index=="SSI"){
    xYC <- mean(media$YC)
    xYS <- mean(media$YS)
    SSI = (1-(YC/YS))/(1-(xYC/xYS))
    SSI = data.frame(GEN,SSI)
    cat("SSI Index")
    cat("\n----------------------------\n")
    print(SSI)
  }
  else if(index=="YSI"){
    YSI = YS/YC
    YSI = data.frame(GEN,YSI)
    cat("YSI Index")
    cat("\n----------------------------\n")
    print(YSI)
  }
  else if(index=="RSI"){
    xYC <- mean(media$YC)
    xYS <- mean(media$YS)
    RSI = (YC/YS)/(xYC/xYS)
    RSI = data.frame(GEN,RSI)
    cat("RSI Index")
    cat("\n----------------------------\n")
    print(RSI)
  }
  else if(index=="ALL"){
    media <- data.frame(GEN1,YS1,YC1)
    STI = ((YS*YC)/(YC^2))
    YI = YS/YC
    GMP = sqrt(YS*YC)
    MP = (YS*YC)/2
    MH = (2*(YC-YS))/(YC+YS)
    xYC <- mean(media$YC)
    xYS <- mean(media$YS)
    SSI = (1-(YC/YS))/(1-(xYC/xYS))
    YSI = YS/YC
    RSI = (YC/YS)/(xYC/xYS)
    final <- data.frame(GEN,STI,YI,GMP,MP,MH,SSI,YSI,RSI)
    cat("\n-----------------------------------------------------------------------------------------------\n")
    cat("Índices de Estresse")
    cat("\n-----------------------------------------------------------------------------------------------\n")
    print(final)
  }
}

#' Índice de Temperatura e Umidade
#'@description
#'Cálculo do Índice de Temperatura e Umidade (ITU)
#'
#'@param CICLO Coluna referente ao Ciclo
#'@param TM Colune referente à Temperatura Média
#'@param UR Coluna referente à Umidade Relativa
#'@export

itu <- function(CICLO,TM,UR){
  require(dplyr)
  require(ggplot2)
  Ciclo <- CICLO
  TM <- TM
  UR <- UR
  Tpo <- ((UR/100)^(1/8))*(112+(0.9*TM))+(0.1*TM)-112

  ITU <- TM+((0.36*Tpo)+41.5)
  if(ITU>=70){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Condição não estressante, faixa dentro do conforto térmico")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=71&ITU<=78){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Condição de estresse térmico")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=79&ITU<=83){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Condição de estresse térmico severo (situação de perigo)")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=84){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Condição de estresse térmico crítico (situação de emergência)")
    cat("\n-----------------------------------------------------------------\n")
  }
}
