#'Cálculo da Soma Térmica
#'@description
#'Cálculo da Soma Térmica de Culturas
#'@param TMED Coluna da temperatura média
#'@param cultura Definir a cultura (Padrão = "milho")
#'@param plot Plotar um gráfico do acúmulo (Padrão é T (TRUE))
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
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

#'Plotando as temperaturas bases e ótimas para as culturas
#'@description
#'Utilitária para plotar gráficos dos preferendos térmicos para as culturas
#'@param DAS descrption
#'@param Var desc
#'@param Cultura Soja, Milho, Trigo
#'@param ylab desc
#'@param xlab description
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

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
#' @param GEN Coluna referente ao genótipo
#' @param TMED Coluna referente a temperatura média
#' @param STAD Estádio fenológico conforme descrito por Fehr & Caviness (ANO).
#' @param habito Hábito de crescimento do genótipo (padrão="ind"). Utilizar
#' "ind" para indeterminado e "det" para determinado.
#' @param plot Imprimir o gráfico (padrão=T)
#' @references Porta, F. S. D., Streck, N. A., Alberto, C. M., da Silva, M. R.,
#'& Tura, E. F. (2024). Improving understanding of the plastochron of
#'determinate and indeterminate soybean cultivars. Revista Brasileira de
#'Engenharia Agrícola e Ambiental, 28(10), e278299.
#'https://doi.org/10.1590/1807-1929/agriambi.v28n10e278299
#' @export

plastocrono <- function(GEN, TMED, STAD, NN, habito = "ind", plot = FALSE) {
  require(dplyr)
  require(ggplot2)
  require(hrbrthemes)
  require(broom)
  require(purrr)
  require(ggrepel)
  require(grid)

  #Falta adicionar o caso de ter mais de um genótipo
  Tb = 7.6
  Tot = 31
  TB = 40
  resultado <- data.frame(GEN, TMED, STAD, NN) %>%
    group_by(GEN) %>%
    mutate(
      TTd = case_when(
        TMED > Tb & TMED <= Tot ~ (Tot - Tb) * ((TMED - Tb) / (Tot - Tb)) * 1,
        TMED > Tot & TMED <= TB ~ (Tot - Tb) * ((TMED - TB) / (Tot - TB)) * 1,
        TRUE ~ 0
      ),
      ATT = cumsum(TTd)
    )
  total <- resultado %>%
    group_by(GEN) %>%
    summarize(TST = max(ATT, na.rm = TRUE))

  if (habito == "ind") {
    dadosf <- resultado %>%
      group_by(NN) %>%
      mutate(STA = max(ATT)) %>%
      ungroup() %>%
      filter(STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10",
                         "R1", "R2", "R3", "R4", "R5")) %>%
      mutate(Class = case_when(
        STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10") ~ "Early",
        STAD %in% c("R1", "R2") ~ "Intermediate",
        STAD %in% c("R3", "R4", "R5") ~ "Late",
        TRUE ~ "Undefined"
      ))

    #LM by Class
    modc <- dadosf %>%
      group_by(Class) %>%
      summarise(
        modelo = list(lm(NN~STA,data=cur_data())),
        .groups = "drop"
      )
    coefic <- modc %>%
      mutate(coeff = lapply(modelo,coef))
    res <- modc %>%
      mutate(resumo = lapply(modelo,summary))
    cat("-------------------------------\n")
    cat("Early Soybean Pheno (V1 to R1)\n")
    print(res$resumo[[1]])
    cat("-------------------------------\n")
    cat("Intermediate Soybean Pheno (R1 to R3)\n")
    print(res$resumo[[2]])
    cat("-------------------------------\n")
    cat("Late Soybean Pheno (R3 to R5)\n")
    print(res$resumo[[3]])
  }

  if(habito=="det"){
    dadosf <- resultado %>%
      group_by(NN) %>%
      mutate(STA = max(ATT)) %>%
      ungroup() %>%
      filter(STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10",
                         "R1", "R2", "R3")) %>%
      mutate(Class = case_when(
        STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10") ~ "Early",
        STAD %in% c("R1","R2","R3") ~ "Late",
        TRUE ~ "Undefined"
      ))

    #LM by Class
    modc <- dadosf %>%
      group_by(Class) %>%
      summarise(
        modelo = list(lm(NN~STA,data=cur_data())),
        .groups = "drop"
      )
    coefic <- modc %>%
      mutate(coeff = lapply(modelo,coef))
    res <- modc %>%
      mutate(resumo = lapply(modelo,summary))
    cat("-------------------------------\n")
    cat("Early Soybean Pheno (V1 to R1)\n")
    print(res$resumo[[1]])
    cat("-------------------------------\n")
    cat("Late Soybean Pheno (R1 to R3)\n")
    print(res$resumo[[2]])
  }

  if (plot == TRUE) {
    modelos <- dadosf %>%
      group_by(Class) %>%
      summarise(model = list(lm(NN ~ STA, data = cur_data())), .groups = "drop")

    modelos_stats <- modelos %>%
      mutate(
        stats = map(model, ~ tidy(.x)),
        model_summary = map(model, ~ summary(.x)),
        rsq = map_dbl(model_summary, ~ .$r.squared),  # R²
        slope_pval = map_dbl(model_summary, ~ coef(.x)[2, "Pr(>|t|)"]),
        eq_text = map2(model, rsq, ~ paste(
          "y =", signif(coef(.x)[2], 6), "x +", signif(coef(.x)[1], 6), "\n",
          "R² =", signif(.y, 2), "\n",
          "Pr(>t) =", format.pval(coef(summary(.x))[2, "Pr(>|t|)"],
                                  digits = 5, eps = 1e-16)
        ))
      )

    dadosf <- dadosf %>%
      left_join(modelos, by = "Class") %>%
      mutate(pred = map2_dbl(model, STA, ~ predict(.x, newdata = data.frame(STA = .y)))) %>%
      left_join(modelos_stats %>% select(Class, eq_text), by = "Class")

    x_limits <- c(min(dadosf$STA), max(dadosf$STA))
    y_limits <- c(min(dadosf$NN), max(dadosf$NN))

    num_classes <- nrow(modelos_stats)
    spacing <- (y_limits[2] - y_limits[1]) * 0.1

    p <- ggplot(dadosf, aes(x = STA, y = NN, color = Class, shape = Class)) +
      geom_point(size = 3) +
      geom_line(aes(y = pred), size = 1.2) +
      labs(title = "Soybean Plastochron",
           x = "Accumulated Thermal Sum (ATT, ºC Day)",
           y = "Number of Nodes (NN)",
           color = "Class",
           shape = "Class") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      ) +
      scale_x_continuous(
        breaks = seq(floor(x_limits[1] / 200) * 200,
                     ceiling(x_limits[2] / 200) * 200, 200),
        expand = expansion(mult = 0.05)
      ) +
      scale_y_continuous(
        breaks = seq(floor(y_limits[1] / 2) * 2,
                     ceiling(y_limits[2] / 2) * 2, 2),
        expand = expansion(mult = 0.05)
      ) +
      coord_cartesian(xlim = x_limits, ylim = y_limits)

    for (i in 1:num_classes) {
      p <- p + annotation_custom(
        grob = textGrob(
          paste("Class:", modelos_stats$Class[i]),
          gp = gpar(fontsize = 10, fontface = "bold", col = "black")
        ),
        xmin = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        xmax = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        ymin = y_limits[2] - (i - 1) * spacing,
        ymax = y_limits[2] - (i - 1) * spacing
      )
      p <- p + annotation_custom(
        grob = textGrob(
          modelos_stats$eq_text[i],
          gp = gpar(fontsize = 10, fontface = "italic", col = "black")
        ),
        xmin = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        xmax = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        ymin = y_limits[2] - (i - 1) * spacing - 0.05 * (y_limits[2] - y_limits[1]),
        ymax = y_limits[2] - (i - 1) * spacing - 0.05 * (y_limits[2] - y_limits[1])
      )
    }
    print(p)
  }
}

#'Determinação do índice fototermal
#'@description
#'Cálculo do índice fototermal
#'@param DIA A coluna com o dia de ciclo
#'@param TMED A coluna com os valores de temperatura média
#'@param RAD A coluna com os valores de radiação incidente
#'@param PER A coluna com o período (utilize VEG para vegetativo e REP para
#'reprodutivo)
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@references
#'Zanon, A. J., & Tagliapietra, E. L. (2022). Ecofisiologia da soja:
#'Visando altas produtividades (2ª ed.). Field Crops.
#'@export

fototermal <- function(DIA, TMED, RAD, PER) {
  # Verificações iniciais
  if (length(DIA) != length(TMED)) {
    stop("O comprimento de 'DIA' deve ser igual ao comprimento de 'TMED'.")
  }
  if (length(DIA) != length(RAD)) {
    stop("O comprimento de 'DIA' deve ser igual ao comprimento de 'RAD'.")
  }
  if (length(DIA) != length(PER)) {
    stop("O comprimento de 'DIA' deve ser igual ao comprimento de 'PER'.")
  }
  if (!is.numeric(TMED) || any(TMED < 0)) {
    stop("Os valores de 'TMED' devem ser numéricos e positivos.")
  }
  if (!is.numeric(RAD) || any(RAD <= 0)) {
    stop("Os valores de 'RAD' (radiação) devem ser numéricos e positivos.")
  }

  data <- data.frame(DIA, PER, TMED, RAD, stringsAsFactors = FALSE)
  data <- data[order(data$DIA), ]
  T_base_dict <- c("vegetativo" = 7.6, "reprodutivo" = 0)

  data$Qac_final <- NA
  periodos <- unique(data$PER)
  offset <- 0
  resultado <- data.frame()

  for (p in periodos) {
    dados_periodo <- subset(data, PER == p)
    T_base <- T_base_dict[p]
    dados_periodo$Tef <- dados_periodo$TMED - T_base
    dados_periodo$Q <- dados_periodo$RAD / dados_periodo$Tef
    dados_periodo$Qac_final <- cumsum(dados_periodo$Q) + offset
    offset <- tail(dados_periodo$Qac_final, 1)
    resultado <- rbind(resultado, dados_periodo)
  }

  resultado <- resultado[order(resultado$DIA), ]
  return(resultado)
}

#' Aplicação de defensivos agrícolas
#' @description
#' Determinação do momento ideal para aplicação
#' @param LON Longitude (em decimal)
#' @param LAT Latitude (em decimal)
#' @param type Tipo de análise. Utilize 1 para forecast e 2 para dados temporais.
#' @param days Número de dias
#' @param control Tipo de produto a ser aplicado. Utilizar "fung" para fungicida,
#' "herb" para herbicida, "ins" para inseticidas, "bio" para produtos biológicos.
#' @param details Retorna o resultado de forma detalhada se TRUE.
#' @param dates Só utilizar esse argumento se type=2. Data de início e final
#' para a obtenção dos dados meteorológicos para um ciclo de cultivo.
#' @return Retorna os momentos ideais de aplicação, considerando cada cenário.
#' Tomado como parâmetro um DELTA_T entre 2 e 8, velocidade do vento entre 3 e 8,
#' e ausência de precipitação.
#' @author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#' @examples
#' \donttest{
#' library(Breeding)
#'
#' # Previsão das condições de aplicação
#' deltat(-53.696944444444,-28.063888888889,type=1,days=10)
#' View(forecast)
#'
#' # Análise retrospectiva das condições de aplicação
#' deltat(-53.696944444444,-28.063888888889,type=1,days=10,
#' dates=c("2023-01-01","2023-05-01"))
#' View(retrospective)
#' }
#' @export


deltat <- function(LON,LAT,type=2,days=7,control=NULL,
                   details=FALSE,dates=NULL,plot=FALSE){
  #Verificações inicias

  if (type==1) {
    # Tipo 1 - Forecast
    require(httr)
    require(jsonlite)
    require(dplyr)
    require(lubridate)

    url <- "https://api.open-meteo.com/v1/forecast"
    res <- GET(url, query = list(
      latitude = LAT,
      longitude = LON,
      hourly = "temperature_2m,relative_humidity_2m,windspeed_10m,precipitation",
      timezone = "auto",
      forecast_days = days
    ))
    if (status_code(res) != 200) {
      stop("Verifique as coordenadas")
    }
    previsao <- fromJSON(content(res, "text"))
    hora <- previsao$hourly$time
    temp <- previsao$hourly$temperature_2m
    ur <- previsao$hourly$relative_humidity_2m
    wind <- previsao$hourly$windspeed_10m
    prec <- previsao$hourly$precipitation
    df1 <- data.frame(
      Hora = hora,
      Temp = temp,
      UR = ur,
      WindS = wind,
      Prec = prec
    )
    df1$Hora <- as.POSIXct(df1$Hora, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    df1$Hora <- with_tz(df1$Hora, "America/Sao_Paulo")
    df1$Dia <- as.Date(df1$Hora, tz = "America/Sao_Paulo")
    df1$HoraF <- format(df1$Hora, "%H:%M")
    df1 <- df1[, c("Dia", "HoraF", "Temp", "UR", "WindS", "Prec")]
    colnames(df1)[2] <- "Hora"
    #Fazer o cálculo do DELTAT
    dt <- df1 %>%
      mutate(alpha = log(UR/100)+(17.27*Temp)/(237.7+Temp),
             Td = (237.7*alpha)/(17.27-alpha),
             DELTAT = Temp-Td)
    dt <- dt %>% select(-alpha,-Td)
    assign("forecast",dt,envir = .GlobalEnv)
    if(details==TRUE){
      print(previsao$hourly_units)
      print(dt)
    }
    if(plot==TRUE){
      require(ggplot2)

      #Fazer gráfico
    }
    if(is.null(control)){
      ideal <- dt %>%
        filter(DELTAT >= 2 & DELTAT <= 8,
               WindS < 10,
               Prec < 2)
      cat("Momentos com condição ideal de aplicação\n")
      cat("--------------------------------------------------------\n")
      print(ideal)
    } else if(control=="fung"){
      ideal <- dt %>%
        filter(DELTAT >= 2 & DELTAT <= 8,
               WindS < 10,
               Prec < 2,
               Temp >= 15 & Temp <=25)
      cat("Momentos de condição ideal para aplicação de fungicida\n")
      cat("--------------------------------------------------------\n")
      print(ideal)
    } else if(control=="ins"){
      ideal <- dt %>%
        filter(DELTAT >= 2 & DELTAT <= 8,
               WindS < 10,
               Prec < 2,
               Temp >= 15 & Temp <=30)
      cat("Momentos de condição ideal para aplicação de inseticida\n")
      cat("--------------------------------------------------------\n")
      print(ideal)
    } else if(control=="herb"){
      ideal <- dt %>%
        filter(DELTAT >= 2 & DELTAT <= 8,
               WindS < 10,
               Prec < 2,
               Temp >= 15 & Temp <=30)
      cat("Momentos de condição ideal para aplicação de herbicida\n")
      cat("--------------------------------------------------------\n")
      print(ideal)
    } else if(control=="bio"){
      ideal <- dt %>%
        filter(DELTAT >= 2 & DELTAT <= 8,
               WindS < 10,
               Prec < 2,
               Temp >= 15 & Temp <=30)
      cat("Momentos de condição ideal para aplicação de biológicos\n")
      cat("--------------------------------------------------------\n")
      print(ideal)
    }
  }

  if(type==2){
    #Tipo 2 - Dados históricos
    require(nasapower)

    if(is.null(dates) || length(dates) !=2){
      stop("O parâmetro 'dates' deve ser um vetor com duas datas
           no formato 'YYYY-MM-DD'. Exemplo: c('2023-01-01', '2023-05-01').")
    }
    clim <- get_power(
              community = "ag",
              pars = c("T2M", "RH2M", "PRECTOTCORR"),
              lonlat = c(LON, LAT),
              dates = dates,
              temporal_api = "hourly")
    assign("climate_data",clim,envir = .GlobalEnv)

    clim <- clim %>%
      select(-LON,-LAT)

    #Cálculo DELTAT
    dt <- clim %>%
      mutate(alpha = log(RH2M/100)+(17.27*T2M)/(237.7+T2M),
             Td = (237.7*alpha)/(17.27-alpha),
             DELTAT = T2M-Td)
    dt <- dt %>% select(-alpha,-Td)
    ideal <- dt %>%
      filter(DELTAT >= 2 & DELTAT <= 8,
             PRECTOTCORR < 2)
    assign("retrospective",ideal,envir = .GlobalEnv)
    if(details==TRUE){
      return(ideal)
    }
  }
}
