#'Estimativas a partir de equações polinomiais.
#'@description
#'Determinação da máxima eficiência técnica (MET), x2, x3 e platô.
#'@param indep Nome da coluna com a variável independente.
#'@param dep Nome da coluna da variável dependente
#'@param type Tipo de análise a ser realizada.
#'@param alpha Significância do teste
#'@return Retorna uma tabela com os genótipos e os índices selecionados.
#'Quanto maior o valor do índice, mais resiliente é o genótipo.
#'@author Willyan Jr. A. Bandeira, Ivan R. Carvalho
#'@export

linearest <- function(indep,dep,type=NULL,alpha=0.05){
  if (is.null(type)) {
    stop("Informar a análise a ser realizada no 'type'")
  }

  if(type=="MET") {
    mod1 <- lm(dep ~ indep + I(indep^2))
    resumo <- summary(mod1)
    pvalue <- resumo$coefficients[3, 4]

    if(pvalue < alpha) {
      intercept <- as.numeric(mod1$coefficients["(Intercept)"])
      bval <- as.numeric(mod1$coefficients["indep"])
      cval <- as.numeric(mod1$coefficients["I(indep^2)"])
      MET <- -bval / (2 * cval)
      est_y <- intercept + bval * MET + cval * MET^2
      print(resumo)
      cat("MET =", round(MET, digits = 5), "\n")
      cat("y_MET =", round(est_y, digits = 5), "\n")
    } else {
      print(resumo)
      return("O coeficiente quadrático não é significativo")
    }}

  if (type == "platô") {
    tryCatch({
      L_init <- max(dep)
      k_init <- 0.1
      x0_init <- mean(indep)

      modelo_logistico <- nls(dep ~ L / (1 + exp(-k * (indep - x0))),
                              start = list(L = L_init, k = k_init, x0 = x0_init),
                              control = nls.control(maxiter = 1000))

      resumo_log <- summary(modelo_logistico)
      print(resumo_log)

      L <- coef(modelo_logistico)["L"]   # Platô
      k <- coef(modelo_logistico)["k"]   # Taxa de crescimento
      x0 <- coef(modelo_logistico)["x0"] # Ponto de inflexão

      cat("Valor do platô (L):", round(L, digits = 5), "\n")
      cat("Taxa de crescimento (k):", round(k, digits = 5), "\n")
      cat("Ponto de inflexão (x0):", round(x0, digits = 5), "\n")
      return(list(platô = L, taxa_crescimento = k, ponto_inflexao = x0))
    }, error = function(e) {
      cat("Erro no ajuste do modelo:", e$message, "\n")
      return(NULL)
    })
  } else {
    stop("Tipo de análise não reconhecido")
  }
}
