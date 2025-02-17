#'Estimates using polynomial equations.
#'@description
#'Determination of maximum technical efficiency (MET), maximum and minimum points
#'and plateau function.
#'@param indep Name of the column with the independent variable.
#'@param dep Name of the dependent variable column
#'@param type Type of analysis to be carried out. Use “MET” to extract the
#'maximum technical efficiency, “x3” to obtain the maximum and minimum points
#'and “plateau” to extract the parameters using the plateau function.
#'@param alpha Significance of the test.
#'@author Willyan Júnior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author José Antonio Gonzalez da Silva
#'@export

linearest <- function(indep,dep,type=NULL,alpha=0.05){
  if (is.null(type)) {
    stop("Enter the analysis to be carried out in the 'type'")
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
      return("The quadratic coefficient is not significant")
    }}

  if (type == "x3"){
    #Função incompleta, finalizar
    mod2 <- lm(dep ~ poly(indep,3,raw = T))
    coeff <- coef(mod2)
    cat("Coefficients of the cubic model:\n",coeff,"\n")

    b0 <- coeff[1]
    b1 <- coeff[2]
    b3 <- coeff[3]
    b4 <- coeff[4]
    der1 <- function(x) b1 + 2 * b2 * x + 3 * b3 * x^2

    # Resolver f'(x) = 0 para encontrar os pontos críticos
    pontos_criticos <- polyroot(c(b1, 2 * b2, 3 * b3))
    pontos_criticos <- Re(pontos_criticos[Im(pontos_criticos) == 0]) # Apenas raízes reais

    cat("Critical points (x):\n", pontos_criticos, "\n")

    # Derivada de 2ª ordem: f''(x) = 2*b2 + 6*b3*x
    derivada_2 <- function(x) 2 * b2 + 6 * b3 * x

    # Classificar os pontos críticos como máxima, mínima ou inflexão
    for (ponto in pontos_criticos) {
      valor_segunda_derivada <- derivada_2(ponto)
      if (valor_segunda_derivada > 0) {
        cat("Point x =", ponto, "is a minimum.\n")
      } else if (valor_segunda_derivada < 0) {
        cat("Point x =", ponto, "is a maximum.\n")
      } else {
        cat("Point x =", ponto, "is an inflection point.\n")
      }
    }

  if (type == "plateau") {
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

      cat("Plateau value (L):", round(L, digits = 5), "\n")
      cat("Growth rate (k):", round(k, digits = 5), "\n")
      cat("Inflection point (x0):", round(x0, digits = 5), "\n")
      return(list(platô = L, taxa_crescimento = k, ponto_inflexao = x0))
    }, error = function(e) {
      cat("Model fitting error:", e$message, "\n")
      return(NULL)
    })
  } else {
    stop("Type of analysis not recognized")
  }
}
}
