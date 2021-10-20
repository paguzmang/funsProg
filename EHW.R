# Documentacion de la funcion del Equilibrio de Hardy-Weinberg

EHW <- function(a,b,c,alfa=0.05){
  
  # Octubre 2021 | Autores: Karen Español, Luisa Murillo, Carolina Morales
  # Descripcion: esta funcion esta diseñada para determinar si la poblacion se encuentra en 
  #equilibrio de Hardy-Weinberg o no.
  
  # Argumentos:
  
  # "a" = representa la frecuencia del homocigotico dominante (AA)
  # "b" = representa la frecuencia del heterocigotico (AT)
  # "c" = representa la frecuencia del homocigotico dominente (TT)
  # "alfa" = margen de error, cuantifica la cantidad de error de muestreo aleatorio.

    
    n <- sum(a,b,c)  
    
    P <- a/n
    H <- b/n
    Q <- c/n
    
    p <- P + 1/2*H
    q <- Q + 1/2*H
    
    j <- p^2*n
    k <- 2*p*q*n
    l <- q^2*n
    
    #Regla de decision
    x2_cal <- ((a-j)^2/j) + ((b-k)^2/k) + ((c-l)^2/l)
    x2_tab <- round(qchisq(p=alfa,df=2,lower.tail = F),3)
    
    if(x2_cal >= x2_tab){
      Respuesta <- "No esta en el EHW"
    }else{
      Respuesta <- "Esta en el EHW"
      lim <- max(c(j,k,l)) 
      par(mar= c(3,5,3,1))
      barplot(c("Homocigoticos\nDominantes" =j,
                "Heterocigoticos"= k,"Homocigoticos\nRecesivos" =l),
              main = "Frecuencias Fenotípicas\nesperadas para EHW",
              ylim = c(0,lim + lim*0.10),
              ylab= "Individuos en la población",col="lightcoral"
      )
      text(0.7,4,round(j,2))
      text(1.9,4,round(k,2))
      text(3.1,4,round(l,2))
    }
    
    data.frame(
      x2_Calculado = x2_cal,
      x2_Tabulado = x2_tab,
      V = 2, 
      Conclusion = Respuesta
      
    )
    
    # Valor:
    # Esta funcion me entrega un data.frame que contiene:
    # 1. el chi cuadrado calculado
    # 2. el chi ciadrado tabulado
    # 3. los grados de libertad
    # 4. la Conclusion 
    
    # Ejemplo de uso
    # EHW(a = 25, b = 52, c = 33, alfa)
}
