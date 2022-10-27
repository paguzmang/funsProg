derivada <- function(fun, x0, h=0.001, lado = 'm') {
  # Oct 2022
  # DESCRIPCION: Calcula la aproximacion a la derivada de una funcion por medio de 
  # diferencias finitas con una distancia entre los puntos de h
  
  # ARGUMENTOS:
  #  fun = funcion matematica a evaluar (creada con function o cualquier funcion matematica que ya
  #        tenga el R guardada p.e., dnorm, dexp, dunif, etc.)
  #   x0 = numericos. indica el punto sobre el se desea calcular la derivada
  #    h = numerico. Por defecto toma el valor de 0.001. Indica distancia entre el punto al que se  
  #        quiere calcular la derivada y el punto que se usa para la aproximacion
  # lado = de texto. Por defecto toma el valor de 'm'. Indica si quiere calcular la aproximacion con 
  #       un punto a la izquiera ('i'), a la derecha ('d') o la media entre ambas aproximaciones ('m')
  
  # CODIGO:  
  # Validacion del argumento lado:
  mensaje <- "El argumento 'lado' debe ser 'i' para izquierda, 'd' para derecha o 'm' para el promedio de las dos"
  if(!lado[1] %in% c('i', 'd', 'm')) stop(mensaje)
  # Calculo de aproximacion por derecha
  der_i = (fun(x0+h)-fun(x0))/(h)
  # Calculo de aproximacion por izquiera
  der_d = (fun(x0)-fun(x0-h))/(h)
  # Calculo de la media entre ambas aproximaciones 
  der_m = (der_i + der_d)/2
  # Resultado de la funcion derivada según lo solicitado por eel usuario
  if(lado[1]=='i') return(der_i)
  if(lado[1]=='d') return(der_d)
  if(lado[1]=='m') return(der_m)
  
  # EJEMPLO: 
  # derivada(
  #  fun = function(x) sqrt(x)*exp(x),
  #  x0 = 4,
  #  h = 0.005,
  #  lado = 'd'
  # )
  # derivada(fun = function(x) 2*x**2, x0 = 1)
  # derivada(fun = function(x) 3*x**5, x0 = 2, h = 0.01)
  # derivada(fun = function(x) sqrt(x), x0 = 5, h = 0.5, lado = 'd') 
}

