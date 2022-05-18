B.oxidacion <- function(carb, dobles = 0, tab = FALSE){
  
  # Fecha: 12 de Mayo 2022
  # Progamadoras: Juanita Gonzalez Restrepo y Valentina Rueda Paniagua
  
  # Descripcion: la funcion calcula los ATPs netos producidos a partir de la beta oxidacion de 
  # los acidos grasos, ademas permite identificar cuantos ATP se generan por cada uno de los
  # productos de la beta oxidacion.
  
  # Argumentos:
  
  # carb = numerico. Cantidad de carbonos en la cadena del acido graso.
  # dobles = numerico. Cantidad de enlaces dobles. Por defecto esta en 0 enlaces dobles.
  # tab = logico. Define si se quiere imprir el resultado en una tabla o no. 
  # Por defecto no se imprime la tabla.
  
  
  # Preparacion de datos.
  
  r <- carb %% 2 == 0 # Define si el # de carbonos es par o impar
  
  
  # Código.
  
  if( r ){
    
    vueltas <- (carb / 2) - 1  
    
    NADH2 <- vueltas * 3               # ATP producidos a partir del NADH2 
    FADH2 <- (vueltas * 2) - dobles    # ATP producidos a partir del FADH2 
    A.COA <- (vueltas + 1) * 12        # ATP producidos a partir del A.COA
    SUCCI <- 0
    
    ATP <- sum(c(NADH2, FADH2, A.COA))
    
    ATP.Neto <- ATP - 2
    
  }else{
    
    vueltas <- (carb - 3) / 2
    
    NADH2 <- vueltas * 3              # ATP producidos a partir del NADH2 
    FADH2 <- (vueltas * 2) - dobles   # ATP producidos a partir del FADH2 
    A.COA <- vueltas * 12             # ATP producidos a partir del A.COA 
    SUCCI <- 6                        # ATP producidos a partir del SUCCI 
    
    ATP <- sum(c(NADH2, FADH2, A.COA, SUCCI))
    
    ATP.Neto <- ATP - 2
  }
  
  # Creacion de resultado.
  
  if(tab){
    
    res <- data.frame(Carbonos = carb, Par = r, Ins = dobles, NADH2 = NADH2,
                      FADH2 = FADH2, Acetil.CoA = A.COA, Succinil = SUCCI, ATP = ATP, ATP.Neto = ATP.Neto )
  }else{
    
    res <- ATP.Neto
  }
  
  # Impresion.
  
  res
  
  # Ejemplos.
  
  # B.oxidacion(carb = 15, dobles = 3, tab = T)
  
  # B.oxidacion(15, 3, F)
  
  # B.oxidacion(15, 3)

  
  # B.oxidacion(carb = 20, dobles = 2, tab = T)
  
  # B.oxidacion(12)
  
  # B.oxidacion(12, 0, T)
  
} 