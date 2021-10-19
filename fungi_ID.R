fungi.ID <- function(hname = NULL,nombrec = NULL, hcolor = NULL , tamañogeneral = NULL, formasombrero = NULL, tamañopie=NULL, grosorpie= NULL){
  
  # Creación: 12/10/21  |  Autores: Sebastian Giraldo Montoya; Omar Andrés Gutierrez Rojas; Daniel Alejandro Jimenez Toro
  
  # Descripcion:
  
  # Funcion que permite filtrar un data.frame (base de datos) deacuerdo a
  # multiples rasgos o caracteristicas de una serie de hongos comestibles que toman valores de texto.
  
  # El usuario puede usar uno o alguna combinacion de estas caracteristicas, o hasta ninguna, con esta ultima 
  # mostrandonosla base de datos o data.frame completa.
  
  
  # Argumentos:
  # hname = rasgo 1. Una o varias cadenas de texto de la columna hongo del data.frame (ho) entre:
  #shiitake,porcini,champiñon,trufa,matsutake,enoki,girgola,tromp.de los muertos,senderilla,
  #portobello,tromp.amarilla,colmenilla,champiñon bola de nieve,champiñon silvestre,champiñon anisado,oronja". Por defecto "NULL"
  
  # nombrec = rasgo 2. Una o varias cadenas de texto de la columna ncien del data.frame (ho) entre: 
  #"Lentinula edodes","Boletus edulis","Agaricus bisporus","Tuber melanosporum","Tricholoma matsutake","Flammulina velutipes",
  #"Pleurotus ostreatus","Cantharellus cinereus","Marasmius oreades","Agaricus bisporus","Craterellus lutescens","Morchella esculenta","agaricus arvensis",
  #"agaricus campestris","agaricus silvicola","amanita caesarea". Por defecto NULL
  
  # hcolor = rasgo 3. Una o varias cadenas de texto de la columna hcol del data.frame (ho) entre: 
  #"cafe","cafe y blanco","blanco","cafe","cafe y blanco","blanco","beige","negro","beige",
  #"cafe y blanco","amarillo","cafe y blanco","blanco","blanco","blanco","naranja". Por defecto NULL
  
  # tamañogeneral = rasgo 4. Una o varias cadenas de texto de la columna tamañoG del data.frame (ho) entre:
  #"grande","grande","pequeño","pequeño","grande","grande","pequeño","mediano","grande",
  #"pequeño","pequeño","grande","mediano","grande","mediano","mediano". Por defecto NULL
  
  # formasombrero = rasgo 5. Una o varias cadenas de texto de la columna formaS del data.frame (ho) entre:
  #"convexo","hemisferico","globoso","globoso","globoso","hemisferico","convexo","embudo","mamelonado",
  #"convexo","embudo","acampanado","globoso","convexo","convexo","convexo". Por defecto NULL
  
  # tamañopie = rasgo 6. Una o varias cadenas de texto de la columna tamañoP del data.frame (ho) entre:
  #"corto","alargado","corto","sin pie","alargado","alargado","alargado","alargado","alargado",
  #"corto","alargado","corto","muy alargado","alargado","alargado","alargado". Por defecto NULL
  
  # grosorpie = rasgo 7.Una o varias cadenas de texto de la columna grosorP del data.frame (ho) entre:
  #"delgado","grueso","grueso","sin pie","grueso","delgado","delgado","grueso","delgado",
  #"delgado","delgado","grueso","grueso","grueso","delgado","grueso". Por defecto NULL
  
  # Datos completos:
  ho <- data.frame(
    hongo = c("shiitake","porcini","champiñon","trufa","matsutake","enoki","girgola","tromp.de los muertos","senderilla",
              "portobello","tromp.amarilla","colmenilla","champiñon bola de nieve","champiñon silvestre","champiñon anisado","oronja"),
    nCien= c("Lentinula edodes","Boletus edulis","Agaricus bisporus","Tuber melanosporum","Tricholoma matsutake","Flammulina velutipes",
             "Pleurotus ostreatus","Cantharellus cinereus","Marasmius oreades","Agaricus bisporus","Craterellus lutescens","Morchella esculenta","agaricus arvensis",
             "agaricus campestris","agaricus silvicola","amanita caesarea"),
    
    hcol= c("cafe","cafe y blanco","blanco","cafe","cafe y blanco","blanco","beige","negro","beige",
            "cafe y blanco","amarillo","cafe y blanco","blanco","blanco","blanco","naranja"),
    
    
    
    tamañoG= c("grande","grande","pequeño","pequeño","grande","grande","pequeño","mediano","grande",
               "pequeño","pequeño","grande","mediano","grande","mediano","mediano"),
    
    formaS= c("convexo","hemisferico","globoso","globoso","globoso","hemisferico","convexo","embudo","mamelonado",
              "convexo","embudo","acampanado","globoso","convexo","convexo","convexo"),
    
    tamañoP= c("corto","alargado","corto","sinpie","alargado","alargado","alargado","alargado","alargado",
               "corto","alargado","corto","muy alargado","alargado","alargado","alargado"),
    
    grosorP= c("delgado","grueso","grueso","sin pie","grueso","delgado","delgado","grueso","delgado",
               "delgado","delgado","grueso","grueso","grueso","delgado","grueso")
  )
  
  # Codigo:
  # Vector indicador de variables usadas en el filtro:
  vuf <- !c(is.null(hname),is.null(nombrec), is.null(hcolor), is.null(tamañogeneral), is.null(formasombrero), is.null(tamañopie), is.null(grosorpie))
  
  # Si el usuario deja todos los argumentos en NULL, entonces
  # la funcion devuelve la tabla completa, de lo contrario
  # se ejecuta el filtro
  if(sum(vuf) == 0) {
    ho
  } else{
    # data.frame de una sola fila, con los valores de cada
    # variable usada en el filtro:
    txt <- data.frame(
      hongo = ifelse(is.null(hname), NA, hname),
      nCien = ifelse(is.null(nombrec), NA, nombrec),
      hcol = ifelse(is.null(hcolor), NA, hcolor),
      tamañoG = ifelse(is.null(tamañogeneral), NA, tamañogeneral),
      formaS = ifelse(is.null(formasombrero), NA, formasombrero),            
      tamañoP = ifelse(is.null(tamañopie), NA, tamañopie),
      grosorP = ifelse(is.null(grosorpie), NA, grosorpie)
      
    )
    
    # Se juntan los valores de las variables usados en el filtro:
    txt <- paste0(txt[1, vuf], collapse = "-")
    
    # Se agrega columna al data.frame de datos para juntar 
    # las columnas involucradas en el filtro:
    if(sum(vuf) == 1) ho$todo <- ho[, vuf] else ho$todo <- apply(ho[, vuf], 1, paste0, collapse = "-")
    
    # Se hace el filtro con la nueva columna "todo" y se imprime de inmediato
    nc <- ncol(ho)
    subset(ho, todo == txt, select = -nc)
  }
  
  #VALOR:
  
  #Esta funcion filtra y entrega la parte del data.frame o base de datos de hongos 
  #comestibles que el usuario desea guiado por la eleccion de caracteristicas.
  
  
  # Ejemplos de uso:
  
  # Con los valores por defecto se devulve el data.frame completo
  # fungi.ID()
  
  # Cuales tienen el rasgo hcolor = "blanco"
  # fungi.ID(hcolor = "blanco")
  
  # Cuales tienen el rasgo hcolor = "blanco" y formasombrero="convexo"
  # fungi.ID(hcolor="blanco",formasombrero="convexo")
  
  # Cuales tienen el rasgo nombrec = "Pleurotus ostreatus"
  # fungi.ID(nombrec = "Pleurotus ostreatus")
  
  # Cuales tienen el rasgo  grosorpie = "grueso", formasombrero = "globoso" y tamañopie = "corto"
  # fungi.ID(grosorpie = "grueso", formasombrero = "globoso", tamañopie = "corto")
  
  # Cuales tienen el rasgo hcol = "naranja" y tamañogeneral = "grande"
  # fungi.ID(hcol = "naranja", tamañogeneral = "grande")  # en este caso, ninguno
}


