RanasCol<- function(DD=T, MI, DT, CII, CID, ED, PD, DI, MII, DN, TDC, GP, TS) {
  
  #Fecha: 10/mayo/23
  #Autor: Sara Campiño Ospina
  #Descripción: Identificación de ranas presentes en Colombia, hasta la categoria de familia; a través de carcateres 
  #morfologicos. 
  
  #Argumentos:
  #DD: Argumento logico de presencia (T) o ausencia (F) de discos  digitales. Por defecto esta en T, dado que de las 14 familias, 8 tienen presencia de escudete dermico. 
  #MI: Argumento logico de presencia (T) o ausencia (F) de membranas interdigitales.
  #DT: Argumento logico de presencia (T) o ausencia (F) de discos  truncados.
  #CII: Argumento logico de presencia (T) o ausencia (F) del cartilago intercalar inferior ubicado en las patas delanteras de la rana. 
  #CID: Argumento logico de presencia (T) o ausencia (F) del cartilago intercalar Distal ubicado en las patas delanteras de la rana.
  #ED: Argumento logico de presencia (T) o ausencia (F) de escudetes dermicos.
  #PD: Argumento logico de presencia (T) o ausencia (F) de papila digital.
  #DI: Argumento logico de presencia (T) o ausencia (F) del dedo 3 pedial mas largo que el dedo 5 pedial.
  #MII:Argumento logico de presencia (T) o ausencia (F) de membranas interdigitales inferiores.
  #DN: Argumento logico de presencia (T) o ausencia (F) de diadema nucal.
  #TDC: Argumento logico de  Presencia (T) o ausencia (F) de la terminación de la uña en cruz
  #GP: Argumentologicos de presencia (T) o ausencia (F) de la glandula parotida
  #TS: Argumento logico de presencia (T) o ausencia (F) de tuberculo supraciliar. 
  
  
  #Calculos 

  # Condicional para identificar la familia de la rana
  if (DD==T & !MI & !DT & !CII & !CID & ED==T & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Aromobatidae o Dendobatidae"
    print(paste("la Rana pertenece a la familia:",familia,".Los Aromobátidos son especies de coloración criptica y de tamaño pequeño, diurnas, conocidas de los ecosistemas asociados a bosques secos o húmedos en Colombia, su mayor diversidad se concentra en las tierras bajas. En colombia actualmente hay 17 especies, distribuidas en tres generos. Y Las ranas venenosas de la familia Dendrobatidae se distribuyen en casi todos los ambientes, pero su mayor riqueza se conoce en los bosques de niebla y en los bosques húmedos tropicales de la región amazónica y el Pacifico de Colombia; En colombia actualmente hay 96 especies, distribuidas en 12 generos.En este grupo podemos encontrar la rana mas toxica del mundo Phyllobates terribilis. Estas dos familias pertenecen a la super familia: Dendrobatoidea.") )
    
  } else if (DD==T & MI==T & DT==T & !CII & !CID & !ED & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Centrolenidae"
    print(paste0("la Rana pertenece a la familia:",familia,".Las conocidas ranas de crsital estan asociadas a los cuerpos de agua de curso rápido que incluye pequeños riachuelos y quebradas, siempre cubiertos de vegetación. Sus distribuciones abarcan desde los ambientes de páramo hasta las áreas selváticas de las tierras bajas. En Colombia actualmente hay 81 especies, distribuidas en 10 generos.") )
    
  } else if (DD==T & !MI & !DT & !CII & !CID & !ED & !PD & DI==T & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Craugastoridae"
    print(paste0("la Rana pertenece a la familia:",familia,". Esta familia de ranas es uno de los grupos de vertebrados terrestres más diversificado que incluye las ranas de desarrollo directo, cuya reproducción no requiere de cuerpos de agua. Este aspecto biológico le permite ocupar ambientes como los páramos, bosques altoandinos hasta los bosques húmedos tropicales. En Colombia existen actualmente 6 especies de esta familia, distribuidas en un genero") )
    
  } else if (DD==T & MI==T & !DT & CII == T & !CID &!ED & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- " Hemiphractidae"
    print(paste0("la Rana pertenece a la familia:",familia,". La familia de ranas marsupiales Hemiphractidae es uno de los grupos biológicos más especializados y a su vez uno de los más sensibles debido a su clara asociación a los ambientes que ocupan, desde los bosques húmedos tropicales hasta los páramos. En Colombia actualmente hay 29 especies, distribuidas en 4 generos.") )
    
  } else if (DD==T & !MI & !DT & !CII & !CID & !ED & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Strabomantidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Las ranas de esta familia agrupan el mayor grupo de especies de anfibios conocidos para el territorio Colombiano con 262 especies distribuidas 4 subfamilias y 7 géneros que incluyen cerca del 30% de las especies.") )
    
  } else if (DD==T & !MI & !DT & !CII & !CID & !ED & PD==T & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Eleutherodactylidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Esta familia de ranas se caracteriza por tener una papilas digitales en sus patas. En colombia actualmente hay 9 especies, distribuidas en 3 generos; donde el genero Diasporus es quien representa una mayor riqueza.") )
    
  } else if (DD==T & MI==T & !DT & !CII & CID==T & !ED & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Hylidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Los Hylidos son  Considerados una de las familias más diversificadas que ocupa todos los ambientes desde áreas subxerofíticas hasta los páramos. En colombia actualmente hay 146 especies, distribuidas en 18 generos. A esta familia pertenece la reconocida Phyllomedusa venusta. ") )
    
  } else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & !MII & !DN & !TDC & !GP & !TS) {
    familia <- "Leptodactylidae"
    print(paste0("la Rana pertenece a la familia:",familia,". La familia Leptodactylidae es una de las más diversificadas, la actual distribución de sus integrantes lo enmarcan en un grupo que es eminentemente asociado a las tierras bajas con algunas excepciones particulares. Actualmente en colombia hay 39 especies, distribuidas en 9 generos. ") )
    
  } else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & MII==T & !DN & !TDC & !GP & !TS) {
    familia <- "Ranidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Las ranas verdaderas, estas estan constituidas en colombia  por dos especies autóctonas y una introducida (Lithobates catesbeianus); las especies nativas se distribuyen en áreas  boscosas en quebradas de curso lento en las tierras bajas. Por otra parte la especie introducida utiliza cuerpos de agua loticos. Hay tres especies, en un solo genero.") )
    
  } else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & !MII & !DN & !TDC & GP==T & !TS) {
    familia <- "Bufonidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Los Sapos abarcan los ecosistemas de páramo desde los 4000 metros de altura hasta las tierras bajas que ocupan ambientes desérticos y selva tropical. Actualmente en Colombia hay 85 especies, distribuidos en 6 generos. En esta familia encontramos al sapo común Rhinella horribilis.") )
    
  } else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & MII==T & !DN & TDC==T & !GP & !TS) {
    familia <- "Pipidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Las ranas acuáticas de la familia Pipidae están constituidas en Colombia por cuatro especies nativas del género Pipa, todas distribuidas en las tierras bajas y una del género Xenopus posiblemente introducida.") )
    
  }else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & MII==T & DN==T & !TDC & !GP & !TS) {
    familia <- "Microhylidae"
    print(paste0("la Rana pertenece a la familia:",familia,". Las ranas cavadoras de la familia Microhylidae, están representadas en Colombia por un diversificado grupo de ranas que poseen la cabeza reducida cuyas distribuciones están asociadas a diversos ecosistemas en las tierras bajas por debajo de los 1700 de altitud. En Colombia hay 18 especies, distribuidas en 6 familias.") )
    
  }else if (!DD & !MI & !DT & !CII & !CID & !ED & !PD & !DI & !MII & !DN & !TDC & !GP & TS==T) {
    familia <- "Ceratophrydae"
    print(paste0("la Rana pertenece a la familia:",familia,". Las ranas cavadoras de la familia Ceratophrydae se distribuyen en las tierras bajas, las dos especies conocidas para Colombia poseen distribución ecológica disyunta dado que una de ellas ocupa los bosques secos y planicies de la región Caribe mientras que la segunda es propia de los bosques húmedos tropicales de la amazonia.  En esta familia encontramos a la rana pacman: Ceratophrys calcarata.") )
  }
  
  #Valor: La función entrega un vector con el dato de la familia, y adicionalmente un dato de texto sobre algunas caracteristicas pricipales a cerca de la familia. 
  
  #Ejemplos de uso:
  
  #1.Suponga que tiene una rana con las siguientes caracteristicas: Presencia de discos digitales y estos son truncados, ademas tiene presencia de membranas interdigitales. 
  #Y se observa que todas la otras caracteristicas requeridas no las presenta.
  
  #RanasCol(DD=T, MI=T, DT=T, CII=F, CID=F, ED=F, PD=F, DI=F, MII=F, DN=F, TDC=F, GP=F, TS=F)
  
  #2.Suponga que tiene una rana con las siguientes caracteristicas: Ausencia de discos digitales, presencia de membranas interdigitales inferiores y en los dedos pediales el individuo presenta una terminacion en uña con forma de cruz.  
  #Y se observa que todas la otras caracteristicas requeridas no las presenta.
  
  #RanasCol(DD=F, MI=F, DT=F, CII=F, CID=F, ED=F, PD=F, DI=F, MII=T, DN=F, TDC=T, GP=F, TS=F)
}


