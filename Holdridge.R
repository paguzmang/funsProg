#Clasificación bioclimática de Holdridge#
#https://biogeografia.net/bioclima06e.html#
#Laura Betancour & Camilo Arango#
#Sin Tildes#

Holdridge <- function(Tm,Pm,Lat){
  #Entradas----
  #Tm = Temperatura media o Temperatura anual en °C (Grados Celsius)
  #Pm = Precipitación media anual en mm (Milimetros)
  #Lat = Latitud, distancia de la linea del ecuador en grados latitudinales
  
  #Calculos----
  BioT <- Tm - (3*(Lat/100)*(Tm - 24)^2) #Calculo de la Biotemperatura
  ETP <- (BioT * 58.93)/Pm #Calculo de Evapotranspiración Potencial
  
  #Respuesta----
  #Arrojar la zona de vida o zonas transición con su respectiva region latitudinal y piso altitudinal
  if(ETP>=0.5 & ETP<1 & Pm>=62.5 & Pm<125){x <- "Desierto Polar Nival"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=125 & Pm<250){x <- "Desierto Polar Nival"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=250 & Pm< 500){x <- "Desierto Polar Nival"}
  
  if(ETP>=1 & ETP<2 & Pm>=62.5 & Pm<125){x <- "Tundra Seca Subpolar Alpina"}
  if(ETP>=0.5 & ETP<1 & Pm>=125 & Pm<250){x <- "Tundra Humeda Subpolar Alpina"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=250 & Pm<500){x <- "Tundra muy Humeda Subpolar Alpina"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=500 & Pm<1000){x <- "Tundra Pluvial Subpolar Alpina"}
  
  if(ETP>=2 & ETP<4 & Pm>=62.5 & Pm<125){x <- "Desierto Boreal Subalpino"}
  if(ETP>=1 & ETP<2 & Pm>=125 & Pm<250){x <- "Matorral Desertico Boreal Subalpino"}
  if(ETP>=0.5 & ETP<1 & Pm>=250 & Pm<500){x <- "Bosque Humedo Boreal Subalpino"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=500 & Pm<1000){x <- "Bosque muy Humedo o Paramo Boreal Subalpino"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=1000 & Pm<2000){x <- "Bosque Pluvial o Paramo Pluvial Boreal Subalpino"}
  
  if(ETP>=4 & ETP<8 & Pm>=62.5 & Pm<125){x <- "Desierto Templado Montano"}
  if(ETP>=2 & ETP<4 & Pm>=125 & Pm<250){x <- "Matorral Desertico Templado Montano"}
  if(ETP>=1 & ETP<2 & Pm>=250 & Pm<500){x <- "Estepa Templada Montana"}
  if(ETP>=0.5 & ETP<1 & Pm>=500 & Pm<1000){x <- "Bosque Humedo Templado Montano"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=1000 & Pm<2000){x <- "Bosque muy Humedo Templado Montano"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=2000 & Pm<4000){x <- "Bosque Pluvial Templado Montano"}
  
  if(ETP>=8 & ETP<16 & Pm>=62.5 & Pm<125){x <- "Desierto Subtropical Premontano"}
  if(ETP>=4 & ETP<8 & Pm>=125 & Pm<250){x <- "Matorral Desertico Subtropical Premontano"}
  if(ETP>=2 & ETP<4 & Pm>=250 & Pm<500){x <- "Monte Espinoso Subtropical Premontano"}
  if(ETP>=1 & ETP<2 & Pm>=500 & Pm<1000){x <- "Bosque Seco Subtropical Premontano"}
  if(ETP>=0.5 & ETP<1 & Pm>=1000 & Pm<2000){x <- "Bosque Humedo Subtropical Premontano"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=2000 & Pm<4000){x <- "Bosque muy Humedo Subtropical Premontano"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=4000 & Pm<8000){x <- "Bosque Pluvial Subtropical Premontano"}
  
  if(ETP>=16 & ETP<=32 & Pm>=62.5 & Pm<125){x <- "Desierto Tropical Basal"}
  if(ETP>=8 & ETP<16 & Pm>=125 & Pm<250){x <- "Matorral Desertico Tropical Basal"}
  if(ETP>=4 & ETP<8 & Pm>=250 & Pm<500){x <- "Monte Espinoso Tropical Basal"}
  if(ETP>=2 & ETP<4 & Pm>=500 & Pm<1000){x <- "Bosque muy Seco Tropical Basal"}
  if(ETP>=1 & ETP<2 & Pm>=1000 & Pm<2000){x <- "Bosque Seco Tropical Basal"}
  if(ETP>=0.5 & ETP<1 & Pm>=2000 & Pm<4000){x <- "Bosque Humedo Tropical Basal"}
  if(ETP>=0.25 & ETP<0.5 & Pm>=4000 & Pm<8000){x <- "Bosque muy Humedo Tropical Basal"}
  if(ETP>=0.125 & ETP<0.25 & Pm>=8000 & Pm<=16000){x <- "Bosque Pluvial Tropical Basal"}
  
  paste0("La zona de vida es: ",x)
}