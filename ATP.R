# Codigo:
ATP <- function(glu, lanz_malato = TRUE, inhibe_triosa = FALSE, inhibe_triosa2 = FALSE) {
  
  if(lanz_malato) {ATP <- glu * 38} else {ATP <- glu * 36}
  if(inhibe_triosa) {ATP <- glu * 18} else {ATP}
  if(inhibe_triosa2) {ATP <- glu * 16} else {ATP}
  
  paste0("El ATP producido es: " , ATP)
}
  
 