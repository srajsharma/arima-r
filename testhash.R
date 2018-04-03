library(hash)


datos <- rnorm(1:10)
i<-9
var<-paste0("x",i)
h <- hash(var, datos[i])
h[paste0("x",i)]



assign(var, datos[i])
var
get(paste0("x",i))
