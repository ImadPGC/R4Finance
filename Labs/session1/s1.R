# Variable Basics --------------------------------------------------------

# Declarar una Variable y operaciones básicas

a <- 2

b <- 23

c <- a+b

a/b

a*b

# Crear un vector numérico c() y operaciones vectorizadas

d <- c(1,2,3,4,5,6,7,8)

d1 <- d

d2 <- c(1,2,3,4)

d + d1

d*d1

# Crear un vector de caracteres c()

f <- c("a", "b", "c")

# Crear un vector boolean c() y operaciones básicas

f1 <- c(TRUE,TRUE, FALSE, FALSE)

v_f <- a > 2

# Sumas con booleans

f1 + f1

sum(f1)

# Crear un vector combinado c()

f2 <- c(1, "TRUE", FALSE)
class(f2)

# Crear un vector numérico sando seq

f3 <- seq(from = 1, to = 100, by = 5)

f4 <- seq(from = 1, to = 100, length.out = 13)

dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-01-01"), by = "weeks")
class(dates)

# Data Frames Y Listas -------------------------------------------------------------

# Crear un data frame rnorm, rt, rexp

set.seed(666) # Semilla replicación

df <- data.frame( normales = rnorm(100, 0, 0.2),
                  t = rt(100, 3),
                  exponentials = rexp(100, 4.5)) # Ojo, todos del mismo tamaño

# Subseting vectores

a[1]

# Subsetting Data Frame

df[1,]
df[,1]
df[2,3]

colnames(df) <- c('normal','t_student', 'exp')

df
  
# Listas

l1 <- list(a = a, df = df, f1 = f1, f3 = f3)

# Subsetting Listas

l1[[1]]

l1[["a"]]

l1[[2]][3,2]

#li$
  

# For, If, functions ------------------------------------------------------

# Ciclos for, if

#Otra amnera de declarar una secuencia

x <- 1:6

for (i in 1:8) {
  
  if(i == 1){ 
  print(paste(1+i, "Hola"))
  
  }else{
    print(paste(1+i, "Adios"))
    
  }

  }

# Funciones

# Argumentos por nombre o por orden
imprimir_nombre <- function(nombre = "Warren buffet", semestre = 6){
  
  if( class(nombre) != "character"){
    
    # Programamos un error
    print("ERROR: Ingresa una String")
     
     } else {
    
        x <- semestre + 4
    
        return(paste("Me llamo ", nombre, " y me voy a graduar en ",x, " semestres" ))
      }
  
}

imprimir_nombre()

args(imprimir_nombre)

imprimir_nombre(nombre = "Donald Trump",
                semestre = 1)


imprimir_nombre(semestre=7, 
                nombre = 2)


# Reto
# Inserta una sección
# Desarrolla una función llamada VP que calcule el valor presente 
# Que tome un vector de flujos (anuales)
# Una tasa de interés (efectiva anual)
# Y devuelva el valor presente del vector de flujos
# La temporalidad es el largo del vector de flujos. Hint usa length()
