# Variables Basics --------------------------------------------------------

# Declara una Variable

a <- 2

b <- 23

c <- a+b

a/b

a*b

# Crear un vector numÃ©rico c()

d <- c(1,2,3,4,5,6,7,8)

d1 <- d

d2 <- c(1,2,3,4)

d + d1

d*d1

# Crear un vector de caracteres c()

f <- c("a", "b", "c")

# Crear un vector boolean c()

f1 <- c(TRUE,TRUE, FALSE, FALSE)

v_f <- a > 2

# Sumas con booleans

f1 + f1

sum(f1)

# Crear un vector combinado c()

f2 <- c(1, "TRUE", FALSE)

# Crear un vector numÃ©rico sando seq

f3 <- seq(from = 1, to = 100, by = 5)

f4 <- seq(from = 1, to = 100, length.out = 13)


# Data Frames Y Listas -------------------------------------------------------------

# Crear un data frame rnorm, rt, rexp

set.seed(666) # Semilla replicaciÃ³n

df <- data.frame( normales = rnorm(100, 0, 0.2),
                  t = rt(100, 3),
                  exponentials = rexp(100, 4.5)) # Ojo, todos del mismo tamaÃ±o

# Subseting vectores

a[1]

# Subsetting Data Frame

df[1,]
df[,1]
df[2,3]

colnames(df)

#df$
  
# Listas

l1 <- list(a = a, df = df, f1 = f1, f3 = f3)

# Subsetting Listas

l1[[1]]

l1[["a"]]

l1[[2]][3,2]

#li$
  

# For, If, functions ------------------------------------------------------

# Ciclos for, if
  
for (i in 1:3) {
  
  if(i == 1){
    
    print(paste("Arriba Markovitz",i))
 
  } else {
    
      print(paste("Ganarle al mercado es una observaciÃ³n de una v.a",i))
    }
  
}


# Funciones

imprimir_nombre <- function(nombre = "DirecciÃ³n financiera", semestre = 6){
  
  if( class(nombre) != "character"){
    
    print("Ingresa una String")
     
     } else {
    
        x <- semestre + 4
    
        return(paste("Me llamo ", nombre, " y me voy a graduar en ",x, " semestres" ))
      }
  
}

imprimir_nombre()

args(imprimir_nombre)

imprimir_nombre(nombre = "Juliana Gudiñoo",
                semestre = 1)


imprimir_nombre(semestre=7, 
                nombre = 2)




# Reto
# Inserta una seccióm
# Desarrolla una función llamada VP que calcule el valor presente 
# Que tome un vector de flujos (anuales)
# Una tasa de interés (efectiva anual)
# Y devuelva el valor presente del vecto de flujos
