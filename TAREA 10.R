# EJERCICIO 1: Identificar si los números son positivos, negativos o cero

numeros <- numeric(10)
for (i in 1:10) {
  numeros[i] <- as.numeric(readline(prompt = paste("Ingrese el número", i, ": ")))
}

for (num in numeros) {
  if (num > 0) {
    cat(num, "es positivo\n")
  } else if (num < 0) {
    cat(num, "es negativo\n")
  } else {
    cat(num, "es cero\n")
  }
}

# EJERCICIO 2: Clasificar números aleatorios como pares o impares

set.seed(Sys.time())
numeros_aleatorios <- sample(1:1000, 100)
clasificacion <- vector("character", length(numeros_aleatorios))
for (i in 1:length(numeros_aleatorios)) {
  if (numeros_aleatorios[i] %% 2 == 0) {
    clasificacion[i] <- "Par"
  } else {
    clasificacion[i] <- "Impar"
  }
}
resultados <- data.frame(Números = numeros_aleatorios, Clasificación = clasificacion)
print(resultados)
pares <- sum(clasificacion == "Par")
impares <- sum(clasificacion == "Impar")

cat("Cantidad de números pares:", pares, "\n")
cat("Cantidad de números impares:", impares, "\n")

# EJERCICIO 3: Suma de los primeros n números

# Versión 1
n <- 10
suma <- n * (n + 1) / 2
print(suma)

# Versión 2
n <- 10
suma <- 0
for (i in 1:n) {
  suma <- suma + i
}
print(suma)

# EJERCICIO 4: Generar una secuencia Fibonacci

n <- 20
secuencia <- numeric(n)
secuencia[1] <- 0
secuencia[2] <- 1

i <- 3
while (i <= n) {
  secuencia[i] <- secuencia[i - 1] + secuencia[i - 2]
  i <- i + 1
}
print(secuencia)

# EJERCICIO 5: Cálculo del factorial de un número

num = as.integer(readline(prompt = "Ingrese número: "))
factorial = 1
if (num < 0) {
  print("Es un número negativo")
} else if (num == 0) {
  print("El factorial de 0 es 1")
} else {
  for (i in 1:num) {
    factorial = factorial * i
  }
  print(paste("El factorial del:", num, "es", factorial))
}

# EJERCICIO 6: Adivinar el número secreto

set.seed(Sys.time())
numero_secreto <- sample(1:100, 1)
intentos <- 0
adivinado <- FALSE

cat("Adivina el número entre 1 y 100.\n")

while (adivinado == FALSE) {
  cat("Ingresa tu número: ")
  intento <- as.numeric(readLines(con = stdin(), n = 1))
  intentos <- intentos + 1
  
  if (is.na(intento)) {
    cat("Por favor, ingresa un número válido.\n")
  } else if (intento < 1 || intento > 100) {
    cat("El número debe estar entre 1 y 100. Intenta de nuevo.\n")
  } else if (intento < numero_secreto) {
    cat("El número ingresado es menor que el número secreto.\n")
  } else if (intento > numero_secreto) {
    cat("El número ingresado es mayor que el número secreto.\n")
  } else {
    cat("¡Felicidades! Adivinaste el número en", intentos, "intentos.\n")
    adivinado <- TRUE
  }
}

# EJERCICIO 7: Verificación de contraseña con número limitado de intentos

contraseña_correcta <- "anyi123"
intentos <- 0
max_intentos <- 3

while (intentos < max_intentos) {
  contraseña_ingresada <- readline(prompt = "Ingrese la contraseña: ")
  
  if (contraseña_ingresada == contraseña_correcta) {
    cat("Acceso concedido.\n")
    break
  } else {
    intentos <- intentos + 1
    cat("Contraseña incorrecta. Intento", intentos, "de", max_intentos, ".\n")
  }
  if (intentos == max_intentos) {
    cat("Número máximo de intentos alcanzado. Usuario bloqueado.\n")
  }
}

# EJERCICIO 8: Suma de los dígitos de un número

numero <- as.integer(readline(prompt = "Ingrese un número entero: "))
suma <- 0

while (numero > 0) {
  suma <- suma + (numero %% 10)
  numero <- numero %/% 10
}

cat("La suma de los dígitos es:", suma, "\n")

# EJERCICIO 9: Imprimir un triángulo de asteriscos

filas <- 10
for (i in 1:filas) {
  cat(rep(" ", filas - i), sep = "")
  cat(rep("*", 2 * i - 1), sep = "")
  cat("\n")
}

# EJERCICIO 10: Verificar números primos entre 1 y 100

for (num in 1:100) {
  es_primo <- TRUE
  
  if (num < 2) {
    es_primo <- FALSE
  } else {
    for (i in 2:sqrt(num)) {
      if (num %% i == 0) {
        es_primo <- FALSE
        break
      }
    }
  }
  
  if (es_primo) {
    cat(num, "es primo\n")
  } else {
    cat(num, "no es primo\n")
  }
}
