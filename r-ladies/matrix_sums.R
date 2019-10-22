#Construyo vectores
vector_1 <- c(1,2,3)
vector_2 <- c(4,5,6)
vector_3 <- c(7,8,9)

#construyo matriz
matrix_name <- matrix(c(vector_1, vector_2, vector_3), nrow = 3, byrow = TRUE)

#construyo vectores de nombres de filas y columnas
filas <- c("F1", "F2", "F3")
cols <- c("C1", "C2", "C3")

#asigno nombres a las filas y columnas
rownames(matrix_name) <- filas
colnames(matrix_name) <- cols

#sumo totales
row_sums <- rowSums(matrix_name)

#muestro row_sums que es valor obtenido de la suma de los valores de fila
row_sums