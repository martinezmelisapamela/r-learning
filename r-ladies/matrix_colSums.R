#Construyo vectores
vector_1 <- c(1,2,3)
vector_2 <- c(4,5,6)
vector_3 <- c(7,8,9)

#construyo matriz
matrix_name <- matrix(c(vector_1, vector_2, vector_3), nrow = 3, byrow = FALSE)

#construyo vectores de nombres de filas y columnas
filas <- c("F1", "F2", "F3")
cols <- c("C1", "C2", "C3")

#asigno nombres a las filas y columnas
rownames(matrix_name) <- filas
colnames(matrix_name) <- cols

#sumo totales de cada columna
col_sums <- colSums(matrix_name)

#mergeo el vector nuevo de totales como una fila mas de la matriz original
big_matrix <- rbind(matrix_name, col_sums)

#muestro big matrix
big_matrix