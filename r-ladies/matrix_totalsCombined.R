#Construyo vectores para la primer matriz
vector_1 <- c(1,2,3)
vector_2 <- c(4,5,6)
vector_3 <- c(7,8,9)

#Construyo vectores para la segunda matriz
vector_4 <- c(10, 11, 12)
vector_5 <- c(13, 14, 15)
vector_6 <- c(16, 17, 18)

#construyo matriz 1 y 2
matriz1 <- matrix(c(vector_1, vector_2, vector_3), nrow = 3, byrow = TRUE)
matriz2 <- matrix(c(vector_4, vector_5, vector_6), nrow = 3, byrow = TRUE)

#construyo vectores de nombres de filas y columnas
filas <- c("F1", "F2", "F3")
filas2 <- c("F4", "F5", "F6") #como voy a mergear necesito q los labels continuen
cols <- c("C1", "C2", "C3")

#asigno nombres a las filas y columnas 
rownames(matriz1) <- filas
colnames(matriz1) <- cols

rownames(matriz2) <- filas2
colnames(matriz2) <- cols

#mergeo las dos matrices
big_matrix <- rbind(matriz1, matriz2)

#calculo la suma de las columnas y filas
total_rows_big_matrix <- rowSums(big_matrix)
total_cols_big_matrix <- colSums(big_matrix)

#mergeo los totales de las filas como columnas
row_totals <- cbind(big_matrix, total_rows_big_matrix)

#mergeo los totales de las columnas como fila
col_totals <- rbind(big_matrix, total_cols_big_matrix)

#salidas
row_totals
col_totals

