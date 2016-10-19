MODULE morse
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Modulo que define matrices almacenadas en formato    !!
  !! CRS                                                  !!
  !!                                                      !!
  !! PROCEDIMIENTOS:                                      !!
  !!                                                      !!
  !!   init_matriz    = inicializa la matriz CRS a partir !!
  !!                    de un array 2D                    !!
  !!   print_flat    = imprime la estructura interna de la!!
  !!                    matriz almacenada                 !!
  !!  buscar_elementos = dado dos enteros i,j devuelve    !!
  !!                      A(i,j)                          !!
  !!  mat_vector = dado un vector x realiza la            !!
  !!                multiplicación con la matriz Ax       !!
  !!                                                      !!
  !! Autor: Paul Mendez Silva                             !!
  !! e-mail: paul.mendez@udec.cl                          !!
  !! Fecha: 11/Septiembre/2016                            !!
  !!                                                      !!
  !! Version: 0.3                                         !!
  !! Ultima revision: 14/Octubre/2016                     !!   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  USE decimal
  IMPLICIT NONE
  PUBLIC  :: MSparse, print_flat

  TYPE MSparse
    ! Tipo para almacenar matrices en formato CRS
    INTEGER                     :: nn, nzero
    REAL (kind=dp), ALLOCATABLE :: valores(:)             ! AA
    INTEGER, ALLOCATABLE        :: filas(:), columnas(:)  ! rows columns
  CONTAINS
    PROCEDURE :: ix => buscar_elementos
  END TYPE MSparse

  INTERFACE MSparse
    MODULE PROCEDURE init_matriz
  END INTERFACE

  INTERFACE OPERATOR (*)
    MODULE PROCEDURE mat_vector
  END INTERFACE

CONTAINS
  FUNCTION init_matriz(matriz, nn)
    !======================================================
    ! Función para inicilizar matriz CRS con un array 2D
    ! Variables:
    !   matriz  = array 2D
    !   nn =      dimensiones de la matriz (nn x nn)
    !======================================================
    TYPE(MSparse) init_matriz
    INTEGER, INTENT(in)         :: nn               ! Dimensiones de la matriz
    REAL (kind=dp), INTENT(in)  :: matriz(nn,nn)    ! Almacena la matriz
    INTEGER                     :: nzero            ! Elementos diferentes de cero
    INTEGER col, row, element                       ! índices para procesar la matriz
    REAL (kind=dp), PARAMETER   :: eps = 1e-8       ! tolerancia para comparacion
    ! Inicializamos VARIABLES
    col = 0; row = 0; element = 0
    ! Contamos elementos diferentes a cero en la matriz
    nzero = COUNT(ABS(matriz) > eps)
    init_matriz%nn = nn
    init_matriz%nzero = nzero
    ! Reservamos memoria
    ALLOCATE(init_matriz%filas(nn+1),init_matriz%columnas(nzero), init_matriz%valores(nzero))

    ! Almacenamos primera entrada de filas
    init_matriz%filas(1) = 1
    ! Ahora procesamos la matriz elemento por elemento
    ! Avanzamos por columnas y luego por filas
    DO row = 1, nn
      DO col = 1, nn
        IF (ABS(matriz(col, row)) > eps) THEN
          element = element  + 1
          init_matriz%valores(element) = matriz(col, row)
          init_matriz%columnas(element) = col
        END IF
      END DO
      init_matriz%filas(row+1) = element + 1
    END DO
  END FUNCTION

  SUBROUTINE print_flat(this)
    !========================================================
    ! Rutina para imprimir la estructura interna de la matriz
    !========================================================
    TYPE(MSparse),  INTENT(in) :: this
    PRINT *, 'valores = ', this%valores
    PRINT *, 'filas = ', this%filas
    PRINT *, 'columnas = ', this%columnas
  END SUBROUTINE print_flat

  FUNCTION buscar_elementos(this, fila, columna) result(elemento)
    !========================================================
    ! Esta función busca el valor A(i,j) dado un par de enteros
    ! Variables:
    !   fila: fila i a buscar
    !   columna: columna j a buscar
    !========================================================
    CLASS(MSparse), INTENT(in)  :: this               ! Almacena la matriz
    REAL (kind=dp)              :: elemento           ! Elemento A(i,j)
    INTEGER                     :: fila, columna, ii  ! indices i, j e iteradores
    ! Validar los indices i, j
    IF (fila < 1 .OR. columna < 1 .or. fila > this%nn .or. columna > this%nn) THEN
      PRINT *, "Error: Índices deben ser mayores a 0 o menores a", this%nn
      STOP
    ELSE
      elemento = 0 ! Asumimos que el espacio está vacio                                   
      DO ii = this%filas(fila), (this%filas(fila+1)-1)
        ! Buscamos si el espacio no está vacio
        IF (this%columnas(ii) == columna) THEN
          ! Si no está vacio reemplazamos valor        
          elemento = this%valores(ii)
          EXIT                 
        END IF
      END DO
    END IF
  END FUNCTION buscar_elementos

  FUNCTION mat_vector(this, vector) RESULT(resultado)
    !========================================================
    ! Esta función permite multiplicar la matriz CRS por un vector
    ! Variables:
    !     vector: vector x para realizar el producto Ax
    !     nn:     dimensiones del vector x
    !========================================================
    TYPE (MSparse), INTENT(in)    :: this           ! almacena la matriz
    REAL (kind=dp), INTENT(in)    :: vector (:)    ! Vector de entrada
    REAL (kind=dp)                :: resultado (this%nn) ! resultado de la multiplicación
    INTEGER                       :: ii, jj         ! variables para iterar
    ! Chequeamos si las dimensiones son compatibles
    IF (this%nn /= SIZE(vector)) THEN
      PRINT *, "Error: Dimensiones no compatibles: matriz: ", this%nn, "Vector: ", SIZE(vector)
      STOP
    END IF
    ! Inicializamos variables
    resultado = 0
    ! Realizamos la multiplicación
    DO ii = 1, this%nn
      DO jj = this%filas(ii), (this%filas(ii+1)-1)
        resultado(ii) = resultado(ii) + this%valores(jj)*vector(this%columnas(jj))
      END DO
    END DO
  END FUNCTION
END MODULE morse