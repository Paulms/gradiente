MODULE datos
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Modulo para lectura de datos en diferentes formatos  !!
  !!                                                      !!
  !! METODOS       :                                      !!
  !! archivo_corto : problema en un solo archivo          !!
  !! linea_a_vector: crea vectores usando cadenas de texto!!
  !!                                                      !!
  !! Autor: Paul Mendez Silva                             !!
  !! e-mail: paul.mendez@udec.cl                          !!
  !! Fecha: 29/Septiembre/2016                            !!
  !!                                                      !!
  !! Version: 0.1                                         !!
  !! Ultima revision: 29/Septiembre/2016                  !!   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  USE decimal
  USE morse
  IMPLICIT NONE
  PUBLIC  :: archivo_corto
  PRIVATE :: linea_a_vector
  CONTAINS
  SUBROUTINE archivo_corto (file_name, mat, bb, xo, tol, itmax, imprimir_datos)
    !======================================================
    ! Leemos datos de un problema lineal almacenados
    ! en un solo archivo
    ! Variables:
    !   file_name: Nombre del archivo
    !   tol: tolerancia
    !   itmax: número máximo de iteraciones
    !   xo: vector con aprox. inicial
    !   bb: vector derecho del sistema
    !   mat: Matriz de tipo MSparse
    !   imprimir_datos: si se desea ver los datos leidos
    !====================================================== 
    CHARACTER(32), INTENT(in)   ::  file_name     ! archivo
    REAL(kind=dp), ALLOCATABLE    :: AA(:)
    INTEGER, ALLOCATABLE          :: rows(:), columns(:)  ! matriz CRS
    REAL(kind=dp), ALLOCATABLE, INTENT(out) :: bb(:), xo(:)
    CHARACTER(1024)               :: str         
    TYPE (MSparse), INTENT(out)   :: mat          ! Almacena matriz en CRS
    INTEGER                       :: nn           ! dimensiones
    INTEGER, INTENT(out)          :: itmax        ! Iteraciones maximas
    REAL(kind=dp), INTENT(out)    :: tol          ! tolerancia e iteraciones
    LOGICAL, OPTIONAL             :: imprimir_datos
    LOGICAL                       :: mensajes
    IF (present(imprimir_datos)) THEN
      mensajes=imprimir_datos
    ELSE
      mensajes = .FALSE.
    END IF
    IF (mensajes) PRINT *, "Datos leidos: "
    OPEN(10,file=file_name, status='old', action='read')
      READ(10,*) tol
      IF (mensajes) PRINT *, "Tolerancia: ", tol
      READ(10,*) itmax
      IF (mensajes) PRINT *, "Iteraciones máximas: ", itmax
      ! xo (punto inicial para iteracion)
      READ(10,"(A)") str
      xo = linea_a_vector(str, " ")
      IF (mensajes) PRINT *, "xo: ", xo
      ! Vector b
      READ(10,"(A)") str
      bb = linea_a_vector(str, " ")
      IF (mensajes) PRINT *, "bb: ", bb
      ! Matriz en Morse  
      READ(10,"(A)") str
      AA = linea_a_vector(str, " ")
      READ(10,"(A)") str
      rows = linea_a_vector(str, " ")
      READ(10,"(A)") str
      columns = linea_a_vector(str, " ")
    CLOSE(10)
    ! Creamos matriz morse
    mat = MSparse(AA, rows, columns)
    IF (mensajes) CALL print_flat(mat)
  END SUBROUTINE archivo_corto

  FUNCTION linea_a_vector(str, sep) result(vector)
    !======================================================
    ! convierte una cadena de texto en un vector
    ! Variables:
    !   str: cadena de texto
    !   sep: caracter de separacion de datos
    ! Salida:
    !   vector: vector de números reales
    !====================================================== 
    REAL(kind=dp), ALLOCATABLE  :: vector(:)
    CHARACTER(*)                :: str
    CHARACTER                   :: sep
    INTEGER                     :: i, n_sep
    ! Determinamos el tamaño del vector por el # de separadores
    n_sep = COUNT((/ (str( i:i ), i = 1, LEN(TRIM(str))) /) == sep)
    ! Reservamos memoria y leemos el vector
    ALLOCATE(vector(n_sep+1))
    READ(str,*) vector
  END FUNCTION
END MODULE