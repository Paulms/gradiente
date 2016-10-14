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
  SUBROUTINE archivo_corto (file_name, output_name, mat, bb, xo, tol, itmax, imprimir_datos)
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
    CHARACTER(32), INTENT(out)   ::  output_name     ! archivo
    REAL(kind=dp), ALLOCATABLE    :: AA(:)
    INTEGER, ALLOCATABLE          :: rows(:), columns(:)  ! matriz CRS
    REAL(kind=dp), ALLOCATABLE, INTENT(out) :: bb(:), xo(:)
    CHARACTER(1024)               :: str         
    TYPE (MSparse), INTENT(out)   :: mat          ! Almacena matriz en CRS
    INTEGER                       :: nn, nzero           ! dimensiones
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
      READ(10,*) output_name
      READ(10,*) tol
      IF (mensajes) PRINT *, "Tolerancia: ", tol
      READ(10,*) itmax
      IF (mensajes) PRINT *, "Iteraciones máximas: ", itmax
      READ(10,*) nn
      IF (mensajes) PRINT *, "dimensión del sistema: ", nn
      ! xo (punto inicial para iteracion)
      xo = leer_vector(10, nn)
      IF (mensajes) PRINT *, "xo: ", xo
      ! Vector b
      bb = leer_vector(10, nn)
      IF (mensajes) PRINT *, "bb: ", bb
      ! Matriz en morse
      READ(10,*) nzero
      IF (mensajes) PRINT *, "nzero: ", nzero
      AA = leer_vector(10, nzero)
      rows = leer_vector(10, nn+1)
      columns = leer_vector(10, nzero)
    CLOSE(10)
    ! Creamos matriz morse
    mat = MSparse(nn, nzero, AA, rows, columns)
    IF (mensajes) CALL print_flat(mat)
  END SUBROUTINE archivo_corto

  SUBROUTINE save_output(file_name, xi)
    !======================================================
    ! Almacena un vector en archivo file_name
    !====================================================== 
    CHARACTER(32), INTENT(in)              ::  file_name  ! archivo
    REAL(kind=dp), ALLOCATABLE, INTENT(in) :: xi(:)       ! vector
    INTEGER                                :: i, nn 
    nn = SIZE(xi) 
    OPEN(unit=20,file=file_name,status='replace',action='write')
    DO i = 1,nn
       WRITE(20,*) xi(i)
    END DO
    CLOSE(20)
  END SUBROUTINE

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

  FUNCTION leer_vector (fileid, dims) result(vector)
    !======================================================
    ! Inicializa vector dimensiones dims desde archivo
    ! fileid
    ! Salida:
    !   vector: vector de números reales
    !====================================================== 
    INTEGER       :: fileid
    INTEGER       :: dims
    REAL(kind=dp) :: vector(dims)
    READ(fileid,*) vector
  END FUNCTION
END MODULE