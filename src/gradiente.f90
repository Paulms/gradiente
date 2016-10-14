PROGRAM gradiente_conjugado
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Prueba del Algoritmo del Gradiente conjugado         !!
  !!                                                      !!
  !!                                                      !!
  !! Autor: Paul Mendez Silva                             !!
  !! e-mail: paul.mendez@udec.cl                          !!
  !! Fecha: 26/Septiembre/2016                            !!
  !!                                                      !!
  !! Version: 0.3                                         !!
  !! Ultima revision: 29/Septiembre/2016                  !!   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  USE morse                 ! Módulo define el tipo MSparse
  USE decimal               ! Define la precisión
  USE iterativos            ! Modulo con MGC
  USE datos

  IMPLICIT NONE
  ! Definimos variables a utilizar
  REAL(kind=dp), ALLOCATABLE    :: bb(:), xr(:), xo(:)
  CHARACTER(1024)               :: str
  TYPE (MSparse)                :: mat                  ! Almacena matriz en CRS
  INTEGER                       :: nn                   !dimensiones
  INTEGER                       :: itmax        ! Iteraciones maximas
  REAL(kind=dp)                 :: tol          ! tolerancia e iteraciones
  CHARACTER(32)                 :: file_name
  CHARACTER(32)                 :: output_name
  INTEGER                       :: ierr

  ! Primero leemos los datos
  file_name = "datos.dat"
  CALL archivo_corto(file_name, output_name, mat, bb, xo, tol, itmax, imprimir_datos=.TRUE.)
  ! GRADIENTE CONJUGADO
  nn = 0; nn = SIZE(bb)
  ALLOCATE(xr(nn)); xr = 0.0
  CALL MGC(mat, bb, xo, nn, tol, itmax, xr)
  CALL save_output(output_name, xr)
  PRINT*,' La solucion es = ', xr
  ! LIBERAMOS MEMORIA
  DEALLOCATE(bb,xo,xr,STAT=ierr)
  IF(ierr/=0) THEN
     PRINT*,'problemas liberando la memoria!! (Programa principal)'
     PRINT*,'ERROR 99, ABORT!!'
     STOP
  END IF

END PROGRAM gradiente_conjugado