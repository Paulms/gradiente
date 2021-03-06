PROGRAM gradiente_conjugado
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Prueba del Algoritmo del Gradiente conjugado         !!
  !!                                                      !!
  !!                                                      !!
  !! Autor: Paul Mendez Silva                             !!
  !! e-mail: paul.mendez@udec.cl                          !!
  !! Fecha: 26/Septiembre/2016                            !!
  !!                                                      !!
  !! Version: 0.4                                         !!
  !! Ultima revision: 14/Octubre/2016                     !!   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  USE morse                 ! Módulo define el tipo MSparse
  USE decimal               ! Define la precisión
  USE iterativos            ! Modulo con MGC
  USE datos                 ! Modulo para leer datos

  IMPLICIT NONE
  ! Definimos variables a utilizar
  REAL(kind=dp), ALLOCATABLE    :: bb(:), xr(:), xo(:)
  TYPE (MSparse)                :: mat          ! Almacena matriz en CRS
  INTEGER                       :: nn           ! dimensiones
  INTEGER                       :: itmax        ! Iteraciones maximas
  REAL(kind=dp)                 :: tol          ! tolerancia e iteraciones
  CHARACTER(32)                 :: file_name    ! Archivo de datos
  CHARACTER(32)                 :: output_name  ! Archivo para salida
  INTEGER                       :: ierr         ! Error memoria

  ! Primero leemos los datos
  file_name = "datos.dat"
  CALL leer_archivo(file_name, output_name, mat, bb, xo, tol, itmax, imprimir_datos=.TRUE.)
  ! Llamamos al método del gradiente conjugado
  nn = 0; nn = SIZE(bb)
  ALLOCATE(xr(nn)); xr = 0.0
  CALL MGC(mat, bb, xo, tol, itmax, xr)
  ! Guardamos el resultado
  CALL save_output(output_name, xr)
  ! Liberamos memoria
  DEALLOCATE(bb,xo,xr,STAT=ierr)
  IF(ierr/=0) THEN
     PRINT*,'problemas liberando la memoria!! (Programa principal)'
     STOP
  END IF

END PROGRAM gradiente_conjugado