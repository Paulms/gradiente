MODULE iterativos
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Modulo que define métodos iterativos para resolver   !!
  !! sistemas lineales                                    !!
  !!                                                      !!
  !! METODOS       :                                      !!
  !!                                                      !!
  !! MGC: Método del Gradiente conjugado                  !!
  !! norma2: norma 2 de un vector                         !!
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
  PUBLIC  :: MGC
  PRIVATE :: norma2
CONTAINS
SUBROUTINE MGC (AA, bb, xo, nn, tol, itmax, xr)
    !======================================================
    ! Método del gradiente conjugado para solución de
    ! sistemas lineales
    ! Variables:
    !   AA: Matriz de tipo MSparse
    !   bb: vector derecho del sistema
    !   xo: vector con aprox. Inicializamos
    !   nn: dimensiones del sistema
    !   tol: tolerancia
    !   itmax: número máximo de iteraciones
    !   xr: vector donde se almacenará la respuesta.
    !====================================================== 
    TYPE (MSparse)              :: AA
    REAL(kind=dp), INTENT(in)   :: bb(:), tol
    REAL(kind=dp)               :: xo(:), xr(:), toladj
    REAL(kind=dp)               :: alpha, beta
    INTEGER                     :: itmax, nn, i
    REAL(kind=dp), ALLOCATABLE  :: rr(:), pp(:), ra(:), Apk(:)
    ! Inicializamos variables
    ALLOCATE (rr(nn), pp(nn), ra(nn), Apk(nn))
    rr = 0.0; ra = 0.0; pp = 0.0; alpha = 0.0; Apk = 0.0
    rr = bb - AA%dot(xo)
    xr = xo
    ! Comprobamos si xo corresponde ya a la respuesta
    IF (ABS(norma2(rr))<tol) THEN
      PRINT *, "La convergencia se alcanzó en xo"
      RETURN
    END IF
    ! Calculamos el criterio de convergencia
    toladj = tol*norma2(rr)
    ! Realizamos las iteraciones
    pp = rr
    DO i=1,itmax
      Apk = AA%dot(pp)
      alpha = (DOT_PRODUCT(rr,pp))/DOT_PRODUCT(Apk, pp)
      xr = xr + alpha*pp
      ra = rr
      rr = rr - alpha*Apk
      beta = DOT_PRODUCT(rr, rr) / DOT_PRODUCT(ra, ra)
      pp = rr + beta*pp
      ! Comprobamos si se alcanzó la convergencia
      IF(norma2(rr) < toladj) THEN
        PRINT*,'la convergencia se alcanzo en ', i,' iteraciones!!'
        RETURN
      END IF
    END DO
    PRINT*,'No se alcanzo la CV en el numero maximo de iteraciones!!'
    STOP
  END SUBROUTINE

  FUNCTION norma2(vect)
    !======================================================
    ! Calcula la norma euclideana de un vector de R^n
    !
    ! Variables:
    !
    ! vect   = vector de R^n al que se le calculara su norma
    ! norma2 = norma 2 del vector vect
    ! Autor: Rodolfo Araya Duran
    !======================================================
    IMPLICIT NONE
    REAL(kind=dp), INTENT(in) :: vect(:)
    REAL(kind=dp)             :: norma2
    norma2 = 0.0_dp
    norma2 = SQRT(DOT_PRODUCT(vect,vect))
  END FUNCTION norma2
END MODULE