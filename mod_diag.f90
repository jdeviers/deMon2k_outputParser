MODULE mod_diag
  implicit none

  contains

  LOGICAL FUNCTION CHECKSYMM(MAT,LEN)
    implicit none

    INTEGER            :: i,j
    INTEGER,INTENT(IN) :: LEN
    DOUBLE PRECISION   :: MAT(:,:)

    DO i=1,LEN
      DO j=i+1,LEN

        IF (MAT(i,j) .NE. MAT(j,i)) THEN
          CHECKSYMM=.FALSE.
          WRITE(*,'(A)') 'A is NOT SYMMETRIC; using DGEEV.'
          RETURN
        END IF
        
      END DO
    END DO
    CHECKSYMM=.TRUE.
    WRITE(*,'(A)') 'A is SYMMETRIC; using DSYEV.'
    RETURN
  END FUNCTION CHECKSYMM

  SUBROUTINE DIAG_SYM_NXN(N,A,EV)
    implicit none

    DOUBLE PRECISION             :: A(:,:),EV(:)
    INTEGER                      :: N,LDA
    INTEGER,PARAMETER            :: LWMAX = 1000
    INTEGER                      :: INFO,LWORK
    DOUBLE PRECISION,ALLOCATABLE :: W(:)
    DOUBLE PRECISION             :: WORK(LWMAX)

    LDA = N
    ALLOCATE(W(N))
! Find optimal internal settings for DSYEV  
    LWORK = -1
    CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
    LWORK = MIN(LWMAX,INT(WORK(1)))
! Solve eigenproblem
    CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
! Check for convergence.
    IF (INFO.GT.0) THEN
      WRITE(*,*)'The algorithm failed to compute eigenvalues.'
      STOP
    END IF
! Eigenvalues print
!    WRITE(*,'(/,A)') 'Eigenvalues (in ascending order) are stored in EV:'
    EV = W

    DEALLOCATE(W)
  END SUBROUTINE DIAG_SYM_NXN

  SUBROUTINE DIAG_ASYM_NXN(N,A,EV)
    implicit none

    DOUBLE PRECISION             :: A(:,:)
    DOUBLE PRECISION             :: EV(:)
    INTEGER,PARAMETER            :: LWMAX = 1000
    INTEGER                      :: N,LDA,LDVL,LDVR,i
    INTEGER                      :: INFO,LWORK
    DOUBLE PRECISION             :: WORK(LWMAX)
    DOUBLE PRECISION,ALLOCATABLE :: VL(:,:),VR(:,:),WR(:),WI(:)
    LOGICAL,DIMENSION(3)         :: LOC = .TRUE.

    LDA = N; LDVL = N; LDVR = N
    ALLOCATE(VL(LDVL,N), VR(LDVL,N), WR(N), WI(N))
! Find optimal internal settings for DGEEV
    LWORK = -1
    CALL DGEEV('Vectors','Vectors',N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,WORK,LWORK,INFO)
    LWORK = MIN(LWMAX,INT(WORK(1)))
! Solve eigenproblem.
    CALL DGEEV('Vectors','Vectors',N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,WORK,LWORK,INFO)
! Check for convergence.
    IF (INFO.GT.0) THEN
      WRITE(*,*)'The algorithm failed to compute eigenvalues.'
      STOP
    END IF
! Eigenvalues print
!    WRITE(*,'(/,A)') 'REAL part of the eigenvalues:'
    EV(1) = MINVAL(WR); EV(3) = MAXVAL(WR)               ! Sort the eigenvalues in ascending order
    LOC(MINLOC(WR)) = .FALSE.; LOC(MAXLOC(WR)) = .FALSE. ! Only the middle value keeps a .TRUE. flag
    DO i=1,3
      IF ( LOC(i) ) EV(2) = WR(i)
    END DO
!    EV(2) = WR(FINDLOC(LOC,.TRUE.,DIM=1)) ! This one-liner is F08 standard

    DEALLOCATE(VL,VR,WR,WI)
  END SUBROUTINE DIAG_ASYM_NXN

END MODULE mod_diag