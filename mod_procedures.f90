MODULE mod_procedures
  USE mod_diag
  implicit none

  contains
!
! >---------- *** READ IN TENSORS FROM FILE *** ----------<
!
  SUBROUTINE read_tensors(infile,nbrows,atom_tensors)
    implicit none

    INTEGER                      :: io,i,nbrows
    CHARACTER(LEN=*)             :: infile
    DOUBLE PRECISION,ALLOCATABLE :: atom_tensors(:,:)

    OPEN(10,file=infile,status='old',action='read',iostat=io)
    IF ( io .NE. 0 ) ERROR STOP "Failed to open infile"
    
    nbrows = -1
    DO WHILE ( io .EQ. 0 )
      READ(10,*,iostat=io)
      nbrows = nbrows+1
    END DO
    
    ALLOCATE(atom_tensors(nbrows,3))
    REWIND(10)
    DO i = 1,nbrows
      READ(10,*,iostat=io) atom_tensors(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE read_tensors
!
! >---------- *** DIAGONALISE 3x3 TENSORS *** ----------<
!
  SUBROUTINE diag_tensors(nbrows,atom_tensors,diagtensors)
    implicit none

    INTEGER                      :: nbrows,i
    LOGICAL                      :: SYM
    DOUBLE PRECISION,ALLOCATABLE :: atom_tensors(:,:),diagtensors(:,:)
    DOUBLE PRECISION,ALLOCATABLE :: slice_in(:,:),slice_out(:)

    ALLOCATE(diagtensors(INT(nbrows/3),5))
    ALLOCATE(slice_in(3,3),slice_out(3))
    diagtensors = 0.d0

    DO i = 1,nbrows-2,3
      SYM = CHECKSYMM(atom_tensors(i:i+2,:),3)
      slice_in = DBLE(atom_tensors(1:1+2,:))

      IF ( .NOT. SYM ) THEN                       ! For nonsymmetric, real matrix
        CALL DIAG_ASYM_NXN(3,slice_in,slice_out)
      ELSE                                        ! For symmetric, real matrix:
        CALL DIAG_SYM_NXN(3,slice_in,slice_out)
      END IF

      diagtensors(INT(1+(i-1)/3),1:3) = slice_out(:)
    END DO

    DEALLOCATE(slice_in,slice_out)

  END SUBROUTINE diag_tensors
!
! >---------- *** COMPUTE ISOTROPIC TENS AND TENSOR ANISOTROPY *** ----------<
!
  SUBROUTINE calc_iso_aniso(diagtensors)
    implicit none

    INTEGER                      :: i
    DOUBLE PRECISION,ALLOCATABLE :: diagtensors(:,:)

    ! Calc iso in 4th column
    DO i = 1,3
      diagtensors(:,4) = diagtensors(:,4) + diagtensors(:,i)
    END DO
    diagtensors(:,4) = diagtensors(:,4) / 3.d0
    ! Calc aniso in 5th column
    diagtensors(:,5) = diagtensors(:,3) - (diagtensors(:,1)+diagtensors(:,2))/2.d0

  END SUBROUTINE calc_iso_aniso
!
! >---------- *** CALC AVERAGE AND STDEV ON ALL TENSOR ELEMENTS *** ----------<
!
  SUBROUTINE avg_stdev_tens(nbrows,atom_tensors,avgtens,stdevtens)
    implicit none

    INTEGER                         :: nbrows,i,j
    DOUBLE PRECISION,DIMENSION(3,3) :: avgtens,tmptens,stdevtens
    DOUBLE PRECISION,ALLOCATABLE    :: atom_tensors(:,:)

    avgtens(:,:) = 0.d0
    tmptens(:,:) = 0.d0
    DO i = 1,3
      DO j = i,nbrows-(3-i),3
      avgtens(i,:) = avgtens(i,:) + atom_tensors(j,:)
      tmptens(i,:) = tmptens(i,:) + (atom_tensors(j,:)**2.)
      END DO
    END DO

    avgtens = avgtens/(nbrows/3)
    stdevtens = SQRT( tmptens/(nbrows/3) - avgtens**2. )

  END SUBROUTINE avg_stdev_tens
!
! >---------- *** WRITE OUT NxN MATRIX - TO FILE OR STDOUT, W/ OR W/O UNITS *** ----------<
!
  SUBROUTINE write_NxN_mat(mat,F,units)
    implicit none

    INTEGER             :: i,j,file,fmt
    DOUBLE PRECISION    :: mat(:,:)
! -- Optional arguments:
    INTEGER,OPTIONAL          :: F
    CHARACTER(LEN=*),OPTIONAL :: units

    file = 6 
    IF ( PRESENT(F) ) file = F

    ASSIGN 100 TO fmt
    IF ( PRESENT(units) ) ASSIGN 110 TO fmt

100 FORMAT(*(F8.2))
110 FORMAT(*(F8.2,X,A))

    DO i=1,UBOUND(mat,DIM=1)
      IF ( PRESENT(units) ) THEN
        WRITE(file,fmt) (mat(i,j),units,j=1,UBOUND(mat,DIM=2))
      ELSE
        WRITE(file,fmt) (mat(i,j),j=1,UBOUND(mat,DIM=2))
      END IF
    END DO

  END SUBROUTINE write_NxN_mat
!
! >---------- *** FILE NAME MANIPULATION *** ----------<
!
  SUBROUTINE change_extension(infile,old_ext,new_ext,outfile)
    implicit none

    CHARACTER(LEN=*) :: infile,old_ext,new_ext,outfile

    outfile = infile(1:INDEX(infile,old_ext)-1)//new_ext

  END SUBROUTINE change_extension

  CHARACTER(LEN=80) FUNCTION change_extension_fct(infile,old_ext,new_ext)
    implicit none

    CHARACTER(LEN=*) :: infile,old_ext,new_ext

    change_extension_fct = TRIM(infile(1:INDEX(infile,old_ext)-1)//new_ext)

  END FUNCTION change_extension_fct


END MODULE mod_procedures
