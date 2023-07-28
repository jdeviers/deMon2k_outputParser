PROGRAM post_prod_tensors
    USE mod_procedures
    implicit none

! General-purpose variables
    INTEGER                         :: io,nb_rows
    CHARACTER(LEN=50)               :: in_file,out_file
    DOUBLE PRECISION,DIMENSION(3,3) :: avg_tens,stdev_tens
    DOUBLE PRECISION,ALLOCATABLE    :: tensors(:,:),diag_tens(:,:)

    DO
      READ(*,'(A)',iostat=io) in_file
      IF ( io .NE. 0 ) STOP "Failed to open infile"

      CALL read_tensors(in_file,nb_rows,tensors)

! -- Quantities on each individual tensor:
      CALL diag_tensors(nb_rows,tensors,diag_tens)
      CALL calc_iso_aniso(diag_tens)

      CALL change_extension(in_file,'.csv','_diagtensors.dat',out_file)
      OPEN(10,file=out_file,status='unknown',action='write')
      WRITE(10,'(A)') '#ZZZ XXX YYY iso aniso'
      CALL write_NxN_mat(diag_tens,F=10)
      CLOSE(10)

! -- Quantities on average tensors:      
      CALL avg_stdev_tens(nb_rows,tensors,avg_tens,stdev_tens)

      CALL change_extension(in_file,'.csv','_average.dat',out_file)
      OPEN(10,file=out_file,status='unknown',action='write')

      WRITE(10,'(3A)') 'Average tensor for ',TRIM(in_file),':'
      CALL write_NxN_mat(avg_tens,F=10)

      WRITE(10,'(/,3A)') 'St.dev. tensor for ',TRIM(in_file),':'
      CALL write_NxN_mat(stdev_tens,F=10)
 
      WRITE(10,'(/,3A)') 'St.dev. percentage of average for ',TRIM(in_file),' in %:'
      CALL write_NxN_mat((stdev_tens/ABS(avg_tens))*100.,F=10,units='%')

      CLOSE(10)
      DEALLOCATE(tensors,diag_tens)
    END DO


END PROGRAM post_prod_tensors