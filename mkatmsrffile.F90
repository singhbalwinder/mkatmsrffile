!===============================================================================
! SVN $Id:  $
! SVN $URL: $
! 12/06/2010  Jim Edwards jedwards@ucar.edu
! Interpolate files needed for cam atmosphere dry deposition to model grid
!===============================================================================

program mkatmsrffile
  use netcdf 
  use mpi, only: MPI_COMM_WORLD
  use pio, only:iosystem_desc_t,io_desc_t,file_desc_t,var_desc_t, &
       pio_rearr_none,pio_openfile,pio_iotype_netcdf,pio_noclobber, &
       pio_inq_dimid,pio_inq_dimlen,pio_double,pio_inq_varid, &
       PIO_OFFSET_KIND,pio_createfile,pio_def_dim, pio_def_var, &
       pio_enddef,pio_clobber, pio_createfile, pio_init, &
       pio_initdecomp,pio_read_darray,pio_setframe,pio_closefile, &
       pio_freedecomp,pio_write_darray,pio_finalize      

  use mct_mod, only:mct_gsmap,mct_SMatP,mct_aVect,mct_aVect_init, &
       mct_avect_indexra,shr_kind_cx,mct_world_init,i90_allloadf,i90_label, &
       i90_gtoken,i90_release,mct_gsmap_orderedpoints, mct_avect_zero, &
       mct_smat_avmult,mct_gsmap_init

  use shr_mct_mod, only: shr_mct_queryconfigfile, shr_mct_smatpinitnc
  use shr_kind_mod, only : r8=>shr_kind_r8, shr_kind_cl


  implicit none

  !remove dependence on csm share
  !integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
  !integer,parameter :: shr_kind_cl = 256           ! long char
  !integer,parameter :: shr_kind_cx = 512           ! extra-long char


  integer :: ierr, npes, iam, npft
  integer, pointer :: sfcgindex(:), atmgindex(:)

  type rptr
     real(r8), pointer :: fld(:)
  end type rptr

  type(rptr), pointer :: soilw(:), pft(:), apft(:), asoilw(:)


  type(iosystem_desc_t) :: iosystem

  type(mct_gsmap) :: gsMap_srf, gsMap_atm
  type(mct_SMatP) :: sMatP
  type(mct_aVect), target :: srf_av, atm_av



  integer, pointer :: comps(:) ! array with component ids
  integer, pointer :: comms(:) ! array with mpicoms
  type(io_desc_t) :: atm_iodesc, srf_iodesc

  character(len=shr_kind_cl) :: srffilename
  character(len=shr_kind_cl) :: atmfilename

  character(len=shr_kind_cl) :: soilwfilename
  character(len=shr_kind_cl) :: landfilename

  character(len=shr_kind_cl) :: outputfilename
  character(len=shr_kind_cx) :: mapname
  character(len=shr_kind_cl) :: maptype
  

  type(file_desc_t) :: landfile, newfile
  integer, pointer :: dof(:), dof2(:), dof3(:)
  real(r8), pointer :: landmask(:),lake(:), wetland(:), urban(:)
  real(r8), pointer :: alake(:), awetland(:), aurban(:), fraction_landuse(:,:)
  integer :: srfnx, atmnx, srfnxg, atmnxg, dimid, nlat, nlon, i, j, clen, index, dim1, dim2
  type(var_desc_t) :: vid, vid1, vid2
  
  character(len=*), parameter :: srffields(5) =(/"PCT_LAKE   ",&
                                                 "PCT_WETLAND",&
                                                 "PCT_URBAN  ", &
                                                 "SOILW      ", & 
                                                 "PCT_PFT    "/)
  
  
  character(len=220) :: rList
  character(len=6) :: str
  real(r8) :: total_land, fraction_soilw
  real(r8), pointer :: total_soilw(:,:)
  Character(len=*), parameter :: ConfigFileName="mkatmsrffile.rc"

  integer :: ncid,varid, ieq
  real(r8), pointer :: lake_balli(:,:),wetland_balli(:,:),urban_balli(:,:),landmask_balli(:,:),pft_balli(:,:,:),soilw_balli(:,:,:)

  !MPI init and other calls to run on multi procs...do not forget to link with -lpmi while compiling
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, npes, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, iam, ierr)

  print*,'npes:',npes
  print*,'iam:',iam

  !init pio
  call pio_init(iam, MPI_COMM_WORLD, npes, 0, 1, pio_rearr_none, iosystem,base=0)
  !iosystem is initialzed in pio_init call, 
  !following I am just printing out one of its member 
  !(see cime/src/externals/pio2/src/clib/pio.h for all members or /cime/src/externals/pio1/pio/pio_types.F90)
  print*,'iosystem%num_iotasks:',iosystem%num_iotasks
  allocate(comps(2), comms(2))
  comps(1)=1
  comps(2)=2
  call mpi_comm_dup(MPI_COMM_WORLD,comms(1),ierr)
  call mpi_comm_dup(MPI_COMM_WORLD,comms(2),ierr)
  call mct_world_init(2, MPI_COMM_WORLD, comms, comps)
  
  !load ConfigFileName to read input file names
  call I90_allLoadF(ConfigFileName,0,MPI_COMM_WORLD,ierr)

  !reading input file names from the file loaded above using I90_allLoadF
  call I90_label('srfFileName:', ierr) ! label 'srfFileName:' is being read from "mkatmsrffile.rc" file ...
  call i90_gtoken(srffilename, ierr)   ! ... and part following this string is stored in srffilename variable ....
  call I90_label('atmFileName:', ierr) ! ....similarly for rest of the files
  call i90_gtoken(atmfilename, ierr)
  call I90_label('landFileName:', ierr)
  call i90_gtoken(landfilename, ierr)
  call I90_label('soilwFileName:', ierr)
  call i90_gtoken(soilwfilename, ierr)
  call I90_label('outputFileName:', ierr)
  call i90_gtoken(outputfilename, ierr)
  call i90_release(ierr) !finish readin file "mkatmsrffile.rc"

  print*,'srffilename   :',trim(adjustl(srffilename))
  print*,'atmfilename   :',trim(adjustl(atmfilename))
  print*,'landfilename  :',trim(adjustl(landfilename))
  print*,'soilwFileName :',trim(adjustl(soilwFileName))
  print*,'outputFileName:',trim(adjustl(outputFileName))

  ! gsmap_srf, srfnx, srfnxg are the intent-outs (see more detail in the subroutine)
  call openfile_and_initdecomp(iosystem, srffilename, npes, iam, gsmap_srf, srfnx, srfnxg)
  ! gsmap_atm, atmnx, atmnxg are the intent-outs ()
  call openfile_and_initdecomp(iosystem, atmfilename, npes, iam, gsmap_atm, atmnx, atmnxg)

 
  ! The following routine is reading values for labels ("srf2atmFmapname:" and  "srf2atmFmaptype:") from "mkatmsrffile.rc" file
  call shr_mct_queryConfigFile(MPI_COMM_WORLD, "mkatmsrffile.rc", &
       "srf2atmFmapname:",mapname,"srf2atmFmaptype:",maptype)
  print*,'mapname:',trim(adjustl(mapname))
  print*,'maptype:',trim(adjustl(maptype))
    
  ! Frm my reading of the following call:
  !sMatP read area_a(n_a), area_b(n_b),col(n_s),row(n_s) and S(n_s) 
  ! from mapname file. MAY BE gsmap_srf is a source grid here and 
  ! gsmap_atm is destination
  !sMatP has fields which stores above variables...it just stores them
  call shr_mct_sMatPInitnc(sMatP,gsmap_srf, gsmap_atm, &
       mapname, maptype, MPI_COMM_WORLD)


  call check(nf90_open(landfilename, NF90_NOWRITE, ncid))
  
  call check( nf90_inq_dimid(ncid, "lon", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = nlon) )
  call check( nf90_inq_dimid(ncid, "lat", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = nlat) )
  call check( nf90_inq_dimid(ncid, "pft", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = npft) )


  call mct_gsmap_OrderedPoints(gsMap_srf, iam, Dof)


  !following do loop is just creating rlist variable to init srf_av and atm_av, nothing else
  rlist = ' '
  clen=1
  do i=1,npft+15
     if(i<=npft) then
        write(str,'(A,i2.2,A)') 'pft',i,':'
        rlist(clen:clen+5) = str
        clen=clen+6
     else if(i<=npft+12) then
        write(str,'(A,i2.2,A)') 'slw',i-npft,':'
        rlist(clen:clen+5) = str
        clen=clen+6
     else
        if(i==npft+13) clen=clen-1
        rlist(clen:clen+len_trim(srffields(i-12-npft))) = ':'//trim(srffields(i-12-npft))
        clen = clen+len_trim(srffields(i-12-npft))+1
     end if
  end do
  print*,'rlist:',rlist
  print*,'clen:',clen

  !following routine creates a variable "srf_av%rattr" having size (number of strings in rlist,srfnx)
  call mct_aVect_init(srf_av, rlist=trim(rlist), lsize=srfnx)
  !probably zero out everything
  call mct_aVect_zero(srf_av)
  call mct_aVect_init(atm_av, rlist=rlist, lsize=atmnx)
  call mct_aVect_zero(atm_av)

  !Get index of PCT_LAKE in rlist (note: rlist also exists in srf_av array)
  index = mct_avect_indexra(srf_av,'PCT_LAKE')
  !point lake to that memory, index is 30 here and it is pointing to 
  !srf_av%rattr(30,:), second subscript has a length of srfnx=64800
  lake => srf_av%rattr(index,:)

  allocate(lake_balli(nlon,nlat))
  call check(nf90_inq_varid(ncid,'PCT_LAKE',varid))
  call check(nf90_get_var(ncid,varid,lake_balli))
  lake(:) = reshape(lake_balli,(/nlon*nlat/))
  
  lake = lake * 0.01_r8

  allocate(pft_balli(nlon,nlat,npft))
  call check(nf90_inq_varid(ncid,'PCT_PFT',varid))
  call check(nf90_get_var(ncid,varid,pft_balli))
  print*,'spahe(pft_balli):',shape(pft_balli)
  allocate(pft(npft),apft(npft))

  !following is reading PCT_PFT array which is dimensioned (npft,nlat,nlon)
  !following loop stores each 2d array (nlat,nlon) in pft(i)%fld, where i goes from
  !1 to npft
  do i=1,npft
     write(str,'(A,i2.2)') 'pft',i

     pft(i)%fld => srf_av%rattr(mct_avect_indexra(srf_av,str(1:5)),:)
     apft(i)%fld => atm_av%rattr(mct_avect_indexra(atm_av,str(1:5)),:)
     !pft(i)%fld = pft(i)%fld * 0.01_r8
     pft(i)%fld = reshape(pft_balli(:,:,i),(/nlon*nlat/)) * 0.01_r8
  end do


  index = mct_avect_indexra(srf_av,'PCT_WETLAND')
  wetland => srf_av%rattr(index,:)

  allocate(wetland_balli(nlon,nlat))
  call check(nf90_inq_varid(ncid,'PCT_WETLAND',varid))
  call check(nf90_get_var(ncid,varid,wetland_balli))
  wetland = reshape(wetland_balli,(/nlon*nlat/))
  wetland = wetland * 0.01_r8


  index = mct_avect_indexra(srf_av,'PCT_URBAN')
  urban => srf_av%rattr(index,:)

  allocate(urban_balli(nlon,nlat))
  call check(nf90_inq_varid(ncid,'PCT_URBAN',varid))
  call check(nf90_get_var(ncid,varid,urban_balli))

  urban = reshape(urban_balli,(/nlon*nlat/))
  urban = urban * 0.01_r8

  
  allocate(landmask(srfnx))
  allocate(landmask_balli(nlon,nlat))
  call check(nf90_inq_varid(ncid,'LANDMASK',varid))
  call check(nf90_get_var(ncid,varid,landmask_balli))
  landmask = reshape(landmask_balli,(/nlon*nlat/))


  call check(nf90_close ( ncid ))

  !open soil file
  call check(nf90_open(soilwfilename, NF90_NOWRITE, ncid))!use a different ncid variable?

  allocate(soilw_balli(nlon,nlat,12))!make 12 a variable or read from file??
  call check(nf90_inq_varid(ncid,'SOILW',varid))
  call check(nf90_get_var(ncid,varid,soilw_balli))
  print*,'spahe(soilw_balli):',shape(soilw_balli)


  allocate(soilw(12),asoilw(12))
  do i=1,12
     str = ' '
     write(str,'(A,i2.2)') 'slw',i     
     soilw(i)%fld => srf_av%rattr(mct_avect_indexra(srf_av,str(1:5)),:)
     asoilw(i)%fld => atm_av%rattr(mct_avect_indexra(atm_av,str(1:5)),:)

     soilw(i)%fld = reshape(soilw_balli(:,:,i),(/nlat*nlon/))
  end do

  do i=1,srfnx
     if(nint(landmask(i)) == 0) then
        lake(i) = 1.0
        wetland(i) = 0.0
        urban(i) = 0.0
        do j=1,12
           soilw(j)%fld(i) = 0.0
        end do
     end if
  end do
  deallocate(landmask)

  !index = mct_avect_indexra(atm_av,'PCT_LAKE')
  !alake => atm_av%rattr(index,:)

  !I went into the routine and found the following relevant code
  !for mat mult:
  !<code start>
  !do n=1,num_elements
  !   
  !   row = sMat%data%iAttr(irow,n)
  !   col = sMat%data%iAttr(icol,n)
  !   wgt = sMat%data%rAttr(iwgt,n)
     
  !   ! loop over attributes being regridded.
  !   do m=1,num_indices
  !      yAV%rAttr(m,row) = yAV%rAttr(m,row) + wgt * xAV%rAttr(m,col)
  !   end do ! m=1,num_indices
  !end do ! n=1,num_elements
  !<code ends>


  call mct_sMat_avMult( srf_av, smatP, atm_av)

  index = mct_avect_indexra(atm_av,'PCT_LAKE')
  alake => atm_av%rattr(index,:)


  index = mct_avect_indexra(atm_av,'PCT_WETLAND')
  awetland => atm_av%rattr(index,:)

  index = mct_avect_indexra(atm_av,'PCT_URBAN')
  aurban => atm_av%rattr(index,:)



  fraction_soilw=0.0

  allocate(fraction_landuse(atmnx,11))
  allocate(total_soilw(atmnx,12))

  fraction_landuse = 0.0_r8
  do i=1,atmnx
     total_soilw(i,:)=0.0
     total_land = (alake(i)+awetland(i)+aurban(i))
     do j=1,npft
        total_land=total_land+apft(j)%fld(i)
     end do
     fraction_soilw = total_land - (alake(i)+awetland(i))
     if(total_land < 1.0_r8) then
        alake(i) = alake(i) + (1.0_r8 - total_land)
     end if
     
!     print *,i,fraction, fraction_soilw
!     if(abs(fraction-1.0_r8) > 0.1_r8) then
!        print *, i, fraction, alake(i), awetland(i), aurban(i), (apft(j)%fld(i), j=1,npft)
!     end if

     do j=1,12
        total_soilw(i,j) = total_soilw(i,j) + asoilw(j)%fld(i) * fraction_soilw
     end do

     fraction_landuse(i,1) = aurban(i)
     fraction_landuse(i,2) = apft(16)%fld(i) + apft(17)%fld(i)
     fraction_landuse(i,3) = apft(13)%fld(i) + apft(14)%fld(i) + apft(15)%fld(i)
     fraction_landuse(i,4) = apft(5)%fld(i) + apft(6)%fld(i) + apft(7)%fld(i)+ apft(8)%fld(i) + apft(9)%fld(i)
     fraction_landuse(i,5) = apft(2)%fld(i) + apft(3)%fld(i) + apft(4)%fld(i)
     fraction_landuse(i,6) = awetland(i)
     fraction_landuse(i,7) = alake(i)
     fraction_landuse(i,8) = apft(1)%fld(i)
     fraction_landuse(i,11) = apft(10)%fld(i) + apft(11)%fld(i) + apft(12)%fld(i)

     if(abs(sum(fraction_landuse(i,:)-1._r8)) > 0.001_r8) then
        fraction_landuse(i,:) = fraction_landuse(i,:)/sum(fraction_landuse(i,:))
     end if
     

  end do

  ierr = pio_createfile(iosystem, newFile, pio_iotype_netcdf, trim(outputfilename), pio_clobber)

  ierr = pio_def_dim(newFile, 'ncol', atmnxg, dim1)
  ierr = pio_def_dim(newFile, 'class',11, dim2)

  ierr = pio_def_var(newFile, 'fraction_landuse', pio_double, (/dim1,dim2/), vid1) 
  ierr = pio_def_dim(newFile, 'month',12, dim2)

  ierr = pio_def_var(newFile, 'soilw', pio_double, (/dim1,dim2/), vid2) 

  ierr = pio_enddef(newFile)
  
  call mct_gsmap_OrderedPoints(gsMap_atm, iam, Dof)



  allocate(dof2(atmnx*12))
  do j=1,12
     do i=1,atmnx
        dof2(i+(j-1)*atmnx) = dof(i)+(j-1)*atmnxg
     end do
  end do

  call pio_initdecomp(iosystem, pio_double, (/atmnxg,11/), dof2(1:11*atmnx-1), atm_iodesc)

  call pio_write_darray(newFile, vid1, atm_iodesc, fraction_landuse ,ierr)
  call pio_freedecomp(newfile, atm_iodesc)

  call pio_initdecomp(iosystem, pio_double, (/atmnxg,12/), dof2, atm_iodesc)

  call pio_write_darray(newFile, vid2, atm_iodesc, total_soilw ,ierr)
  call pio_freedecomp(newfile, atm_iodesc)


  call pio_closefile(newFile)
  call pio_finalize(iosystem, ierr)

  deallocate(comps, comms)
  call mpi_finalize(ierr)


contains

  subroutine openfile_and_initdecomp(iosystem, filename, npes, iam, gsmap, nx, nxg)
    type(iosystem_desc_t) :: iosystem
    character(len=*), intent(in) :: filename
    integer, intent(in) :: npes, iam
    type(mct_gsmap), intent(out) :: gsmap
    integer, intent(out) :: nx, nxg

    !local
    integer, pointer :: gindex(:)


    !nx is the # of grid points for one proc and nxg is the toal number of grid points
    !For single proc, gindex is an array of 1 to 64800, nx=nxg=64800
    gindex => get_grid_index(iosystem, filename, npes, iam, nx, nxg)
    print*,'nx, nxg:',nx, nxg
    !print*,'gindex:',gindex
    !The following gives a gsmap object which contains info about grid points assigned to each proc
    call mct_gsMap_init( gsMap, gindex, MPI_COMM_WORLD,1 , nx, nxg)
    print*,'gsmap%start,gsmap%length',gsmap%start,gsmap%length
    deallocate(gindex)



  end subroutine openfile_and_initdecomp


  function get_grid_index(iosystem, filename, npes, iam, nx, nxg) result(gindex)
    !This function computes nx (# of grid points for each task) and
    !nxg (total number of grid points)
    !It also computes gindex which describes which grids point is for which proc.
    !For single proc runs, nx=nxg=64800
    !This subroutine also fills in iosystem data structure

    implicit none
    character(len=*), intent(in) :: filename
    type(iosystem_desc_t),intent(inout) :: iosystem
    integer, intent(in) :: npes, iam
    integer, intent(out) :: nx, nxg

    type(file_desc_t) :: file
    integer, pointer :: gindex(:)


    integer :: dimid, ierr, add1=0, i, start_offset
    integer :: ncid, varid
    
    call check(nf90_open(filename, NF90_NOWRITE, ncid))


    call check( nf90_inq_dimid(ncid, "grid_size", dimid) )
    call check( nf90_inquire_dimension(ncid, dimid, len = nxg) )

    nx = nxg/npes

    if(nx*npes < nxg-(npes-iam-1)) then
       start_offset = nxg-(npes-iam-1)-(nx*npes)-1
       add1 = 1
    else
       add1 = 0
       start_offset=0
    end if
    allocate(gindex(nx+add1))
    do i=1,nx+add1
       gindex(i)=i+iam*nx+start_offset
    end do

  call check(nf90_close ( ncid ))
  end function get_grid_index

  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end program mkatmsrffile




