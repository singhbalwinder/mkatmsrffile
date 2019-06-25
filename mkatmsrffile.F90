!===============================================================================
! 12/06/2010  Jim Edwards jedwards@ucar.edu
! 06/24/2018  Balwinder Singh (balwinder.singh@pnnl.gov) removed pio, mct, mpi 
!             and csm_share dependencies
!
! Interpolate files needed for cam atmosphere dry deposition to model grid
!===============================================================================

program mkatmsrffile
  use netcdf, only: nf90_open, nf90_inq_dimid, nf90_inquire_dimension,      &
       nf90_inq_varid, nf90_get_var, nf90_close, NF90_NOWRITE, nf90_create, &
       nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, NF90_NOERR,   &
       nf90_strerror, NF90_CLOBBER, NF90_DOUBLE

  implicit none

  !Define datatypes
  integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: shr_kind_cl = 256           ! long char
  integer,parameter :: shr_kind_cx = 512           ! extra-long char

  !Other parameters
  real(r8),parameter :: huge_real = huge(1.0_r8)

  
  !Variables
  type rptr
     real(r8), pointer :: fld(:)
  end type rptr

  type(rptr), pointer :: soilw(:), pft(:), apft(:), asoilw(:)

  character(len=shr_kind_cl) :: srffilename, atmfilename
  character(len=shr_kind_cl) :: soilwfilename, landfilename, outputfilename

  character(len=shr_kind_cx) :: srf2atmFmapname

  integer :: srfnx, atmnx, dimid, nlat, nlon, i, j, dim1, dim2, npft
  integer :: ncid_map, ncid_land, ncid_soil, ncid_out
  integer :: varid, varid1,varid2,n_a,n_b,n_s, total_grd_pts

  integer,pointer :: col(:),row(:)

  real(r8) :: total_land, fraction_soilw  

  real(r8), pointer :: landmask(:),lake(:), wetland(:), urban(:)
  real(r8), pointer :: alake(:), awetland(:), aurban(:), fraction_landuse(:,:)
  real(r8), pointer :: total_soilw(:,:)
  real(r8), pointer :: tmp2d(:,:), tmp3d(:,:,:)
  real(r8), pointer :: wgt(:)


  !Read namelist
  namelist /input/srfFileName,atmFileName,landFileName,soilwFileName,srf2atmFmapname,outputFileName 
  
  open(1, file = 'nml_atmsrf', status = 'old')
  read(1,nml=input)
  close(1)

  !Read grid sizes of srf and atm files
  call openfile_and_initdecomp(srffilename, srfnx)
  call openfile_and_initdecomp(atmfilename, atmnx)

  !Read map file for weights and other parameters required for remapping
  call nc_check(nf90_open(trim(srf2atmFmapname), NF90_NOWRITE, ncid_map))
  call nc_check( nf90_inq_dimid(ncid_map,"n_a", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_map, dimid, len = n_a) )
  call nc_check( nf90_inq_dimid(ncid_map,"n_b", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_map, dimid, len = n_b) )
  call nc_check( nf90_inq_dimid(ncid_map,"n_s", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_map, dimid, len = n_s) )

  !allocate and read map file variables
  allocate(col(n_s))
  call nc_check(nf90_inq_varid(ncid_map,'col',varid))
  call nc_check(nf90_get_var(ncid_map,varid,col))

  allocate(row(n_s))
  call nc_check(nf90_inq_varid(ncid_map,'row',varid))
  call nc_check(nf90_get_var(ncid_map,varid,row))

  allocate(wgt(n_s))
  call nc_check(nf90_inq_varid(ncid_map,'S',varid))
  call nc_check(nf90_get_var(ncid_map,varid,wgt))

  !Close map file
  call nc_check(nf90_close ( ncid_map ))

  !Read Land file
  call nc_check(nf90_open(landfilename, NF90_NOWRITE, ncid_land))
  
  call nc_check( nf90_inq_dimid(ncid_land, "lon", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_land, dimid, len = nlon) )
  call nc_check( nf90_inq_dimid(ncid_land, "lat", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_land, dimid, len = nlat) )

  total_grd_pts = nlon*nlat

  call nc_check( nf90_inq_dimid(ncid_land, "pft", dimid) )
  call nc_check( nf90_inquire_dimension(ncid_land, dimid, len = npft) )

  !Allocate temporary variables to read data from netcdf file
  allocate(tmp2d(nlon,nlat),tmp3d(nlon,nlat,npft))
  tmp2d(:,:)   = huge_real
  tmp3d(:,:,:) = huge_real

  allocate(lake(total_grd_pts))
  call nc_check(nf90_inq_varid(ncid_land,'PCT_LAKE',varid))
  call nc_check(nf90_get_var(ncid_land,varid,tmp2d))
  lake(:) = reshape(tmp2d,(/total_grd_pts/))
  lake = lake * 0.01_r8


  call nc_check(nf90_inq_varid(ncid_land,'PCT_PFT',varid))
  call nc_check(nf90_get_var(ncid_land,varid,tmp3d))

  allocate(pft(npft),apft(npft))

  !deallocate all memory at the end???
  !Storing in a data structure
  do i=1,npft
     allocate(pft(i)%fld(total_grd_pts))
     pft(i)%fld = reshape(tmp3d(:,:,i),(/total_grd_pts/)) * 0.01_r8
  end do


  tmp2d(:,:)   = huge_real  !Reinitialize tmp2d to inf
  allocate(wetland(total_grd_pts))
  call nc_check(nf90_inq_varid(ncid_land,'PCT_WETLAND',varid))
  call nc_check(nf90_get_var(ncid_land,varid,tmp2d))
  wetland = reshape(tmp2d,(/total_grd_pts/))
  wetland = wetland * 0.01_r8

  tmp2d(:,:)   = huge_real  !Reinitialize tmp2d to inf
  allocate(urban(total_grd_pts))
  call nc_check(nf90_inq_varid(ncid_land,'PCT_URBAN',varid))
  call nc_check(nf90_get_var(ncid_land,varid,tmp2d))

  urban = reshape(tmp2d,(/total_grd_pts/))
  urban = urban * 0.01_r8

  tmp2d(:,:)   = huge_real    !Reinitialize tmp2d to inf
  allocate(landmask(srfnx))
  call nc_check(nf90_inq_varid(ncid_land,'LANDMASK',varid))
  call nc_check(nf90_get_var(ncid_land,varid,tmp2d))
  landmask = reshape(tmp2d,(/total_grd_pts/))


  call nc_check(nf90_close ( ncid_land ))

  !open soil file
  call nc_check(nf90_open(soilwfilename, NF90_NOWRITE, ncid_soil))!use a different ncid variable?

  deallocate(tmp3d)
  allocate(tmp3d(nlon,nlat,12))!make 12 a variable or read from file??
  call nc_check(nf90_inq_varid(ncid_soil,'SOILW',varid))
  call nc_check(nf90_get_var(ncid_soil,varid,tmp3d))
  call nc_check(nf90_close ( ncid_soil ))


  allocate(soilw(12),asoilw(12))
  do i=1,12
     allocate(soilw(i)%fld(nlat*nlon))
     soilw(i)%fld = reshape(tmp3d(:,:,i),(/total_grd_pts/))
  end do
  tmp3d(:,:,:)   = huge_real   !Reinitialize tmp3d to inf

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


  !sanity nc_checks for n_s being equal to atmnx etc.?
  allocate(alake(n_b), awetland(n_b), aurban(n_b))
  alake(:)    = 0.0_r8
  awetland(:) = 0.0_r8
  aurban(:)   = 0.0_r8 !why can't it be huge_real???
  do i = 1, n_s !change i to longer var name with mult characters?
     alake(row(i)) =  alake(row(i)) + wgt(i)*lake(col(i))
     awetland(row(i)) =  awetland(row(i)) + wgt(i)*wetland(col(i))
     aurban(row(i)) =  aurban(row(i)) + wgt(i)*urban(col(i))
  enddo
  
  do j = 1, npft
     allocate(apft(j)%fld(n_b))
     apft(j)%fld(:) = 0.0_r8
     do i = 1, n_s !change i to longer var name with mult characters?
        apft(j)%fld(row(i)) = apft(j)%fld(row(i)) + wgt(i)*pft(j)%fld(col(i))
     enddo
  enddo

  do j = 1, 12 !replace 12 with a var name?
     allocate(asoilw(j)%fld(n_b))
     asoilw(j)%fld(:) = 0.0_r8
     do i = 1, n_s !change i to longer var name with mult characters?
        asoilw(j)%fld(row(i)) = asoilw(j)%fld(row(i)) + wgt(i)*soilw(j)%fld(col(i))
     enddo
  enddo

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

  call nc_check( nf90_create(trim(outputfilename), NF90_CLOBBER, ncid_out) ) ! NEW NCID VAR?

  call nc_check( nf90_def_dim(ncid_out, 'ncol', atmnx, dim1) )
  call nc_check( nf90_def_dim(ncid_out, 'class', 11, dim2) )
  call nc_check( nf90_def_var(ncid_out, 'fraction_landuse', NF90_DOUBLE, (/dim1,dim2/), varid1) )
  call nc_check( nf90_def_dim(ncid_out,'month',12, dim2))
  call nc_check( nf90_def_var(ncid_out,'soilw', NF90_DOUBLE, (/dim1,dim2/), varid2))

  call nc_check( nf90_enddef(ncid_out) )

  call nc_check(nf90_put_var(ncid_out, varid1, fraction_landuse))
  call nc_check(nf90_put_var(ncid_out, varid2, total_soilw ))

  call nc_check( nf90_close(ncid_out) )


contains

  subroutine openfile_and_initdecomp(filename, nx)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nx

    call nc_check(nf90_open(filename, NF90_NOWRITE, ncid_out))

    call nc_check( nf90_inq_dimid(ncid_out, "grid_size", dimid) )
    call nc_check( nf90_inquire_dimension(ncid_out, dimid, len = nx) )
    call nc_check(nf90_close ( ncid_out ))
  end subroutine openfile_and_initdecomp




  subroutine nc_check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine nc_check  

end program mkatmsrffile




