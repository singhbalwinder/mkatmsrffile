!===============================================================================
! SVN $Id:  $
! SVN $URL: $
! 12/06/2010  Jim Edwards jedwards@ucar.edu
! Interpolate files needed for cam atmosphere dry deposition to model grid
!===============================================================================

program mkatmsrffile
  use netcdf 


  implicit none

  !remove dependence on csm share
  integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: shr_kind_cl = 256           ! long char
  integer,parameter :: shr_kind_cx = 512           ! extra-long char

  integer :: ierr, npes, iam, npft
  integer, pointer :: sfcgindex(:), atmgindex(:)

  type rptr
     real(r8), pointer :: fld(:)
  end type rptr

  type(rptr), pointer :: soilw(:), pft(:), apft(:), asoilw(:)

  integer, pointer :: comps(:) ! array with component ids
  integer, pointer :: comms(:) ! array with mpicoms

  character(len=shr_kind_cl) :: srffilename
  character(len=shr_kind_cl) :: atmfilename

  character(len=shr_kind_cl) :: soilwfilename
  character(len=shr_kind_cl) :: landfilename

  character(len=shr_kind_cl) :: outputfilename
  character(len=shr_kind_cx) :: mapname,srf2atmFmapname
  character(len=shr_kind_cl) :: maptype,srf2atmFmaptype
  

  integer, pointer :: dof(:), dof2(:), dof3(:)
  real(r8), pointer :: landmask(:),lake(:), wetland(:), urban(:)
  real(r8), pointer :: alake(:), awetland(:), aurban(:), fraction_landuse(:,:)
  integer :: srfnx, atmnx, srfnxg, atmnxg, dimid, nlat, nlon, i, j, clen, index, dim1, dim2
  
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

  integer :: ncid,varid, ieq, varid1,varid2,n_a,n_b,n_s
  integer,pointer :: col(:),row(:)
  real(r8), pointer :: lake_balli(:,:),wetland_balli(:,:),urban_balli(:,:),landmask_balli(:,:),pft_balli(:,:,:),soilw_balli(:,:,:)
  real(r8), pointer :: area_a(:),area_b(:),s(:)

  
  namelist /input/srfFileName,atmFileName,landFileName,soilwFileName,srf2atmFmapname,srf2atmFmaptype,outputFileName 
  
  open(1, file = 'nml_atmsrf', status = 'old')
  read(1,nml=input)
  close(1)

  mapname = trim(adjustl(srf2atmFmapname))
  maptype = trim(adjustl(srf2atmFmaptype))

  ! gsmap_srf, srfnx, srfnxg are the intent-outs (see more detail in the subroutine)
  call openfile_and_initdecomp(srffilename, srfnx)
  ! gsmap_atm, atmnx, atmnxg are the intent-outs ()
  call openfile_and_initdecomp(atmfilename, atmnx)

 
  ! The following routine is reading values for labels ("srf2atmFmapname:" and  "srf2atmFmaptype:") from "mkatmsrffile.rc" file

      
  ! Frm my reading of the following call:
  !sMatP read area_a(n_a), area_b(n_b),col(n_s),row(n_s) and S(n_s) 
  ! from mapname file. MAY BE gsmap_srf is a source grid here and 
  ! gsmap_atm is destination
  !sMatP has fields which stores above variables...it just stores them


  call check(nf90_open(trim(mapname), NF90_NOWRITE, ncid))
  call check( nf90_inq_dimid(ncid,"n_a", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = n_a) )
  call check( nf90_inq_dimid(ncid,"n_b", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = n_b) )
  call check( nf90_inq_dimid(ncid,"n_s", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = n_s) )

  !allocate and read vars
  allocate(area_a(n_a))
  call check(nf90_inq_varid(ncid,'area_a',varid))
  call check(nf90_get_var(ncid,varid,area_a))

  allocate(area_b(n_b))
  call check(nf90_inq_varid(ncid,'area_b',varid))
  call check(nf90_get_var(ncid,varid,area_b))

  allocate(col(n_s))
  call check(nf90_inq_varid(ncid,'col',varid))
  call check(nf90_get_var(ncid,varid,col))

  allocate(row(n_s))
  call check(nf90_inq_varid(ncid,'row',varid))
  call check(nf90_get_var(ncid,varid,row))

  allocate(s(n_s))
  call check(nf90_inq_varid(ncid,'S',varid))
  call check(nf90_get_var(ncid,varid,s))

  call check(nf90_close ( ncid ))




  call check(nf90_open(landfilename, NF90_NOWRITE, ncid))
  
  call check( nf90_inq_dimid(ncid, "lon", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = nlon) )
  call check( nf90_inq_dimid(ncid, "lat", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = nlat) )
  call check( nf90_inq_dimid(ncid, "pft", dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len = npft) )


  allocate(lake_balli(nlon,nlat),lake(nlon*nlat))
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
     allocate(pft(i)%fld(nlon*nlat))
     pft(i)%fld = reshape(pft_balli(:,:,i),(/nlon*nlat/)) * 0.01_r8
  end do


  allocate(wetland_balli(nlon,nlat),wetland(nlon*nlat))
  call check(nf90_inq_varid(ncid,'PCT_WETLAND',varid))
  call check(nf90_get_var(ncid,varid,wetland_balli))
  wetland = reshape(wetland_balli,(/nlon*nlat/))
  wetland = wetland * 0.01_r8


  allocate(urban_balli(nlon,nlat),urban(nlon*nlat))
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
  print*,'shape(soilw_balli):',shape(soilw_balli)


  allocate(soilw(12),asoilw(12))
  do i=1,12
     allocate(soilw(i)%fld(nlat*nlon))
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


  !sanity checks for n_s being equal to atmnx etc.?
  allocate(alake(n_b))
  alake(:) = 0.0_r8
  do i = 1, n_s !change i to longer var name with mult characters?
     alake(row(i)) =  alake(row(i)) + s(i)*lake(col(i))
  enddo
  
  allocate(awetland(n_b))
  awetland(:) = 0.0_r8
  do i = 1, n_s !change i to longer var name with mult characters?
     awetland(row(i)) =  awetland(row(i)) + s(i)*wetland(col(i))
  enddo
  
  allocate(aurban(n_b))
  aurban(:) = 0.0_r8
  do i = 1, n_s !change i to longer var name with mult characters?
     aurban(row(i)) =  aurban(row(i)) + s(i)*urban(col(i))
  enddo

  do j = 1, npft
     allocate(apft(j)%fld(n_b))
     apft(j)%fld(:) = 0.0_r8
     do i = 1, n_s !change i to longer var name with mult characters?
        apft(j)%fld(row(i)) = apft(j)%fld(row(i)) + s(i)*pft(j)%fld(col(i))
     enddo
  enddo

  do j = 1, 12 !replace 12 with a var name?
     allocate(asoilw(j)%fld(n_b))
     asoilw(j)%fld(:) = 0.0_r8
     do i = 1, n_s !change i to longer var name with mult characters?
        asoilw(j)%fld(row(i)) = asoilw(j)%fld(row(i)) + s(i)*soilw(j)%fld(col(i))
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

  call check( nf90_create(trim(outputfilename), NF90_CLOBBER, ncid) ) ! NEW NCID VAR?

  call check( nf90_def_dim(ncid, 'ncol', atmnx, dim1) )
  call check( nf90_def_dim(ncid, 'class', 11, dim2) )
  call check( nf90_def_var(ncid, 'fraction_landuse', NF90_DOUBLE, (/dim1,dim2/), varid1) )
  call check( nf90_def_dim(ncid,'month',12, dim2))
  call check( nf90_def_var(ncid,'soilw', NF90_DOUBLE, (/dim1,dim2/), varid2))

  call check( nf90_enddef(ncid) )

  call check(nf90_put_var(ncid, varid1, fraction_landuse))
  call check(nf90_put_var(ncid, varid2, total_soilw ))

  call check( nf90_close(ncid) )


contains

  subroutine openfile_and_initdecomp(filename, nx)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nx

    call check(nf90_open(filename, NF90_NOWRITE, ncid))

    call check( nf90_inq_dimid(ncid, "grid_size", dimid) )
    call check( nf90_inquire_dimension(ncid, dimid, len = nx) )
    call check(nf90_close ( ncid ))
  end subroutine openfile_and_initdecomp




  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end program mkatmsrffile




