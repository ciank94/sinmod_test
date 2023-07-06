program init_part
  implicit none
  !First make sure to use parameters as they are used in track.f90
  !Then, create a grid using xlim and ylim vector
  real,dimension(2)::xlim,ylim
  real::xstep,ystep
  real,dimension(:),allocatable::x0,y0
  integer::nsites = 0,tsites
  real::tn_sites
  integer::i,j
  print*, "tn_sites"
  read(*,*) tn_sites
  tsites = sqrt(tn_sites)
  print*,tsites
  print*,"xlim(1)"
  read(*,*) xlim(1)
  print*,"xlim(2)"
  read(*,*) xlim(2)

   print*,"ylim(1)"
  read(*,*) ylim(1)
  print*,"ylim(2)"
  read(*,*) ylim(2)
  
  xstep = (xlim(2)-xlim(1))/(tsites)
  ystep = (ylim(2)-ylim(1))/(tsites)
   allocate(x0(tsites**2),y0(tsites**2))
  do i = 1,tsites
     do j = 1,tsites
        nsites=nsites+1
        x0(nsites) = xlim(1) + ((i)*xstep)
        y0(nsites) = ylim(1) + ((j)*ystep)
        ! print*, nsites, x0, y0
     end do
  end do
  open(1,file='output.txt',status='replace',action='write')
  write(1,*) 'x0, y0'
  do i = 1,tsites**2
     write(1,*) x0(i), ',', y0(i)
  end do
  
  
  !In that grid initialize n particles that are equally spaced
  !Consider using reshape to change to a one dimensional looping over coordinates within xlim and ylim boundaries. (length(reshape xygrid)/length(total_part))
  !Then put the data in a format that can easily be copied to matlab- probably just save output as individual text files.
end program init_part
