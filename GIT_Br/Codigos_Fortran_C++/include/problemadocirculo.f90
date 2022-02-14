!write a fortran program to examine the point x1,y1 lies inside/outside/on circle
!Author: DARKER665
program area_circle
    implicit none
    real::x,y,x1,y1,h,k,r,s
    print*,'enter the value of x,y'
    read*,x,y
    print*,'enter the value of h,k'
    read*,h,k
    print*,'enter the value of x1,y1'
    read*,x1,y1
    
    r=sqrt((x-h)**2+(y-k)**2)
    s=sqrt((x1-h)**2+(y1-k)**2)
    
    if(r==s) then
    print*,'the point lies on the circle'
    elseif(r>s) then
    print*,'the point lies inside of the circle'
    else
    print*,'the point lies outside of the circle'
    end if
    end program area_circle