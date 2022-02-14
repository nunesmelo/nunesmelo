!write a fortran program vertices are given verify its square or Rhombus or Parallelogram or rectangle
!Author: DARKER665
program cheak_four
    implicit none
    real::x1,x2,x3,x4,y1,y2,y3,y4,a,b,c,d,p,q
    print*,'enter the value of x1 & y1'
    read*,x1,y1
    print*,'enter the value of x2 & y2'
    read*,x2,y2
    print*,'enter the value of x3 & y3'
    read*,x3,y3
    print*,'enter the value of x4 & y4'
    read*,x4,y4
    a=sqrt((x1-x2)**2+(y1-y2)**2)
    b=sqrt((x2-x3)**2+(y2-y3)**2)
    c=sqrt((x3-x4)**2+(y3-y4)**2)
    d=sqrt((x4-x1)**2+(y4-y1)**2)
    p=sqrt((x1-x3)**2+(y1-y3)**2)
    q=sqrt((x2-x4)**2+(y2-y4)**2)
if(a==c .and. b==d .and. a/=b .and. p==q ) then
    print*,'this is rectangle'

    else if(a==c .and. b==d .and. a/=b .and. p/=q ) then
    print*,'this is Parallelogram'

    else if( a==b .and. b==c .and. c==d .and. d==a .and. p==q ) then
    print*,'this is square'

    else if( a==b .and. b==c .and. c==d .and. d==a .and. p/=q ) then
    print*,'this is Rhombus'
    else
        print*,'none of them'
    end if
end program cheak_four