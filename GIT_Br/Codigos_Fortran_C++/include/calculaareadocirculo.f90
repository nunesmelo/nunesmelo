!author:DARKER665
!The program finds area,circumference  and vol of a circle
program area_vol
    implicit none
    real::area,cir,vol,r
    real,parameter::pi=3.1416
    print*,'Enter the value of radus='
    read*,r
    area=pi*r**2
    vol=(4.0/3.0)*pi*r**3
    cir=2*pi*r
    print*,'area of the circle=',area
    print*,'volume of the circle=',vol
    print*,'circumference of the circle=',cir
end program area_vol