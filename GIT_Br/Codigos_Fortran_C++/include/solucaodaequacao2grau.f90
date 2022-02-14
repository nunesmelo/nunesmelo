!author: DARKER665
!ax^2+bx+c=0
!The program finds any roots of 2nd degree Equation
program roots_eqn_any
    implicit none
    real::a,b,c,root1,root2,d,p,q
    print*,'enter the value of a,b,c'
    read*,a,b,c
    d=b**2-4.0*a*c
    if(d>=0.00) then
        root1=-b+sqrt(d)
        root2=-b-sqrt(d)
    print*,'root one is=',root1
    print*,'root two is=',root2
    else
        p=-b/(2.0*a)
        q=sqrt(abs(d))
        print*,'1st root',p,'+i',q
        print*,'2st root',p,'-i',q

    end if
end program roots_eqn_any