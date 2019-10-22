!---------------------------- Pràctica 3 ----------------------------------!
! Autor: Javier Rozalén Sarmiento
! Grup: B1B
! Data: 22/10/2019
!
! Funcionalitat: es desenvolupen els algorismes de la bisecció i 
! de Newton-Raphson i comprova la seva eficàcia mitjançant exemples.

program practica3
implicit none
double precision x,y,D,vectE(100),vectD(100),Dprima(100),E
double precision x1,x2,xarrel,precisio ! Extrems per la bisecció i variable auxiliar
double precision precisio2 ! Precisio per Newton-Rapshon
double precision a,epsilon,G,Eo,dfu,vect1,fu
double precision t
integer niter,i
external F
external G

common/dades/t
a=17.857619
epsilon=0.967990

!---------------- Apartat 1 ------------------!
! Escrivim els vectors amb els punts equiespaiats E i D(E)
do i=1,100
	E = (i-1)*((2*acos(-1.d0))/100)
	call coord(x,y,E) ! Retorna les coordenades
	call dist_origen(x,y,D) ! Retorna D(x,y)
	vectE(i)=E
	vectD(i)=D
enddo

call derfun(100,vectE,vectD,Dprima) ! Retorna el vector Dprima
open(20,file="P3-1920-res.dat")
do i=1,100
	write(20,*) vectE(i),vectD(i),Dprima(i)
enddo
write(20,*) ""
write(20,*) ""

!---------------- Apartat 2 ------------------!
x1=0.1d0
x2=5.8d0
precisio=1.d-10
! En vista de que F(E) només té una arrel, es farà una única iteració
call Bisection(x1,x2,precisio,F,niter,xarrel)
call coord(x,y,xarrel) ! Retorna x(E),y(E)
call dist_origen(x,y,D) ! Retorna D(x,y)
write(20,*) xarrel,D
write(20,*) ""
write(20,*) ""

!---------------- Apartat 3 ------------------!
precisio2=1.d-12
Eo = acos(-1.d0)/4.d0
do i=1,80
	t=(i-1)*(75.3d0/80.d0)
	call NewtonRap(Eo,precisio2,G,niter,xarrel) ! Retorna xarrel
	call coord(x,y,xarrel) ! Calcula x(E),y(E) amb el valor de E obtingut amb N-R
	write(20,*) t,xarrel,x,y
enddo

close(20)
end program practica3

! Subrutina auxiliar que calcula x(E),y(E)
subroutine coord(x,y,E)
	implicit none
	double precision x,y,E,a,epsilon
	a=17.857619
	epsilon=0.967990
	x=a*(cos(E)-epsilon)
	y=a*sin(E)*(1-epsilon**2)**0.5d0
	return
end subroutine coord

! Funció D(x(E),y(E))
subroutine dist_origen(x,y,D) ! Output--> D
	implicit none
	double precision x,y,D
	D=(x**2+y**2)**0.5d0
	return
end subroutine dist_origen

! Subrutina NewtonRap --> retorna la posició d'un zero d'una funció donada
subroutine NewtonRap(x0,eps,function,niter,xarrel)
    implicit none
    double precision x0,x1,eps,xarrel,fu,dfu
    integer niter
    niter = 1
12  call function(x0,fu,dfu)
    x1 = x0-(fu/dfu)
    if (abs(x1-x0).le.eps) then
        xarrel = x1
    else
        x0 = x1
        niter = niter+1
        goto 12
    endif
    return
end subroutine NewtonRap

! Subrutina Bisection --> retorna la posició d'un zero d'una funció donada
subroutine Bisection(A,B,eps,function,niter,xarrel) ! Output --> xarrel
    implicit none
    double precision A,B,eps,C,fu,dfu,fa,fb,fc,xarrel
    integer niter,maxiter,i
    maxiter = nint(log((B-A)/eps)/log(2.d0)) ! Nombre màxim d'iteracions
    ! Algoritme de la bisecció
    do i=1,maxiter-1    
        call function(A,fu,dfu)
        fa = fu
        call function(B,fu,dfu)
        fb = fu
    ! Hi ha canvi de signe? Si és així, seguim
        if ((fa*fb).lt.0.d0) then
            C = (A+B)/2.d0
    ! L'interval és tant petit com es volia? Si és així, parem
            if ((B-A).lt.eps) then
                niter = i
                xarrel = C
                exit
            else
                call function(C,fu,dfu)
                fc = fu
    ! Hem trobat la solució exacta? Si és així, parem         
                if (fc.eq.0.d0) then 
                    niter = i
                    xarrel = C
                    exit
                else
    ! En cas contrari, continuem iterant                    
                    if ((fa*fc).lt.0.d0) then
                        B = C
                    else if ((fc*fb).lt.0.d0) then
                        A = C
                    else
                        print*,"Algo ha anat molt malament noi, fes-t'ho mirar."
                    endif
                endif
            endif
        else
            print*,"No hi ha canvi de signe en l'interval donat."
            exit
        endif
    enddo
    xarrel = C
    niter = i
    print*, "Bisecció"
    print*, "Nombre màxim d'iteracions:",maxiter
    print*, "Nombre d'iteracions: ",niter
    print*, "Arrel: ",xarrel
    return
end subroutine Bisection

! Subrutina derfun --> retorna la derivada d'una funció en un interval de punts
subroutine derfun(ndat,x,fu,der)
    implicit none
    double precision x(ndat),fu(ndat),der(ndat)
    integer ndat,i
    do i=1,ndat
        if (i.eq.1) then
            der(i)=(fu(i+1)-fu(i))/(x(i+1)-x(i))
        else if (i.eq.ndat) then
            der(i)=(fu(i)-fu(i-1))/(x(i)-x(i-1))
        else
            der(i)=(fu(i+1)-fu(i-1))/(x(i+1)-x(i-1))
        endif
    enddo
    return 
end subroutine derfun

! Subrutina fun --> retorna els valors numèrics de f(x) i f'(x)
subroutine F(E,fu)
    implicit none
    double precision E,fu,a,epsilon
    a=17.857619
	epsilon=0.967990
    fu = sin(2*E)*(1-epsilon**2)-sin(E)*(cos(E)*(2.d0-epsilon**2)-epsilon)
    return 
end subroutine F

! Subrutina G --> funció anomalia excèntrica
subroutine G(E,fu,dfu)
    implicit none
    double precision E,fu,Th,epsilon,dfu,t
    common/dades/t
    epsilon=0.967990
    Th=73.5d0
    fu = E-t*(2*acos(-1.d0))/Th+sin(E)
    dfu = 1+epsilon*cos(E)
    return 
end subroutine G


