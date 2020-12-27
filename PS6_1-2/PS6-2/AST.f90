module AST

implicit none

real,parameter ::pi=3.1415926536

contains

function astime(long,direct,dmt,year,hour,min,day)
logical direct
integer astime(2),dmt,year,hour,min,day,lstm,temp
real d,et,long,tt

if (((mod(year,4)==0).and.(mod(year,100)/=0)) .or. (mod(year,400)==0)) then
d=360*(dble(day)-81)/366
else
d=360*(dble(day)-81)/365
end if
et=9.87*sin(2*d*pi/180)-7.53*cos(d*pi/180)-1.5*sin(d*pi/180)
if (direct.eqv..true.) then
long=-long
else
long=long
endif
lstm=15*dmt
temp=4*(long-lstm)+et
astime(1)=hour+int(temp/60)
astime(2)= min+mod(temp,60)

if (astime(2)>60) then
astime(1) = astime(1) +1
astime(2) = astime(2)-60
elseif(astime(2)<0) then
astime(1)= astime(1)+1
astime(2)= astime(2)-60
elseif(astime(2)<0) then
astime(1)=astime(1)-1
astime(2)=60+astime(2)
else
astime(1)=astime(1)
astime(2)=astime(2)
endif
if (astime(1)>=24) then
astime(1)=astime(1)-24
else
astime(1)=astime(1)
endif
print*,"D =",d
print*,"ET =",et
print*,"LSTM =",lstm
end function astime
end module AST
