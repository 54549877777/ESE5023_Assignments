module Declination_angle

implicit none

real,parameter:: pi = 3.1415926536

contains
integer function daysinyear(year,month,day)
integer::year,month,day
integer:: daysinmonth(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
if (((mod(year,4)==0).and.(mod(year,100)/=0)).or.(mod(year,400)==0)) then
daysinmonth(2)=29
else
daysinmonth(2)=28
end if
daysinyear = sum(daysinmonth(:month-1))+day

end function daysinyear

real function decangle (year,n)
integer year,n
real temp

if (((mod(year,4)==0).and.(mod(year,100)/=0)).or.(mod(year,400)==0)) then
temp = (n+284)*360*pi/(366*180)
else
temp = (n+284)*360*pi/(365*180)
end if
decangle = 23.45*sin(temp)
end function decangle
end module Declination_angle
