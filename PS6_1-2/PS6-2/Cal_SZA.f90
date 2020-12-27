program FunctionTest
use Declination_angle
use AST
implicit none

logical direct
integer dmt,year,month,date,hour,min,day,apparentst(2)
real p,da,long,lat,h,saa,sza

p=3.1415926536
write(*,*)"Please input the year:"
read (*,*) year

write(*,*)"Please input the month:"
read (*,*) month

write(*,*)"Please input the date:"
read (*,*) date

write(*,*)"Please input the hour:"
read (*,*) hour

write(*,*)"Please input the minute:"
read (*,*) min

write(*,*)"Please input the time zone(West-12----+12East):"
read (*,*) dmt

write(*,*)'In the western longitudes?(please input ".true."or".false."):'
read (*,*) direct

write(*,*)"Please input the Longitude:"
read (*,*) long

write(*,*)"Please input the Latitude:"
read (*,*) lat
day = daysinyear(year,month,date)
write(*,*) "The day in this year is:",day

da=DecAngle(year,day)
write(*,*)"The declination angle is:",da,"Deg"

apparentst= astime(long,direct,dmt,year,hour,min,day)
write(*,*)"The apparentst solar time(AST) is :",apparentst(1),":",apparentst(2)

h=((60*dble(apparentst(1))+dble(apparentst(2))) - 720)/4
write(*,*)"The hour angle(H) is:",h

saa = asin(cos(lat*p/180)*cos(da*p/180)*cos(h*p/180)+sin(lat*p/180)*sin(da*p/180))

saa = saa*180/p
write(*,*)"The altitude angle is:",saa

sza=90-saa
write(*,*)"The zenith angle(SZA) is:",sza

end program FunctionTest
