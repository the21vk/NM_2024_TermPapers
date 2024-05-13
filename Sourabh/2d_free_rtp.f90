program rtpreset
implicit none
integer::i,j
real::gamm,vel,dt,reset_rate,time_count,initial_dir,rad_square
real,dimension(200)::avg_time
real,dimension(2)::x_initial=[0.0,0.9]
real,dimension(2)::centre=[0,3]
real,parameter::pi=3.14159265359
rad_square=1
initial_dir=-pi/2
gamm=1
vel=1
dt=0.01
do i=1,200
reset_rate=(i-1)*0.01+0.001
time_count=0
do j=1,10000
time_count=time_count+(run(x_initial, gamm, vel, dt, initial_dir, centre, reset_rate,rad_square)/10000)
end do
print*,time_count
avg_time(i)=time_count
end do
open(11,file="rtp_2d_neg_09.dat")
!change file name as well x_initial to get each data set plotted, neg/pos means negative or positive speed and number at the end of file name is posiition of particle without decimal 
do i=1,200
write(11,*)(i-1)*0.01+0.001,avg_time(i)
end do
close(11)
contains
function run(x_initial, gamm, vel, dt, initial_dir, centre, reset_rate,rad_square) result(time)
    real :: gamm, vel, dt, reset_rate
    real :: time,random1
    real :: switch_time, reset_time,rad_square,direction,initial_dir
    integer :: i, flag
    real,dimension(2)::x_initial
	real,dimension(2)::x
	real,dimension(2)::xnew
	real,dimension(2)::centre
	real,parameter::pi=3.14159265359
    switch_time = arrival_time(gamm)
    reset_time = arrival_time(reset_rate)
    direction = initial_dir
    i = 0
    time = 0.0
    x = x_initial
    flag = 0
    
    do while (flag == 0)
        i = i + 1
        time = i * dt
        if (time >= switch_time) then
            switch_time = time + arrival_time(gamm)
            call random_number(random1)
            direction =2.0*pi*random1
        end if
        if (time >= reset_time) then
            reset_time = time + arrival_time(reset_rate)
            x = x_initial
            direction = initial_dir
            !print*,reset_time
        end if
        xnew(1)=x(1)+COS(direction)*vel*dt
        xnew(2)=x(2)+SIN(direction)*vel*dt
        if ((xnew(1)-centre(1))**2+(xnew(2)-centre(2))**2<=rad_square) then
            flag = 1
        end if
        x=xnew
        x(1)=MIN(6.0, MAX(-6.0, x(1)))
        x(2)=MIN(6.0, MAX(-6.0, x(2)))
        !ignorning very high times for efficieny, also sometimes due to pseudo random numbers looping, infinte loop will be created so to curb that this is necessary
        if (time>10000) then
        flag=1
        endif
    end do
    
end function run

function arrival_time(poisson_rate) result(arrive_time)
implicit none
    real::arrive_time
    real,intent(in)::poisson_rate 
    real::u
    ! Generate a random number between 0 and 1
    call random_number(u)
    ! Transform the random number to an exponential random variable
    arrive_time=-log(u)/poisson_rate
end function arrival_time
endprogram rtpreset
