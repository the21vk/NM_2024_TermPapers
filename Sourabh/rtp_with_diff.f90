program rtpdiffreset
implicit none
   interface!interface for random number generator for exponential distribution
    subroutine gaussain(rand1)
        real,intent(out)::rand1
    end subroutine gaussain
    end interface
integer::initial_dir,i,j
real::x_initial,gamm,vel,dt,reset_rate,target_pos,time_count,stand_dev,D
real,dimension(1000)::avg_time
x_initial=5.5
initial_dir=-1
D=0.5
gamm=1
vel=1
target_pos=6.2
dt=0.01
stand_dev=(2*D*dt)**0.5
do i=1,1000
reset_rate=(i-1)*0.01+0.001
time_count=0
do j=1,10000
time_count=time_count+(run(x_initial, gamm, vel, dt, initial_dir, target_pos, reset_rate,stand_dev)/10000)
end do
print*,time_count
avg_time(i)=time_count
end do
open(1,file="rtp_diff_neg_55.dat")
!change file name as well x_initial to get each data set plotted, neg/pos means negative or positive speed and number at the end of file name is posiition of particle without decimal
do i=1,1000
write(1,*)(i-1)*0.01+0.001,avg_time(i)
end do
close(1)
contains
function run(x_initial, gamm, vel, dt, initial_dir, target_pos, reset_rate,stand_dev) result(time)
    real :: x_initial, gamm, vel, dt, target_pos, reset_rate
    real :: time,random1
    real :: switch_time, reset_time, x, xnew,stand_dev
    integer :: i, flag,initial_dir,direction
    
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
            direction = -direction
        end if
        if (time >= reset_time) then
            reset_time = time + arrival_time(reset_rate)
            x = x_initial
            direction = initial_dir
        end if
        call random_number(random1)
        call gaussian(random1)
        xnew = x + direction * vel * dt+stand_dev*random1
        if ((xnew <= target_pos .and. x > target_pos) .or. (xnew >= target_pos .and. x < target_pos)) then
            flag = 1
        end if
        x = xnew
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
endprogram rtpdiffreset
subroutine gaussian(rand1)
    implicit none
    real::rand1,rand2
    real,parameter::pi=3.14159265359
    call random_number(rand2)
    rand1=((-2.0*log(rand1))**0.5)*cos(2.0*pi*rand2)
endsubroutine gaussian
