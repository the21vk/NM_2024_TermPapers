program rtpreset
implicit none
integer::initial_dir,i,j
real::x_initial,gamm,vel,dt,reset_rate,target_pos,time_count
real,dimension(400)::avg_time
x_initial=5.5
initial_dir=-1
gamm=1
vel=1
target_pos=6.2
dt=0.01
do i=1,400
reset_rate=(i-1)*0.01+0.001
time_count=0
do j=1,10000
time_count=time_count+(run(x_initial, gamm, vel, dt, initial_dir, target_pos, reset_rate)/10000)
end do
print*,time_count
avg_time(i)=time_count
end do
open(1,file="rtp_neg_5.5.dat")
!change file name as well x_initial to get each data set plotted, neg/pos means negative or positive speed and number at the end of file name is posiition of particle without decimal
do i=1,400
write(1,*)(i-1)*0.01+0.001,avg_time(i)
end do
close(1)
contains
function run(x_initial, gamm, vel, dt, initial_dir, target_pos, reset_rate) result(time)
    real :: x_initial, gamm, vel, dt, target_pos, reset_rate
    real :: time
    real :: switch_time, reset_time, x, xnew
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
        xnew = x + direction * vel * dt
        if ((xnew <= target_pos .and. x > target_pos) .or. (xnew >= target_pos .and. x < target_pos)) then
            flag = 1
        end if
        x = xnew
        if (time>1000) then
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
