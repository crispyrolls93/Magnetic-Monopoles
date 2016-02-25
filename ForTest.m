%clears variable for a fresh start

clear all
close all

%begins a timer to see how long the program takes to run
tic

A = [1:10];

%sets initial time variables
tmax = 1;
dt = tmax * 10^(-4);
nmax = tmax/dt;

%Sets values of lamda and M
qe = 1.602 * 10 ^ (-19);
planck = 6.626 * 10 ^ (-34);
mu0 = 1.256637 * 10 ^ (-6);
qd = planck/(mu0 * qe);
qm = 4.5 * qd;
lamda = qe * qm * mu0/(2 * planck);

M = 0;

%Opens output file for potential (V)
fVTid = fopen('Vout.txt','w');

%Runs a for loop to get values of potential as a function of time and
%output to a display file and gain a value of E for the next F calculations
for n = (1:nmax)
    t = n * dt;
    V = (1/((t)^2))*(M + lamda * (1 - (1/sqrt(1 + (t^2/lamda))))^2);
    
    %Display progress to terminal
    check = 10 * n / nmax;
    if any(check==A) == 1
           disp(10*check/4)
    end 
    
    fprintf(fVTid, '%18.18f \n', [V]');
end
%Loads output file as a matrix
load Vout.txt;


%Gets the time scale for plotting
p = linspace(dt,tmax,nmax);

%Plots potential against time
figure(1)
subplot(1,2,1)
plot (p,Vout)
title('Graph of V against time')
xlabel('time')
ylabel('V')

%gains a value of E proportional to the average of V
Eav = Vout;
Esug = mean(Eav);
E = Esug;

%Opens output file for Force (F)
fFTid = fopen('Fout.txt','w');

%Runs a for loop to get values of Force as a function of time and output to a display file
for n = (1:nmax)
    t = n * dt;
    V = (1/((t)^2))*(M + lamda * (1 - (1/sqrt(1 + (t^2/lamda))))^2);
    F = V - E;
    
    %Display progress to terminal
    check = 10 * n / nmax;
    if any(check==A) == 1
           disp((10*check/4)+25)
    end 
    fprintf(fFTid, '%18.18f \n', [F]');
end

%Loads F output file as a matrix
load Fout.txt;

%Plots F against time
subplot(1,2,2)
plot (p,Fout)
title('Graph of F against time')
xlabel('time')
ylabel('F')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Forward set of iterations

%Sets initial position/velocity variables
x = 1;
v = 10;

%Opens output files
fxfid = fopen('xforout.txt','w');
fvfid = fopen('vforout.txt','w');
fafid = fopen('aforout.txt','w');

%Iterates through maths for new values of a, x and v
for n = (1:nmax)
    t = n * dt;
    V = (1/((t)^2))*(M + lamda * (1 - (1/sqrt(1 + (t^2/lamda))))^2);
    F = (V - E);
    
    a = F * x - v/t;
    v = v + a * dt;
    x = x + v * dt;    
    
    %Display progress to terminal
    check = 10 * n / nmax;
    if any(check==A) == 1
           disp(10*check/4+50)
    end 
    
    %Prints to output file
    fprintf(fxfid, '%18.18f \n', [x]');
    fprintf(fvfid, '%18.18f \n', [v]');
    fprintf(fafid, '%18.18f \n', [a]');
end

%loads output file as matrices
load xforout.txt;
load vforout.txt;
load aforout.txt;

%plots v, a and x against t
figure(2)
subplot (1,3,1);
plot (p,xforout)
title('Graph of displacement against time')
xlabel('time')
ylabel('displacement')

subplot (1,3,2);
plot (p,vforout)
title('Graph of velocity against time')
xlabel('time')
ylabel('velocity')

subplot (1,3,3);
plot (p,aforout)
title('Graph of acceleration against time')
xlabel('time')
ylabel('acceleration')

%removes the first point of a (which is far removed from the rest of the
%scale to give a viewable graph)
figure(3)
z = p(2:n);
Z = aforout(2:n);
plot(z,Z);
title('Fixed graph of acceleration against time')
xlabel('time')
ylabel('acceleration')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Reverse set of iterations

%Opens output files
fxrid = fopen('xrevout.txt','w');
fvrid = fopen('vrevout.txt','w');
farid = fopen('arevout.txt','w');

%Iterates for new values of x and v from the end of the last set of maths
for n = (nmax:-1:1)
    t = n * dt;
    V = (1/((t)^2))*(M + lamda * (1 - (1/sqrt(1 + (t^2/lamda))))^2);
    F = (V - E);
    
    a = F * x - v/t;
    v = v + a * dt;
    x = x + v * dt;    
    
    %Display progress to terminal
    check = 10 * n / nmax;
    if any(check==A) == 1
           disp(100-10*check/4)
    end 
    
    %Prints to output files
    fprintf(fxrid, '%18.18f \n', [x]');
    fprintf(fvrid, '%18.18f \n', [v]');
    fprintf(farid, '%18.18f \n', [a]');
end

%loads output files as matrices
load xrevout.txt;
load vrevout.txt;
load arevout.txt;


p = linspace(-dt,-tmax,nmax)+1;

%plots x, v and a against t
figure(4)
subplot (1,3,1);
plot (p,xrevout)
title('Graph of displacement against time')
xlabel('time')
ylabel('displacement')

subplot (1,3,2);
plot (p,vrevout)
title('Graph of velocity against time')
xlabel('time')
ylabel('velocity')

subplot (1,3,3);
plot (p,arevout)
title('Graph of acceleration against time')
xlabel('time')
ylabel('acceleration')

%plots the fixed graph of acceleration
figure(5)
z = p(2:n);
Z = arevout(2:n);
plot(z,Z);
title('Fixed graph of acceleration against time')
xlabel('time')
ylabel('acceleration')

toc