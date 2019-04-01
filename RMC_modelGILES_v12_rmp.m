% Script to run MyLake (v_12) for GILES 
% Used in E-project Pcode
% by TSA, last modified 23.02.2005
% updated by RMP, for GitHub (2019-Mar-27)
    
clear all;
path(path,'MyLake_public\v12\air_sea') %path for air-sea toolbox
path(path,'MyLake_public\v12\v12_1') %path for MyLake model code

global ies80 Eevapor;
test_time=0;
Eevapor=0;

load 'Observations\GILESice.dat'
load 'Observations\GILEStemp_all.dat' 

[Obs_TP_Chla, trash]=xlsread('Observations\GILES_TP_Chla1989_2018.xls');

lake='Giles';
year=2016;
m_start=[2016,05,17];
m_stop=[2018,12,31];

initfile='GILES_init_v12.xls';
parafile='GILES_para_v12.xls';
inputfile='GILES_input_1997-2018-wInflowScenario1.xls';

tic
        [zz,Az,Vz,tt,Qst,Kzt,Tzt,Czt,Szt,Pzt,Chlzt,PPzt,DOPzt,DOCzt,Qzt_sed,lambdazt,...
        P3zt_sed,P3zt_sed_sc,His,DoF,DoM,MixStat,Wt]...
           = solvemodel_v12_1b_ut(m_start,m_stop,initfile,'lake',inputfile,'timeseries', parafile,'lake');    
run_time=toc

DoF_realtime=DoF+tt(1)-1; %TSA, antatt at tidsteg er 1 dag
DoM_realtime=DoM+tt(1)-1; %TSA
DoF_plottime=DoF+tt(1)-1-datenum(year,1,1); %TSA, antatt at tidsteg er 1 dag
DoM_plottime=DoM+tt(1)-1-datenum(year,1,1); %TSA

tt_mod = tt - datenum(year,1,1); %time now scaled so that it begins from the 1 january of the "year" (=0)

% %=Ice thickness observations (tt_mod, Hice (m))
IceObs=[datenum(fliplr(GILESice(:,1:3))) - datenum(year,1,1),GILESice(:,4)./100];
inx=find((IceObs(:,1)<(datenum(m_start)- datenum(year,1,1)))|(IceObs(:,1)>(datenum(m_stop)- datenum(year,1,1))));
IceObs(inx,:)=[];

%=Temperature profile observations (tt_mod, z, T)
Datestr=num2str(GILEStemp_all(:,1));
Dummydate=[str2num(Datestr(:,1:4)),str2num(Datestr(:,5:6)),str2num(Datestr(:,7:8))];
TempObs=[datenum(Dummydate) - datenum(year,1,1), -GILEStemp_all(:,3)./100, GILEStemp_all(:,4)];

%=align temperature observations with model results
alku=[1;find(diff(TempObs(:,1))~=0)+1];
loppu=[find(diff(TempObs(:,1))~=0); length(TempObs)];

for i=1:length(alku)
inxt=find(tt_mod==TempObs(alku(i),1));
    if (isempty(inxt)==0)
    TempMod(alku(i):loppu(i))=interp1(zz,Tzt(:,inxt),TempObs(alku(i):loppu(i),2));
    else
    TempMod(alku(i):loppu(i))=NaN;    
    end    
end

zlim = [0 max(zz)];
tlim = [min(tt_mod) max(tt_mod)];

csvwrite("ModelledTemp-Giles_modelOutput.csv",Tzt)
csvwrite("ModelledHis-Giles_modelOutput.csv",His)

