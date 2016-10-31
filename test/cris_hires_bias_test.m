function [fh] = cris_hires_bias_test(rtp_input,kcarta_truth,fout);

% Below needed by atom.ios
cd ~/Work/Rta/sarta/test

% The matlib package is on github at:
% https://github.com/strow/matlib
addpath /asl/matlib/h4tools
addpath /asl/matlib/aslutil

% SARTA executable
sarta_exec = '../bin/crisg4_oct16';

% Symbolic link input file to rtpin.rtp
% rtp_input = 'rtp_drivers/regr_rtp_6angs_49profs_1013mb_seaemis.rtp';
unix(['rm rtpin.rtp']);
unix(['ln -s '  rtp_input ' rtpin.rtp']);

% kCARTA truth files
% kcarta_truth = 'kcarta_truth/kcarta_crisHI_1013mb_seaemiss.mat';
% SARTA output file
% fout = 'cris_test_output.rtp';

% SARTA run script
%sartarun = [sarta_exec ' fin=rtpin.rtp fout=' fout ' > /dev/null'];
sartarun = [sarta_exec ' fin=rtpin.rtp fout=' fout ];

% Run SARTA
tic
unix(sartarun);
toc

% SARTA output
[h,ha,p,pa] = rtpread(fout);

% Subsest profiles, don't use 6th secant angle, too high
ip = 1:(5*49);
ip = 1:length(p.rlat);

% Sort by wavenumber for comparison to kcarta output and delete last secant
[b,i]=sort(h.vchan);
btcal = rad2bt(h.vchan(i),p.rcalc(i,ip));

% Sergio's kcarta output
load(kcarta_truth);

% Truth BT
btk = rad2bt(fcris,rcris_all(:,ip));

% Bias (truth - regression)
bias = btk - btcal;

% Sorted frequencies
f = h.vchan(i);

fh = figure;
h1 = subplot(211);
plot(f,nanmean(bias,2));hold on;grid;
ylabel('Bias in K');
h2 = subplot(212);
plot(f,nanstd(bias,0,2));hold on;grid;
ylabel('Std in K')
xlabel('Wavenumber')
adjust21(h1,h2,'even');
linkaxes([h1 h2],'x');
xlim([650 2552]);

% % Use indices below for plotting individual secants
% s = unique(p.satzen(ip));
% k1 = find( p.satzen(ip) == s(1));
% k2 = find( p.satzen(ip) == s(2));
% k3 = find( p.satzen(ip) == s(3));
% k4 = find( p.satzen(ip) == s(4));
% k5 = find( p.satzen(ip) == s(5));<
