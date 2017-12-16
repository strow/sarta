function [vchan,ichan,stempjac,cldjac,Tjac,WVjac,O3jac] = read_sarta_jacob(fjacob);

w        = [];
cldjac   = [];
stempjac = [];
Tjac  = [];
WVjac = [];
O3jac = [];

[fid,msg] = fopen(fjacob, 'r', 'ieee-be'); %%% GAHHHHHRRR
if fid == -1
  error(['error opening input file\n', msg]);
end

%% read header info
flen   = fread(fid, 1, 'integer*4');
numprof = fread(fid, 1, 'integer*4');
nchan   = fread(fid, 1, 'integer*4');
flen   = fread(fid, 1, 'integer*4');
fprintf(1,'there are %5i profiles, and %4i chans in %s \n',numprof,nchan,fjacob);

flen   = fread(fid, 1, 'integer*4');
dST = fread(fid, 1, 'real*4');
dQ  = fread(fid, 1, 'real*4');
flen   = fread(fid, 1, 'integer*4');
fprintf(1,'dST, dQ = %8.6f %8.6f \n',dST,dQ)

stempjac = zeros(nchan,numprof);
cldjac   = zeros(4,nchan,numprof);
Tjac     = zeros(100,nchan,numprof);
WVjac    = zeros(100,nchan,numprof);
O3jac    = zeros(100,nchan,numprof);

flen   = fread(fid, 1, 'integer*4');
ichan  = fread(fid,nchan,'integer*4');
flen   = fread(fid, 1, 'integer*4');

flen   = fread(fid, 1, 'integer*4');
vchan  = fread(fid,nchan,'real*4');
flen   = fread(fid, 1, 'integer*4');

disp('read in ichan,vchan')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ii = 1 : numprof
  %%%%%%%%%%%%
  %% stemp jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading stemp jac ixprof ~= ii')
%  end
  if itype ~= 1
    [itype]
    error('reading stemp jac itype ~= 1')
  end
  flen = fread(fid, 1, 'integer*4');
  xjac = fread(fid, nchan, 'real*4');
  flen = fread(fid, 1, 'integer*4');
  
  stempjac(:,ii) = xjac';  

  %%%%%%%%%%%%
  %% cngwat1 jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading stemp jac ixprof ~= ii')
%  end
  if itype ~= 11
    [itype]
    error('reading stemp jac itype ~= 11')
  end
  flen = fread(fid, 1, 'integer*4');
  xjac = fread(fid, nchan, 'real*4');
  flen = fread(fid, 1, 'integer*4');
  
  cldjac(1,:,ii) = xjac';  

  %%%%%%%%%%%%
  %% cngwat2 jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading stemp jac ixprof ~= ii')
%  end
  if itype ~= 12
    [itype]
    error('reading stemp jac itype ~= 12')
  end
  flen = fread(fid, 1, 'integer*4');
  xjac = fread(fid, nchan, 'real*4');
  flen = fread(fid, 1, 'integer*4');
  
  cldjac(2,:,ii) = xjac';

  %%%%%%%%%%%%
  %% cpsize1 jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading stemp jac ixprof ~= ii')
%  end
  if itype ~= 21
    [itype]
    error('reading stemp jac itype ~= 21')
  end
  flen = fread(fid, 1, 'integer*4');
  xjac = fread(fid, nchan, 'real*4');
  flen = fread(fid, 1, 'integer*4');
  
  cldjac(3,:,ii) = xjac';  

  %%%%%%%%%%%%
  %% cpsize1 jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading stemp jac ixprof ~= ii')
%  end
  if itype ~= 22
    [itype]
    error('reading stemp jac itype ~= 22')
  end
  flen = fread(fid, 1, 'integer*4');
  xjac = fread(fid, nchan, 'real*4');
  flen = fread(fid, 1, 'integer*4');

  cldjac(4,:,ii) = xjac';  

  %%%%%%%%%%%%
  %% T jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading T(z) jac ixprof ~= ii')
%  end
  if itype ~= -100
    [itype]
    error('reading T(z) jac itype ~= -100')
  end
  for jj = 1 : 100
    flen = fread(fid, 1, 'integer*4');
    xjac = fread(fid, nchan, 'real*4');
    flen = fread(fid, 1, 'integer*4');
    Tjac(jj,:,ii) = xjac';  
  end

  %%%%%%%%%%%%
  %% WV jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading WV(z) jac ixprof ~= ii')
%  end
  if itype ~= +100
    [itype]
    error('reading WV(z) jac itype ~= +100')
  end
  for jj = 1 : 100
    flen = fread(fid, 1, 'integer*4');
    xjac = fread(fid, nchan, 'real*4');
    flen = fread(fid, 1, 'integer*4');
    WVjac(jj,:,ii) = xjac';  
  end

  %%%%%%%%%%%%
  %% O3 jac
  flen = fread(fid, 1, 'integer*4');
  ixprof = fread(fid, 1, 'integer*4');
  itype   = fread(fid, 1, 'integer*4');
  flen = fread(fid, 1, 'integer*4');
%  if ixprof ~= ii
%    [ixprof ii]
%    error('reading O3(z) jac ixprof ~= ii')
%  end
  if itype ~= +300
    [itype]
    error('reading O3(z) jac itype ~= +300')
  end
  for jj = 1 : 100
    flen = fread(fid, 1, 'integer*4');
    xjac = fread(fid, nchan, 'real*4');
    flen = fread(fid, 1, 'integer*4');
    O3jac(jj,:,ii) = xjac';  
  end

end

fclose(fid);
