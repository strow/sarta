clear all
tic
[h,ha,p0,pa] = rtpread('junk135_2014_02_08.rtp');

addpath /home/sergio/MATLABCODE/matlib/clouds/sarta
addpath /home/sergio/MATLABCODE/CRODGERS_FAST_CLOUD
addpath /home/sergio/MATLABCODE/CLOUD
g = dogoodchan;
clist = get_retrieval_chans(h,g,4,2);
clist = [clist; 2321; 2333];
[h,p0] = subset_rtp_allcloudfields(h,p0,[],clist,[]);
rtpwrite('junk135_2014_02_08_421chans.rtp',h,ha,p0,pa);

rtpwrite('junkjunk.rp.rtp',h,ha,p0,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
r0 = pjunk.rcalc;

p = p0;
p.stemp = p.stemp + 1;
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rST = pjunk.rcalc;

p = p0;
p.cngwat = p.cngwat*(1+0.1);
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rcngwat1 = pjunk.rcalc;

p = p0;
p.cngwat2 = p.cngwat2*(1+0.1);
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rcngwat2 = pjunk.rcalc;

p = p0;
p.cpsize = p.cpsize*(1+0.1);
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rcpsize1 = pjunk.rcalc;

p = p0;
p.cpsize2 = p.cpsize2*(1+0.1);
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rcpsize2 = pjunk.rcalc;

for ii = 1 : 100
  fprintf(1,'TEMP  %2i \n',ii)
  p = p0;
  p.ptemp(ii,:) =  p.ptemp(ii,:) + 1;
  rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
  dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
  eval(dojac)
  [hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
  raaT(ii,:,:) = pjunk.rcalc;
end

for ii = 1 : 100
  fprintf(1,'WATER %2i \n',ii)
  p = p0;
  p.gas_1(ii,:) =  p.gas_1(ii,:)*1.1;
  rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
  dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
  eval(dojac)
  [hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
  raaWV(ii,:,:) = pjunk.rcalc;
end

for ii = 1 : 100
  fprintf(1,'OZONE %2i \n',ii)
  p = p0;
  p.gas_3(ii,:) =  p.gas_3(ii,:)*1.1;
  rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
  dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
  eval(dojac)
  [hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
  raaO3(ii,:,:) = pjunk.rcalc;
end

wtime = toc;
fprintf ( 1, '  finite diff jac took %f seconds to run.\n', wtime );

error('finished finite diff')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iDo = -1
if iDo > 0
  dojac = ['!time a.out fin=junk135_2014_02_08_421chans.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh; more newugh'];
  dojac = ['!time a.out fin=junk135_2014_02_08_421chans.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh'];
  eval(dojac)
end

tic
iStart = 1; iEnd = iStart + 25 -1;
iCnt = 0;
while iStart <= 135
  iCnt = iCnt + 1;
  fjacobX = ['/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_' num2str(iStart) '_' num2str(iEnd)];    
  [vchan,ichan,stempjacX,cldjacX,TjacX,WVjacX,O3jacX] = read_sarta_jacob(fjacobX);
  if iCnt == 1
    stempjac = stempjacX;
    cldjac = cldjacX;
    Tjac = TjacX;
    WVjac = WVjacX;
    O3jac = O3jacX;
  else
    stempjac = [stempjac stempjacX];
    cldjac = cat(3,cldjac,cldjacX);
    Tjac   = cat(3,Tjac,TjacX);
    WVjac  = cat(3,WVjac,WVjacX);
    O3jac  = cat(3,O3jac,O3jacX);
  end
  iStart = iStart + 25;
  iEnd   = min(iStart + 25 - 1,length(p0.stemp));
end
readtime = toc;
fprintf ( 1, '  reading the SARTA direct finite diff jac took %f seconds to run.\n', readtime );

%fjacob = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat';
%[vchan,ichan,stempjac,cldjac,Tjac,WVjac,O3jac] = read_sarta_jacob(fjacob);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% compare stemp jac
stempjacOld = rad2bt(vchan,rST)-rad2bt(vchan,r0);
stempjacNew = rad2bt(vchan,stempjac)-rad2bt(vchan,r0);
figure(1); plot(vchan,stempjacOld); figure(2); plot(vchan,stempjacNew); figure(3); plot(vchan,stempjacOld-stempjacNew);

%% compare cngwat
cldjacOld = rad2bt(vchan,rcngwat1)-rad2bt(vchan,r0);
cldjacNew = rad2bt(vchan,squeeze(cldjac(1,:,:)))-rad2bt(vchan,r0);
figure(1); plot(vchan,cldjacOld); figure(2); plot(vchan,cldjacNew); figure(3); plot(vchan,cldjacOld-cldjacNew);

%% compare cngwat2
cldjacOld = rad2bt(vchan,rcngwat2)-rad2bt(vchan,r0);
cldjacNew = rad2bt(vchan,squeeze(cldjac(2,:,:)))-rad2bt(vchan,r0);
figure(1); plot(vchan,cldjacOld); figure(2); plot(vchan,cldjacNew); figure(3); plot(vchan,cldjacOld-cldjacNew);

%% compare cpsize
cldjacOld = rad2bt(vchan,rcpsize1)-rad2bt(vchan,r0);
cldjacNew = rad2bt(vchan,squeeze(cldjac(3,:,:)))-rad2bt(vchan,r0);
figure(1); plot(vchan,cldjacOld); figure(2); plot(vchan,cldjacNew); figure(3); plot(vchan,cldjacOld-cldjacNew);

%% compare cpsize
cldjacOld = rad2bt(vchan,rcpsize2)-rad2bt(vchan,r0);
cldjacNew = rad2bt(vchan,squeeze(cldjac(4,:,:)))-rad2bt(vchan,r0);
figure(1); plot(vchan,cldjacOld); figure(2); plot(vchan,cldjacNew); figure(3); plot(vchan,cldjacOld-cldjacNew);

%% compare T jacs for profile 1
iProf = 1; nlays = p0.nlevs(iProf)-1;
TjacNew = squeeze(Tjac(:,:,iProf)); TjacNew = TjacNew'; TjacNew = rad2bt(vchan,TjacNew)-rad2bt(vchan,r0(:,iProf))*ones(1,100);
for ii = 1 : 100
  TjacOld(:,ii) = rad2bt(vchan,squeeze(raaT(ii,:,iProf))')-rad2bt(vchan,r0(:,iProf));
end
figure(1); pcolor(vchan,1:nlays,TjacOld(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); cx = caxis;
figure(2); pcolor(vchan,1:nlays,TjacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); caxis(cx);
figure(3); plot(h.vchan,TjacOld(:,81),'.-',h.vchan,TjacNew(:,81)); title('lay 81')
figure(4); plot(h.vchan,TjacOld(:,nlays-2),'.-',h.vchan,TjacNew(:,nlays-2)); title(['lay ' num2str(nlays-2)])
figure(5); plot(h.vchan,TjacOld(:,nlays-1),'.-',h.vchan,TjacNew(:,nlays-1)); title(['lay ' num2str(nlays-1)])
figure(6); plot(h.vchan,TjacOld(:,nlays-0),'.-',h.vchan,TjacNew(:,nlays-0)); title(['lay ' num2str(nlays-0)])
xyz = squeeze(Tjac(:,:,iProf)); xyz = xyz'; xyz = rad2bt(vchan,xyz(:,nlays));
figure(7); plot(h.vchan,rad2bt(h.vchan,r0(:,iProf)),'.-',h.vchan,xyz); title(['calculated rads0 and at lay ' num2str(nlays-0)])
figure(7); pcolor(h.vchan,1:nlays,TjacOld(:,1:nlays)'-TjacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
figure(7); dJ = TjacOld(:,1:nlays)'-TjacNew(:,1:nlays)';
figure(7); dJ = 100*dJ ./(TjacNew(:,1:nlays)'+eps);  pcolor(h.vchan,1:nlays,dJ); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
  caxis([-1 +1]); colorbar
  
WVjacNew = squeeze(WVjac(:,:,iProf)); WVjacNew = WVjacNew'; WVjacNew = rad2bt(vchan,WVjacNew)-rad2bt(vchan,r0(:,iProf))*ones(1,100);
for ii = 1 : 100
  WVjacOld(:,ii) = rad2bt(vchan,squeeze(raaWV(ii,:,iProf))')-rad2bt(vchan,r0(:,iProf));
end
figure(1); pcolor(vchan,1:nlays,WVjacOld(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); cx = caxis;
figure(2); pcolor(vchan,1:nlays,WVjacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); caxis(cx);
figure(3); plot(h.vchan,WVjacOld(:,81),'.-',h.vchan,WVjacNew(:,81)); title('lay 81')
figure(4); plot(h.vchan,WVjacOld(:,nlays-2),'.-',h.vchan,WVjacNew(:,nlays-2)); title(['lay ' num2str(nlays-2)])
figure(5); plot(h.vchan,WVjacOld(:,nlays-1),'.-',h.vchan,WVjacNew(:,nlays-1)); title(['lay ' num2str(nlays-1)])
figure(6); plot(h.vchan,WVjacOld(:,nlays-0),'.-',h.vchan,WVjacNew(:,nlays-0)); title(['lay ' num2str(nlays-0)])
xyz = squeeze(WVjac(:,:,iProf)); xyz = xyz'; xyz = rad2bt(vchan,xyz(:,nlays));
figure(7); plot(h.vchan,rad2bt(h.vchan,r0(:,iProf)),'.-',h.vchan,xyz); title(['calculated rads0 and at lay ' num2str(nlays-0)])
figure(7); pcolor(h.vchan,1:nlays,WVjacOld(:,1:nlays)'-WVjacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
figure(7); dJ = WVjacOld(:,1:nlays)'-WVjacNew(:,1:nlays)';
figure(7); dJ = 100*dJ ./(WVjacNew(:,1:nlays)'+eps);  pcolor(h.vchan,1:nlays,dJ); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
  caxis([-1 +1]); colorbar
  
O3jacNew = squeeze(O3jac(:,:,iProf)); O3jacNew = O3jacNew'; O3jacNew = rad2bt(vchan,O3jacNew)-rad2bt(vchan,r0(:,iProf))*ones(1,100);
for ii = 1 : 100
  O3jacOld(:,ii) = rad2bt(vchan,squeeze(raaO3(ii,:,iProf))')-rad2bt(vchan,r0(:,iProf));
end
figure(1); pcolor(vchan,1:nlays,O3jacOld(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); cx = caxis;
figure(2); pcolor(vchan,1:nlays,O3jacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse'); caxis(cx);
figure(3); plot(h.vchan,O3jacOld(:,30),'.-',h.vchan,O3jacNew(:,30)); title('lay 30')
figure(4); plot(h.vchan,O3jacOld(:,nlays-2),'.-',h.vchan,O3jacNew(:,nlays-2)); title(['lay ' num2str(nlays-2)])
figure(5); plot(h.vchan,O3jacOld(:,nlays-1),'.-',h.vchan,O3jacNew(:,nlays-1)); title(['lay ' num2str(nlays-1)])
figure(6); plot(h.vchan,O3jacOld(:,nlays-0),'.-',h.vchan,O3jacNew(:,nlays-0)); title(['lay ' num2str(nlays-0)])
xyz = squeeze(O3jac(:,:,iProf)); xyz = xyz'; xyz = rad2bt(vchan,xyz(:,nlays));
figure(7); plot(h.vchan,rad2bt(h.vchan,r0(:,iProf)),'.-',h.vchan,xyz); title(['calculated rads0 and at lay ' num2str(nlays-0)])
figure(7); pcolor(h.vchan,1:nlays,O3jacOld(:,1:nlays)'-O3jacNew(:,1:nlays)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
figure(7); dJ = O3jacOld(:,1:nlays)'-O3jacNew(:,1:nlays)';
figure(7); dJ = 100*dJ ./(O3jacNew(:,1:nlays)'+eps);  pcolor(h.vchan,1:nlays,dJ); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse');
  caxis([-1 +1]); colorbar