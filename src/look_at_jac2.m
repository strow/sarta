addpath /home/sergio/MATLABCODE

clear all
tic
[h,ha,p0,pa] = rtpread('junk135_2014_02_08_save27prof.rp.rtp');

addpath /home/sergio/MATLABCODE/matlib/clouds/sarta
addpath /home/sergio/MATLABCODE/CRODGERS_FAST_CLOUD
addpath /home/sergio/MATLABCODE/CLOUD
g = dogoodchan;
clist = get_retrieval_chans(h,g,4,2);
iDoSun = +1;
if iDoSun > 0
  clist = [clist; 2321; 2333];
  [h,p0] = subset_rtp_allcloudfields(h,p0,[],clist,[]);
  rtpwrite('junk135_2014_02_08_save27prof_421chans.rp.rtp',h,ha,p0,pa);
else
  [h,p0] = subset_rtp_allcloudfields(h,p0,[],clist,[]);
  rtpwrite('junk135_2014_02_08_save27prof_419chans.rp.rtp',h,ha,p0,pa);
end

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
  if ii > 95
    dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh' num2str(ii)];
  end
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
if iDo > 0 & iDoSun < 0
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof_419chans.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh; more newugh'];
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof_419chans.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh'];
  eval(dojac)
elseif iDo > 0 & iDoSun > 0
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof_421chans.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh; more newugh'];
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof_421chans.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh'];
  eval(dojac)
end

%% in 3 bunches
fjacob1 = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_1_10';
fjacob2 = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_11_20';
fjacob3 = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_21_27';
[vchan,ichan,stempjac1,cldjac1,Tjac1,WVjac1,O3jac1] = read_sarta_jacob(fjacob1);
[vchan,ichan,stempjac2,cldjac2,Tjac2,WVjac2,O3jac2] = read_sarta_jacob(fjacob2);
[vchan,ichan,stempjac3,cldjac3,Tjac3,WVjac3,O3jac3] = read_sarta_jacob(fjacob3);
stempjac = [stempjac1 stempjac2 stempjac3];
cldjac = cat(3,cat(3,cldjac1,cldjac2),cldjac3);
Tjac = cat(3,cat(3,Tjac1,Tjac2),Tjac3);
WVjac = cat(3,cat(3,WVjac1,WVjac2),WVjac3);
O3jac = cat(3,cat(3,O3jac1,O3jac2),O3jac3);

%% in 2 bunches
fjacob1 = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_1_25';
fjacob2 = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat_26_27';
[vchan,ichan,stempjac1,cldjac1,Tjac1,WVjac1,O3jac1] = read_sarta_jacob(fjacob1);
[vchan,ichan,stempjac2,cldjac2,Tjac2,WVjac2,O3jac2] = read_sarta_jacob(fjacob2);
stempjac = [stempjac1 stempjac2];
cldjac = cat(3,cldjac1,cldjac2);
Tjac = cat(3,Tjac1,Tjac2);
WVjac = cat(3,WVjac1,WVjac2);
O3jac = cat(3,O3jac1,O3jac2);

%% in 1 bunch
fjacob = '/home/sergio/SARTA_CLOUDY/GitSarta/sarta/src/xjacob.dat';
[vchan,ichan,stempjac,cldjac,Tjac,WVjac,O3jac] = read_sarta_jacob(fjacob);

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