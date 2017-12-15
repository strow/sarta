clear all
tic
[h,ha,p0,pa] = rtpread('junk135_2014_02_08_save27prof.rp.rtp');

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
  fprintf(1,'%2i \n',ii)
  p = p0;
  p.ptemp(ii,:) =  p.ptemp(ii,:) + 1;
  rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
  dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh'];
  eval(dojac)
  [hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
  raaT(ii,:,:) = pjunk.rcalc;
end
toc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iDo = -1
if iDo > 0
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh; more newugh'];
  dojac = ['!time a.out fin=junk135_2014_02_08_save27prof.rp.rtp fout=new.rp.rtp fjacob=xjacob.dat >& newugh'];
  eval(dojac)
end
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
iProf = 1;
TjacNew = squeeze(Tjac(:,:,iProf)); TjacNew = TjacNew'; TjacNew = rad2bt(vchan,TjacNew)-rad2bt(vchan,r0(:,iProf))*ones(1,100);
nlays = p0.nlevs(iProf)-1;
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
