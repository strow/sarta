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
TjacNew = squeeze(Tjac(:,:,1)); TjacNew = TjacNew'; TjacNew = rad2bt(vchan,TjacNew)-rad2bt(vchan,r0(:,1))*ones(1,100);
for ii = 1 : 100
  TjacOld(:,ii) = rad2bt(vchan,squeeze(raaT(ii,:,1))')-rad2bt(vchan,r0(:,1));
end
figure(1); pcolor(vchan,1:97,TjacOld(:,1:97)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse')
figure(2); pcolor(vchan,1:97,TjacNew(:,1:97)'); shading flat; colormap jet; colorbar; set(gca,'ydir','reverse')
