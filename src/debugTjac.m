clear all
tic
[h,ha,p0,pa] = rtpread('junk135_2014_02_08_prof10.rtp');

rtpwrite('junkjunk.rp.rtp',h,ha,p0,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh0'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
r0 = pjunk.rcalc;

p = p0;
p.ptemp(95,:) = p.ptemp(95,:) + 1;
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh95'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rST = pjunk.rcalc;

p = p0;
p.ptemp(96,:) = p.ptemp(96,:) + 1;
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh96'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rST = pjunk.rcalc;

p = p0;
p.ptemp(97,:) = p.ptemp(97,:) + 1;
rtpwrite('junkjunk.rp.rtp',h,ha,p,pa);
dojac = ['!time a.out fin=junkjunk.rp.rtp fout=new.rp.rtp >& newugh97'];
eval(dojac)
[hjunk,hajunk,pjunk,pajunk] = rtpread('new.rp.rtp');
rST = pjunk.rcalc;

