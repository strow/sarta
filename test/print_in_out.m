rtp_input = 'rtp_drivers/regr_rtp_6angs_49profs_1080mb_seaemis.rtp';
sarta_output = 'cris_test_output.rtp';

[h,ha,p,pa] = rtpread(rtp_input);
[h2,ha2,p2,pa2] = rtpread(sarta_output);

bt = rad2bt(h2.vchan,p2.rcalc);

fid = fopen('profile_6.txt','w');
fprintf(fid,'Surface Pressure: %6.3f',p.spres(6));
fprintf(fid,'%s\n',' ');
fprintf(fid,'Secant Angle : %6.3f',p.satzen(6));
fprintf(fid,'%s\n',' ');
fprintf(fid,'CO2 ppm : %6.3f',p.co2ppm(6));
fprintf(fid,'%s\n',' ');
fprintf(fid,'%s\n',' ');
fprintf(fid,'%s\n','Index   Water        Fixed       Ozone    Temperature ');
for i=1:100;
   fprintf(fid,'%3d   %6.4e   %6.4e   %6.4e   %5.2f \n',i,p.gas_1(i,6),p.gas_2(i,6),p.gas_3(i,6),p.ptemp(i,6));
end
fprintf(fid,'%s\n',' ');
fprintf(fid,'%s\n','Index  Efreq        Emis ');
for i=1:19;
   fprintf(fid,'%3d   %8.4f   %8.4f   \n',i,p.efreq(i,6),p.emis(i,6));
end
fprintf(fid,'%s\n',' ');

fprintf(fid,'%s\n','Chan ID   Freq    BT(K)');
for i=1:2235
   fprintf(fid,'%3d   %8.4f   %8.4f \n',h2.ichan(i),h2.vchan(i),bt(i,6));
end

fclose(fid);

