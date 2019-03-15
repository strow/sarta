% Get the subsetting (to be used for plotting)
subs.ang = unique(p.satzen);
idx = struct;
for i=1:7 
  idx.a{i} =  find(p.satzen == subs.ang(i) & p.emis(9,:) == 1); 
  idx.b{i} =  find(p.satzen == subs.ang(i) & p.emis(9,:) < 1); 
end
subs.names = {'scan angles x 7','2 x surface emissivity: unit and sea'};