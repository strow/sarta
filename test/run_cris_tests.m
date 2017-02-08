% rtp_input    = 'rtp_drivers/daytime_regr_rtp_6angs_49profs_1080mb_seaemis.rtp';
% kcarta_truth = 'kcarta_truth/kcarta_crisHI_1080mb_seaemiss.mat';
% fout = 'day_with_nlte_fix';
% fh = cris_hires_bias_test(rtp_input,kcarta_truth,fout);
% figure(fh)
% aslprint('with_nlte_fix');

% rtp_input    = 'rtp_drivers/regr_rtp_6angs_49profs_1080mb_seaemis.rtp';
% kcarta_truth = 'kcarta_truth/kcarta_crisHI_1080mb_seaemiss.mat';
% fout = 'cris_test_output.rtp';
% fh = cris_hires_bias_test(rtp_input,kcarta_truth,fout);
% figure(fh)
% aslprint('regr_1080mb_seaemis');
% 
% rtp_input    = 'rtp_drivers/regr_rtp_6angs_49profs_1080mb_unitemis.rtp';
% kcarta_truth = 'kcarta_truth/kcarta_crisHI_1080mb_unitemiss.mat';
% fout = 'cris_test_output.rtp';
% fh = cris_hires_bias_test(rtp_input,kcarta_truth,fout);
% figure(fh)
% aslprint('regr_1080mb_unitemis');
% % 
% rtp_input    = 'rtp_drivers/SAF_6angs_704profs_1013mb_seaemis.rtp';
% kcarta_truth = 'kcarta_truth/kcarta_crisHI_SAF_6angs_704profs_1013mb_seaemis.mat';
% fout = 'cris_test_output.rtp';
% fh = cris_hires_bias_test(rtp_input,kcarta_truth,fout);
% figure(fh)
% aslprint('SAF_1013mb_seaemis');
% 
rtp_input    = 'rtp_drivers/SAF_6angs_704profs_1013mb_unitemis.rtp';
kcarta_truth = 'kcarta_truth/kcarta_crisHI_SAF_6angs_704profs_1013mb_unitemis.mat';
fout = 'cris_test_output.rtp';
fh = cris_hires_bias_test(rtp_input,kcarta_truth,fout);
figure(fh)
aslprint('SAF_1013mb_unitemis');
