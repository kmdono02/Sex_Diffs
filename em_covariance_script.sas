* Set default values to whole calib sample, all_social, variables for all_social;
%MACRO compute_em_cov(subset=whole, model=all_social, start_var=EYEC, end_var=aosi_q9);
	proc import datafile="C:\Users\kmdono02\Documents\mnlfa_mplus_dir\&model\data\calib_sample_&subset..CSV"
	        out=calib
	        dbms=csv
	        replace;
			getnames=yes;
	run;

	* Est. covariance using EM;
	proc mi data=calib nimpute=0;
		em outem=EM_cov;
		var &start_var--&end_var;
	run;

	* Save covariance matrix as CSV to read into R;
	proc export data=EM_cov outfile="C:\Users\kmdono02\Documents\mnlfa_mplus_dir\&model\data\em_cov_&subset..CSV"
		dbms=csv
		replace;
	run;
%MEND;

%compute_em_cov(subset=whole);
%compute_em_cov(subset=male);
%compute_em_cov(subset=female);
