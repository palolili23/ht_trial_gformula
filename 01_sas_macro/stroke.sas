options nonotes nosource nosource2 errors=0;

%include 'V:\Uitwissel\Paloma\HT trial\gformula\gformula3.sas';

libname rose 'V:\Uitwissel\Paloma\HT trial\gformula\stroke\sbp_smoke';

PROC IMPORT OUT= rstrial
            DATAFILE= "V:\Uitwissel\Paloma\HT trial\Setup_R\01_setup\data\ht_trial_stroke.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

proc contents data = rstrial;
run;

data rose.dates_sim;
set rstrial;
run;

data dates_sim;
set rose.dates_sim;
run;

proc contents data = dates_sim; run;


proc freq data = dates_sim;
tables stroke_plr dead_plr combined_plr cens; 
run;



%macro bp_drop(percent = );
if sbp < 140 then do;
new_sbp = sbp;
end;
   if sbp >= 140 then do;
          new_sbp = sbp - (%sysevalf(&percent)/100)*sbp;
          sbp = new_sbp;
          sbp_totinterv = sbp_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
  end;
  ssbp[&time] = new_sbp; 
%mend;

%macro bp_drop2(percent = );
if sbp le 120 then do;
new_sbp = sbp;
end;
else if sbp > 120 and sbp le 130 then do;
         new_sbp = sbp - (%sysevalf(&percent)/100)*sbp;
         sbp = new_sbp;
         sbp_totinterv = sbp_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
			  end;
else if sbp > 130 and sbp le 140 then do;
         new_sbp = sbp - (%sysevalf(&percent + 5)/100)*sbp;
         sbp = new_sbp;
         sbp_totinterv = sbp_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
			  end;
else if sbp > 140 and sbp le 150 then do;
          new_sbp = sbp - (%sysevalf(&percent + 10)/100)*sbp;
          sbp = new_sbp;
          sbp_totinterv = sbp_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
			  end;
else if sbp > 150 then do;
 		  new_sbp = sbp -(%sysevalf(&percent + 15)/100)*sbp;
          sbp = new_sbp;
          sbp_totinterv = sbp_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
  end;
  ssbp[&time] = new_sbp; 
%mend;

%macro smoking;
if smoke_cig2 ne 3 then do;
new_smoke = smoke_cig2;
end;
   if smoke_cig2 = 3 then do;
          new_smoke = 2;
          smoke_cig2 = new_smoke;
          smoke_totinterv = smoke_totinterv +1; 
              intervened = 1;
              intervenedk[&time] =1;
              totinterv = totinterv+1;
  end;
  ssmoke_cig2[&time] = new_smoke; 

%mend;

%let interv1 =
intno = 1,
nintvar = 1,
intlabel = 'Under 120',
intvar1 = sbp, inttype1 = 2, intmax1 = 120, 
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;

%let interv2 =
intno = 2,
nintvar = 1,
intlabel = 'Under 140',
intvar1 = sbp, inttype1 = 2, intmax1 = 140, 
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;

%let interv3 = 
intno = 3, 
intlabel = "Drop 10% of SBP if above 140",
nintvar = 1,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop(percent = 10);

%let interv4 = 
intno = 4, 
intlabel = "Drop 20% of SBP if above 140",
nintvar = 1,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop(percent = 20);

%let interv5 = 
intno = 5, 
intlabel = "Drop 5% if between 120-130, 10% if between 130-140, 15% if between 140-150, 20% if >150",
nintvar = 1,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop2(percent = 5);

%let interv6 =
intno     = 6, 
intlabel  = 'Current to former smoker',
nintvar   = 1,
intvar1   = smoke_cig2,
inttype1  = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = smoking;

%let interv7 =
intno   = 7, 
nintvar = 2,
intlabel  = 'Joint 1 + 6',
intvar1 = sbp, 
inttype1 = 2, 
intmax1 = 120, 
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intvar2  = smoke_cig2,
inttype2  = -1,
inttimes2 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro2 = smoking;

%let interv8 =
intno     = 8, 
intlabel  = 'Joint 2 + 6',
nintvar = 2,
intvar1 = sbp, inttype1 = 2, intmax1 = 140, 
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intvar2  = smoke_cig2,
inttype2  = -1,
inttimes2 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro2 = smoking;

%let interv9 =
intno     = 9, 
intlabel  = 'Joint 3 + 6',
nintvar = 2,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop(percent = 10),
intvar2  = smoke_cig2,
inttype2  = -1,
inttimes2 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro2 = smoking;

%let interv10 =
intno     = 10, 
intlabel  = 'Joint 4 + 6',
nintvar = 2,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop(percent = 20),
intvar2  = smoke_cig2,
inttype2 = -1,
inttimes2 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro2 = smoking;

%let interv11 =
intno     = 11, 
intlabel  = 'Joint 5 + 6',
nintvar   = 2,
intvar1 = sbp,
inttype1 = -1,
inttimes1 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro1 = bp_drop2(percent = 5),
intvar2   = smoke_cig2,
inttype2  = -1,
inttimes2 = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,
intusermacro2 = smoking;


%macro create_visit2;
	if years < 1 then v2_bin = 0;
	if years = 6 and v2_bin = 0 then v2_bin = 1;
	if years > 6 then v2_bin = 1;
%mend;
%macro create_visit3;
	if years < 5 then v3_bin = 0;
	if v2_bin = 0 then v3_bin = 0;
	if years = 10 and v3_bin = 0 then v3_bin = 1;
	if years > 10 then v3_bin = 1;
%mend;
%macro create_visit4;
	if years < 9 then v4_bin = 0;
	if v3_bin = 0 then v4_bin = 0;
	if years = 15 and v4_bin = 0 then v4_bin = 1;
	if years > 15 then v4_bin = 1;
	%mend;
%macro create_cov4;
	smoke_cig2 = smoke_cig2_l1;
%mend;
%macro create_cov5;
	oh_cat = oh_cat_l1;
%mend;
%macro create_cov6;
	bmi = bmi_l1;
%mend;
%macro create_cov7;
	sbp = sbp_l1;
%mend;
%macro create_cov8;
	chol_cat = chol_cat_l1;
%mend;
%macro create_cov9;
	ht_drug = ht_drug_l1;
%mend;
%macro create_cov10;
	diab_v = diab_v_l1;
%mend;
%macro create_cov11;
	hd_v = hd_v_l1;
%mend;
%macro create_cov12;
	cancer_v = cancer_v_l1;
%mend;
%macro create_cov13;
	dem_v = dem_v_l1;
%mend;
%gformula(
data =dates_sim,
id = id, 
time = years, 
timepoints = 15,
timeptype = concub,

outc = stroke_plr ,
outctype = binsurv,
comprisk = dead_plr,


numint = 11,
fixedcov = age_0 age_sq sex apoe_cat_1 education_1 education_3 education_2 education_4 hd_prev_cat_1 diab_prev_cat_1 sbp_bl sbp_sq sbp_cub,
wherevars = v2_bin v2_bin_l1 v3_bin v3_bin_l1 v4_bin v4_bin_l1,

ncov = 13,
cov1 = v2_bin, 
	cov1ptype = tsswitch1, 		
	cov1otype = 2, 
	cov1wherem = (1 le years le 6),
	cov1wherenosim = (years < 1 or years ge 6), 
	cov1nosimelsemacro = create_visit2,
cov2 = v3_bin, 
	cov2ptype = tsswitch1, 
	cov2otype = 2, 
	cov2wherem = ((5 le years le 10) and v2_bin = 1), 
	cov2wherenosim = ((years < 5 or years ge 10) or v2_bin = 0), 
	cov2nosimelsemacro = create_visit3, 
cov3 = v4_bin, 
	cov3ptype = tsswitch1, 		
	cov3otype = 2, 
	cov3wherem = ((9 le years le 15) and v3_bin = 1),
	cov3wherenosim = ((years < 9 or years ge 15) or v3_bin = 0), 
	cov3nosimelsemacro = create_visit4,
cov4 = smoke_cig2,  
	cov4ptype = lag1cat, 	
	cov4otype = 5, 
	cov4wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov4wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov4nosimelsemacro = create_cov4,
cov5 = oh_cat,    
	cov5ptype = lag1cat, 	
	cov5otype = 5, 
	cov5wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov5wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov5nosimelsemacro = create_cov5,
cov6 = bmi,  
	cov6ptype = lag1spl, 	
	cov6otype = 3, 
	cov6knots = 18 25 30 35,
	cov6wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov6wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov6nosimelsemacro = create_cov6,
cov7 = sbp,  
	cov7ptype = lag1cub, 	
	cov7otype = 3, 
	cov7wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov7wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov7nosimelsemacro = create_cov7,
cov8 = chol_cat,  
	cov8ptype = lag1cat, 	
	cov8otype = 5, 
	cov8wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov8wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov8nosimelsemacro = create_cov8,
cov9 = ht_drug,    
	cov9ptype = lag1bin, 	
	cov9otype = 1, 
	cov9wherem =((v2_bin > v2_bin_l1) or (v3_bin > v3_bin_l1) or (v4_bin > v4_bin_l1)), 
	cov9wherenosim = ((v2_bin = v2_bin_l1) and (v3_bin = v3_bin_l1) and (v4_bin = v4_bin_l1)),  
	cov9nosimelsemacro = create_cov9,
cov10 = diab_v,    
	cov10ptype = tsswitch1, 	
	cov10otype = 1, 
cov11 = hd_v,    
	cov11ptype = tsswitch1, 	
	cov11otype = 2, 
cov12 = cancer_v,    
	cov12ptype = tsswitch1, 	
	cov12otype = 2, 
cov13 = dem_v,    
	cov13ptype = tsswitch1, 	
	cov13otype = 2, 
seed = 9468, 
nsamples =500, 

print_cov_means = 1,
check_cov_models = 1 ,
save_raw_covmean = 1,
outputs = no,
testing = yes,
print_stats = 1,
rungraphs = 1,	
simuldata = natcourse,
resultsdata = results,
survdata = survdata,
covmeandata = covmean, 
savelib = rose,
graphfile= stroke.pdf
);

data rose.survdata_str;
set work.Survdata;
run;

data rose.natcourse_str;
set work.Natcourse;
run;

data rose.covmean_str;
set work.Covmean;
run;

data rose.results_str;
set work.Results;
run;

