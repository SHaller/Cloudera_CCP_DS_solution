******************************************************************************; 
* MAIN MACROS AND ROUTINES FOR                                               *; 
* CLOUDERA DATA SCIENCE CHALLENGE: MEDICARE ANOMALIES                        *; 
* PATRICK.HALL@SAS.COM                                                       *;
******************************************************************************; 

******************************************************************************;
* SYSTEM OPTIONS                                                             *;
******************************************************************************;

*** OLD SCHOOL LISTING OUTPUT IS MORE EFFICIENT; 
ods html close; 
ods listing; 

*** DIRECTORY SEPERATOR CHARACTER; 
%let DSEP= \; 	

*** RAW DATA DIR; 
%let RAW_DIR= C:\Public\MedicareData\raw; 

*** PERMANENT NETWORKED STORAGE; 
%let NET_DIR= C:\Public\MedicareData\sas; 
libname net "&NET_DIR"; 

*** OUTPUT DIRECTORY; 
%let OUT_DIR= C:\Public\MedicareData\out; 

**** GRAPHICS OPTIONS; 
ods listing gpath= "&OUT_DIR";  

*** GLOBAL MACRO VARS; 
%global _BEST_K;

******************************************************************************;
* IMPORT DATA                                                                *;
******************************************************************************;

*** IMPORT SUMMARY DATA ******************************************************; 

*** COMMON UTILITY FUNCTION; 
%macro get_local_copy(DS); 
	 	%if ^%sysfunc(exist(&DS)) %then %do;
			data &DS; 
				set net.&DS; 
			run; 
			%put NOTE: COPYING &DS TO LOCAL WORKING LIBRARY.; 
		%end; 
%mend get_local_copy;

%macro get_summary_data(R_DIR= &RAW_DIR, N_DIR= &NET_DIR); 

	libname net "&N_DIR"; 

	*** FUNCTION TO CONDITIONALLY IMPORT SUMMARY BY PROCEDURE ****************; 
	%macro get_summary_by_procedure(DS, FILENAME, MAX_PROC_REC_LENGTH); 

		*** IF SET IS NOT IN SAS FORMAT, IMPORT IT; 
		%if ^%sysfunc(exist(net.&DS)) %then %do;
			data net.&DS;
				infile "&R_DIR.&DSEP.&FILENAME"
				delimiter = ',' dsd missover lrecl= 32767 firstobs= 2;
				informat procedure_code $ &MAX_PROC_REC_LENGTH..;
				informat num_service best32.;
				informat ave_provider_charge best32.;
				informat ave_medicare_payment best32.;
				format procedure_code $ &MAX_PROC_REC_LENGTH..;
				format num_service best12.;
				format ave_provider_charge dollar12.2;
				format ave_medicare_payment dollar12.2;
				input 
					procedure_code $
					num_service 
					ave_provider_charge
					ave_medicare_payment; 
			run;
			%put NOTE: IMPORTING &FILENAME TO NET LIBRARY.; 
		%end; 
		*** IF SET IS NOT ON LOCAL MACHINE, GET IT;
		%get_local_copy(&DS); 

	%mend get_summary_by_procedure; 

	*** EXECUTE IMPORT; 
	%get_summary_by_procedure(outpatient_by_procedure, Medicare_Charge_Outpatient_APC30_Summary_by_APC_CY2011_v2.csv, 100); 
	%get_summary_by_procedure(inpatient_by_procedure, Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRG_FY2011.csv, 100);

/*	*** FUNCTION TO CONDITIONALLY IMPORT SUMMARY BY PROCEDURE AND STATE ******;*/
/*	%macro get_summary_by_proc_st(DS, FILENAME, MAX_PROC_REC_LENGTH);*/
/**/
/*		*** IF SET IS NOT IN SAS FORMAT, IMPORT IT; */
/*		%if ^%sysfunc(exist(net.&DS)) %then %do;*/
/*			data net.&DS;*/
/*				infile "&R_DIR.&DSEP.&FILENAME"*/
/*					delimiter = ',' dsd missover lrecl= 32767 firstobs= 2;*/
/*				informat procedure_code $ &MAX_PROC_REC_LENGTH..;*/
/*				informat state $2.; */
/*				informat num_service best32.;*/
/*				informat ave_provider_charge best32.;*/
/*				informat ave_medicare_payment best32.;*/
/*				format procedure_code $&MAX_PROC_REC_LENGTH..;*/
/*				format state $2.; */
/*				format num_service best12.;*/
/*				format ave_provider_charge dollar12.2;*/
/*				format ave_medicare_payment dollar12.2;*/
/*				input */
/*					procedure_code $*/
/*					state $*/
/*					num_service */
/*					ave_provider_charge*/
/*					ave_medicare_payment; */
/*			run; */
/*			%put NOTE: IMPORTING &FILENAME TO NET LIBRARY.; */
/*		%end; */
/*		*** IF SET IS NOT ON LOCAL MACHINE, GET IT; */
/*	 	%get_local_copy(&DS);*/
/**/
/*	%mend get_summary_by_proc_st; */
/**/
/*	*** EXECUTE IMPORT; */
/*	%get_summary_by_proc_st(outpatient_by_proc_st, Medicare_Charge_Outpatient_APC30_Summary_by_APCState_CY2011_v2.csv, 100); */
/*	%get_summary_by_proc_st(inpatient_by_proc_st, Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRGState_FY2011.csv, 100);*/

	*** FUNCTION TO CONDITIONALLY IMPORT SUMMARY BY PROCEDURE AND STATE ******;
	%macro get_summary_by_proc_st_prov(DS, 
									   FILENAME, 
									   MAX_PROC_REC_LENGTH,
									   MAX_PROV_NAME_REC_LENGTH,
									   MAX_PROV_ADDRESS_REC_LENGTH,
									   MAX_PROV_CITY_REC_LENGTH,
									   MAX_PROV_REF_REGION_REC_LENGTH
									  );

		*** IF SET IS NOT IN SAS FORMAT, IMPORT IT; 
		%if ^%sysfunc(exist(net.&DS)) %then %do;
			data net.&DS;
				infile "&R_DIR.&DSEP.&FILENAME"
					delimiter = ',' dsd missover lrecl= 32767 firstobs= 2;
				informat procedure_code $ &MAX_PROC_REC_LENGTH..;
				informat provider_id best32.;
				informat name $ &MAX_PROV_NAME_REC_LENGTH..; 
				informat address $ &MAX_PROV_ADDRESS_REC_LENGTH..; 
				informat city $ &MAX_PROV_CITY_REC_LENGTH..; 
				informat state $ 2.; 
				informat zip best32.; 
				informat ref_region $ &MAX_PROV_REF_REGION_REC_LENGTH..;
		        informat num_service best32.;
		        informat ave_provider_charge best32.;
		        informat ave_medicare_payment best32.;
	        	format procedure_code $&MAX_PROC_REC_LENGTH..;
				format provider_id best12.;
				format name $ &MAX_PROV_NAME_REC_LENGTH..; 
				format address $ &MAX_PROV_ADDRESS_REC_LENGTH..; 
				format city $ &MAX_PROV_CITY_REC_LENGTH..; 
				format state $2.; 
				format zip 5.; 
				format ref_region $ &MAX_PROV_REF_REGION_REC_LENGTH..;
				format num_service best12.;
				format ave_provider_charge dollar12.2;
				format ave_medicare_payment dollar12.2;
				input 
					procedure_code $
					provider_id
					name $
					address $
					city $
					state $
					zip
					ref_region $
					num_service 
					ave_provider_charge
					ave_medicare_payment; 
			run; 
			%put NOTE: IMPORTING &FILENAME TO NET LIBRARY.; 
		%end; 
		*** IF SET IS NOT ON LOCAL MACHINE, GET IT; 
	 	%get_local_copy(&DS);

	%mend get_summary_by_proc_st_prov; 

	*** EXECUTE IMPORT; 
	%get_summary_by_proc_st_prov(outpatient_by_proc_st_prov, Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv, 100, 75, 75, 20, 20); 
	%get_summary_by_proc_st_prov(inpatient_by_proc_st_prov, Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv, 100, 75, 75, 20, 20); 

	*** GENERATE UNIQUE, TEMPORARY KEY ACROSS OUT- AND IN- PATIENT PROCEDURES; 
	data Outpatient_by_procedure;
		length proc_type $3;  
		set Outpatient_by_procedure; 
		proc_type= 'OUT';
	run; 
	proc append base= proc_key_map data= Outpatient_by_procedure(keep= procedure_code proc_type) force; run; 
	data Inpatient_by_procedure;
		length proc_type $3;  
		set Inpatient_by_procedure; 
		proc_type= 'IN';
	run; 
	proc append base= proc_key_map data= Inpatient_by_procedure(keep= procedure_code proc_type) force; run; 
	proc sort data= proc_key_map noduprec; by procedure_code; run;  
	data proc_key_map; 
		set proc_key_map; 
		global_proc_id= _n_; 
	run; 

	*** APPEND OUT- AND IN- PATIENT PROCEDURES; 
	proc append base= all_procs data= Outpatient_by_proc_st_prov force; run;
	proc append base= all_procs data= Inpatient_by_proc_st_prov force; run;

	*** JOIN WITH global_prod_id; 
	proc sort
		data= all_procs
		threads
		sortsize= MAX; 
		by procedure_code; 
	run;
	data all_procs; 
		merge all_procs proc_key_map; 
		by procedure_code; 
	run; 

%mend get_summary_data;
%get_summary_data; 

*** IMPORT PATIENT DUMP XML FILES ********************************************; 

*** FUNCTION TO IMPORT XML PATIENT DUMPS; 
%macro get_patient_xml(XML_FILE, DS); 

	%if ^%sysfunc(exist(net.&DS)) %then %do;
		libname patient xml "&XML_FILE"; 
		data net.&DS;
			length id 8 age $12 gender $12 income $12; 
			set patient.rows;
			id= field0;	/* XML LIBNAME IS TREATING ID AS A NUMERIC, BE CAREFUL OF PREPADDED ZEROS */
			format id 12.; 
			array f field3 field2 field1;  
			do i= 1 to dim(f); 
				if strip(f[i]) in ('<16000', '16000-23999', '24000-31999','32000-47999', '48000+') then income= f[i]; 
				else if strip(f[i]) in ('M','F') then gender= f[i]; 
				else if strip(f[i]) in ('<65', '65-74', '75-84', '85+') then age= f[i]; 
				else f[i]= ' '; 
			end; 
			keep id age gender income; 
			if mod(_n_, 100000)= 0 then do;
				line= 'Processing record: '||put(_n_, best.)||' ...';
				put line; 
			end;  
		run; 
	%end;  
	%get_local_copy(&DS);

%mend get_patient_xml; 

*** EXECUTE CONDITIONAL IMPORT; 
*** IMPORT REQUIRES MINS. PER FILE;
*** USED GREP, HEAD, TAIL AND SED TO CREATE SMALLER FILES BEFORE IMPORT; 
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP1.XML, pntdump1); 
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP2.XML, pntdump2);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP3.XML, pntdump3); 
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP4.XML, pntdump4);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP5.XML, pntdump5);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP6.XML, pntdump6);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP7.XML, pntdump7);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP8.XML, pntdump8);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP9.XML, pntdump9);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP10.XML, pntdump10);
%get_patient_xml(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PNTDUMP11.XML, pntdump11);

*** IMPORT PATIENT TRANSACTIONS **********************************************; 

*** FUNCTION TO IMPORT ASCII DELIMITED DATA; 
%macro get_patient_trans(ASCII_FILE, DS); 

	%if ^%sysfunc(exist(net.&DS)) %then %do;
		data net.&DS; ; 						
			infile "&ASCII_FILE" recfm= f lrecl= 1 end= eof; 
			length accum $32 date $8 id 8 procedure_code $6; /* XML LIBNAME IS TREATING ID AS A NUMERIC, BE CAREFUL OF PREPADDED ZEROS */
			retain accum ' ';								 /* DOING THE SAME HERE FOR LATER MERGING */
			retain cut 0;  
			input x $char1.;  
			delim= x in ('1f'x,'1e'x);  
			if not delim then accum= trimn(accum)||x; 
			if not delim and not eof then return; 
			if x= '1f'x then do; 
				_x= length(accum); 
				if _x ne . and _x gt 8 and _x lt 20 then cut= _x+1; 
			end; 
			if x='1e'x or eof then do;
				date= accum; 
				id= substr(accum, 9, cut-9); 
				procedure_code= substr(accum, cut); 
				output; 
				accum= ' ';
				cut= 0;  
			end; 
			if mod(_n_, 1000000)= 0 then do;
				line= 'Processing record: '||put(_n_, best.)||' ...';
				put line; 
			end;
			keep date id procedure_code; 
		run;
	%end; 
	%get_local_copy(&DS);

%mend get_patient_trans; 

*** EXECUTE CONDITIONAL IMPORT; 
*** IMPORT REQUIRES MINS. PER FILE; 
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1101.ADT, pcdr1101);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1102.ADT, pcdr1102);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1103.ADT, pcdr1103);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1104.ADT, pcdr1104);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1105.ADT, pcdr1105);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1106.ADT, pcdr1106);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1107.ADT, pcdr1107);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1108.ADT, pcdr1108);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1109.ADT, pcdr1109);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1110.ADT, pcdr1110);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1111.ADT, pcdr1111);
%get_patient_trans(\\rdcx200\EM_data\All_Users\pathal\MedicareData\raw\PCDR1112.ADT, pcdr1112);

******************************************************************************;
* PART 1                                                                     *; 
******************************************************************************;

*** PART A: Which three procedures have the highest relative variance in cost?; 

*** CREATE TEMP WORKING SET;  
data x y; 
	set all_procs(keep= global_proc_id procedure_code proc_type ave_provider_charge); 
run; 
*** FIND COEFFICIENT OF VARIATION;  
proc univariate 
	data= x 
	noprint
	idout;
	var ave_provider_charge; 
	by global_proc_id; 
	id procedure_code proc_type;
	output out=x cv= cv;
run;  
*** SORT BY DESCENDING CV; 
proc sort data= x; by descending cv; run; 
*** OUTPUT RESULTS; 
data _null_; 
	length line $256;
	set x (obs= 3);
	file "&OUT_DIR.&DSEP.part1a.csv"; 
	line= strip(procedure_code);
	put line; 
run; 
proc export 
	data= x
	outfile= "&OUT_DIR.&DSEP.procedureCV.csv" 
	dbms= csv 
	replace; 
run; 
data x; 
	set x (obs= 3 keep= global_proc_id cv); 
run; 
proc sort; by global_proc_id; run; 
data x; 
	merge x (in= x) y; 
	by global_proc_id;
	if x;   
run; 
proc sort; by descending cv; run; 
proc sgplot data= x; 
 	hbox ave_provider_charge / category= procedure_code;
	label procedure_code= 'Procedure Code' ave_provider_charge= 'Average Provider Charge';
run;

proc delete data= x; run; 

*** FUNCTION FOR SORTING, COUNTING AND SELECTING THE TOP 3 RECORDS ***********;
*** BY A CERTAIN VARIABLE; 
*** ALSO OUTPUT; 
*** USED FOR PARTS B-D; 
%macro get_top_3(SUMMARY_SET, BY_VAR, OUT_FILE, TITLE=, PIE_CHART= 1, PIE_VAR=); 
	proc sort 
		data= &SUMMARY_SET
		out= &SUMMARY_SET; 
		by &BY_VAR; 
	run; 

	%if (&PIE_CHART) %then %do; 
		
		filename pie "&OUT_DIR.&DSEP.&OUT_FILE..png"; 
		goptions 
			reset= all 
			cback= white 
			htitle= 48pt 
			htext= 10pt 
			gsfname= pie 
			dev= png
			hsize= 2400pt
			vsize= 1600pt;  
			title "&TITLE";
		proc gchart data=x;
			pie &PIE_VAR / 
				other= 2.5
				radius= 35 
				value= none 
				percent= arrow
				slice= arrow
				ascending
				noheading 
				plabel= (font= 'Albany AMT/bold' h=1 color=depk);
		run;
		quit;
		title; 

	%end; 

	data &SUMMARY_SET; 
		set &SUMMARY_SET; 
		by &BY_VAR; 
		retain count 0; 
		if first.&BY_VAR. then count= 1;
		if last.&BY_VAR. then output; 
		count + 1;  
	run; 
	proc sort; by descending count; run; 

	data _null_; 
		length line $256;
		retain check_sum 0; 
		set &SUMMARY_SET end= eof;
		file "&OUT_DIR.&DSEP.&OUT_FILE..csv"; 
		line= strip(&BY_VAR);
		if _n_ le 3 then put line;
		check_sum= check_sum + count; 
		if eof then call symput('CHECK_SUM', strip(put(check_sum, best.))); 
	run; 
	%put NOTE: CHECK SUM= &CHECK_SUM..; 

%mend get_top_3; 

*** PART B: Which three providers claimed the highest amount (on average) for the largest number of procedures?; 

*** SORT TO CREATE TEMP WORKING SET; 
proc sort 
	data= all_procs
	out= x
	threads
	sortsize= MAX; 
	by global_proc_id descending ave_provider_charge; 
run;  
data x; 
	set x; 
	by global_proc_id descending ave_provider_charge;
	if first.global_proc_id; 
run; 
%get_top_3(x, provider_id, part1b, TITLE= PERCENT OF HIGHEST CLAIMS FOR A PROCEDURE, PIE_VAR= NAME); 
proc delete data= x; run; 

*** PART C: The providers in which three regions claimed the highest average amount for the largest number of procedures?; 

*** SORT TO CREATE TEMP WORKING SET; 
proc sort 
	data= all_procs
	out= x (keep= ref_region global_proc_id ave_provider_charge)
	threads
	sortsize= MAX; 
	by ref_region global_proc_id; 
run;
*** CREATE SUMMARY SET; 
proc means
	data= x
	noprint; 
	by ref_region global_proc_id; 
	var ave_provider_charge; 
	output out= x (drop= _TYPE_ _FREQ_ where=(_STAT_= 'MEAN')); 
run;  
proc sort 
	data= x (keep= ref_region global_proc_id ave_provider_charge)
	out= x 
	threads
	sortsize= MAX; 
	by global_proc_id descending ave_provider_charge; 
run;
data x; 
	set x; 
	by global_proc_id descending ave_provider_charge;
	if first.global_proc_id; 
run; 
%get_top_3(x, ref_region, part1c, TITLE= PERCENT OF HIGHEST CLAIMS FOR A PROCEDURE, PIE_VAR= REF_REGION); 
proc delete data= x; run; 

*** PART D: Which three providers had the largest claim difference for the largest number of procedures?; 

*** CREATE TEMP WORKING SET; 
data x (keep= difference provider_id name global_proc_id); 
	set all_procs; 
	difference= ave_provider_charge-ave_medicare_payment; 
run; 
*** CREATE SUMMARY SET; 
proc sort 
	data= x
	threads
	sortsize= MAX; 
	by global_proc_id descending difference; 
run;
proc export 
	data= x
	outfile= "&OUT_DIR.&DSEP.procedureDiff.csv"
	dbms= csv
	replace; 
run;
data x; 
	set x; 
	by global_proc_id descending difference;
	if first.global_proc_id; 
run;
%get_top_3(x, provider_id, part1d, PIE_CHART= 0);  
 	
proc delete data= x; run; 

******************************************************************************;
* PART 2                                                                     *;
******************************************************************************;

*** COUNT PROVIDERS; 
proc sql noprint;
	select count(unique(provider_id)) into :NUM_PROVIDER 
	from all_procs; 
quit; 
%put NOTE: NUMBER OF PROVIDERS= &NUM_PROVIDER..;

*** COUNT REGIONS; 
proc sql noprint;
	select count(unique(ref_region)) into :NUM_REGION 
	from all_procs; 
quit; 
%put NOTE: NUMBER OF REGIONS= &NUM_REGION..;

*** BASIC FEATURE ENGINEERING ************************************************; 

*** LEVEL OF A PROCEDURE; 

data level; 
	set Proc_key_map; 
	if index(upcase(procedure_code), 'LEVEL I')^= 0 or
		index(upcase(procedure_code), 'LEVEL 1')^= 0 then level= 1; 
	if index(upcase(procedure_code), 'LEVEL II')^= 0 or
		index(upcase(procedure_code), 'LEVEL 2')^= 0 then level= 2;  
	if index(upcase(procedure_code), 'LEVEL III')^= 0 or
		index(upcase(procedure_code), 'LEVEL 3')^= 0 then level= 3; 
	if index(upcase(procedure_code), 'LEVEL IV')^= 0 or
		index(upcase(procedure_code), 'LEVEL 4')^= 0 then level= 4; 
	if index(upcase(procedure_code), 'LEVEL V')^= 0 or
		index(upcase(procedure_code), 'LEVEL 5')^= 0 then level= 5;
	if strip(proc_type)= 'IN' then do; 
		level= 6; 
	if index(upcase(procedure_code), 'W CC')^= 0 then level= level+1; 
	if index(upcase(procedure_code), 'W MCC')^= 0 then level= level+1; 
	end;  
	if level= . then level= 0;
run; 

*** IS THE PROVIDER TIGHTLY ASSOCIATED WITH A UNIVERSITY; 

proc sort 
	data= all_procs (keep= name provider_id city)
	out= university
	threads
	sortsize= max
	nodupkey; 
	by provider_id; 
run; 
data university; 
	set university; 
	if index(upcase(name), 'UAMS ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UCSF ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UMASS ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UNM ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UMC ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UW ')^= 0 then university_flag= 1;
	if index(upcase(name), 'STANFORD')^= 0 and 
		strip(upcase(city))= 'STANFORD' then university_flag= 1;
	if index(upcase(name), 'UT ')^= 0 and 
		index(upcase(name), 'NUT ')= 0 and
		index(upcase(name), 'OUT ')= 0 then university_flag= 1;
	if index(upcase(name), 'UVA ')^= 0 then university_flag= 1;
	if index(upcase(name), 'YALE')^= 0 then university_flag= 1;
	if index(upcase(name), 'ALBERT EINSTEIN')^= 0 then university_flag= 1;
	if index(upcase(name), 'BAYLOR')^= 0 then university_flag= 1;
	if index(upcase(name), 'UNIV')^= 0  and provider_id^= 340166 then university_flag= 1;
	if index(upcase(name), 'UI ')^= 0 and 
		index(upcase(name), 'LOUI ')= 0 and 
			index(upcase(name), 'MAUI ')= 0 then university_flag= 1;
	if index(upcase(name), 'USC ')^= 0 then university_flag= 1;
	if index(upcase(name), 'COLLEGE')^= 0 and 
		provider_id^= 450299 then university_flag= 1;
	if index(upcase(name), 'LSU ')^= 0 then university_flag= 1;
	if index(upcase(name), 'UPMC ')^= 0 then university_flag= 1;
	if index(upcase(name), 'MOUNT SINAI')^= 0 then university_flag= 1;
	if index(upcase(name), 'USD ')^= 0 then university_flag= 1; 
	if index(upcase(name), 'UCLA ')^= 0 then university_flag= 1; 
	if index(upcase(name), 'UNIVERSITY')^= 0 and 
		provider_id^= 340166 then university_flag= 1;
	if university_flag= . then university_flag= -1;  
	drop city; 
run; 

*** Which three providers are least like the others? *************************;

*** CREATE A SUMMARY SET; 
proc sort 
	data= all_procs 
	out= summary
	threads
	sortsize= max;
	by global_proc_id; 
run; 
*** MERGE WITH LEVEL FEATURES; 
data summary; 
	merge summary level (keep= global_proc_id level); 
	by global_proc_id; 
	array levels level0-level7;
	do i= 1 to dim(levels); 
		if i-1= level then levels[i]= num_service;
		else levels[i]= 0;  
	end;
	drop i;  
run; 
*** MERGE WITH UNIVERSITY FEATURES; 
proc sort 
	data= summary 
	threads
	sortsize= max;
	by provider_id; 
run;
data summary; 
	merge summary university (keep= provider_id university_flag); 
	by provider_id;  
run; 

*** COLLAPSE SET BY PROVIDER ID **********************************************;
 
proc sql noprint; 
	create table provider_summary as
	select unique provider_id,
		   name,
		   mean(ave_provider_charge) as AVE_ave_provider_charge,
		   mean(ave_medicare_payment) as AVE_ave_medicare_payment,
		   mean(num_service) as AVE_num_service,
		   mean(level) as AVE_level,
		   sum(level0) as SUM_level0,
		   sum(level1) as SUM_level1,
		   sum(level2) as SUM_level2,
		   sum(level4) as SUM_level4, /* LEVELS 3 AND 4 HIGHLY CORRELATED */
		   sum(level5) as SUM_level5,
		   sum(level7) as SUM_level7, /* LEVELS 6 AND 7 HIGHLY CORRELATED */
		   max(university_flag) as MAX_university_flag 
	from summary
	group by provider_id;
quit; 

*** FUNCTIONS TO FIND MOST DISTINCT CASES IN SET *****************************; 

*** TRADITIONAL APPROACH; 
%macro regress_with_diagnostics(DS, ID_VAR, TITLE, N);

	title "&TITLE";

	*** LOG TRANSFORM SKEWED DISTRIBUTIONS;
	data reg; 
		set &DS. (keep= &ID_VAR. ave_ave_provider_charge ave_ave_medicare_payment ave_num_service); 
		LOG_AVE_num_service= log(AVE_num_service); 
		LOG_AVE_ave_provider_charge= log(AVE_ave_provider_charge); 
		LOG_AVE_ave_medicare_payment= log(AVE_ave_medicare_payment); 
		label
			LOG_AVE_num_service= 'Log(Number of Services)'
			LOG_AVE_ave_provider_charge= 'Log(Average Provider Charge)' 
			LOG_AVE_ave_medicare_payment= 'Log(Average Medicare Payment)'
		; 
		drop AVE_:; 
	run; 
	proc univariate data= reg noprint;
		var LOG_AVE_num_service LOG_AVE_ave_provider_charge LOG_AVE_ave_medicare_payment;
		histogram  / normal (mu=est sigma=est noprint);
		inset min max skewness kurtosis / position=ne;
	run;
	proc reg 
		data= reg
		plots (label)= all; 
		model log_ave_ave_provider_charge = log_ave_ave_medicare_payment log_ave_num_service	/
			vif 		/* MULTICOLLINEARITY */
			influence 	/* OUTLIERS */
			spec 		/* HETEROSCEDASTICITY, INDEPENDENCE OF ERRORS */ 
			partial 	/* PARTIAL REGRESSOR PLOTS */
			; 
		id &ID_VAR; 
		output out= regout rstudent= rstudent h= leverage p= pred r= res;  
	run; 
	quit; 

	*** FIND OUTLIERS BY;
	*** ABS(RSTUDENT) > 2; 
	*** LEVERAGE < 2P/N;   
	data regout; 
		set regout;
		length TYPE $25.; 
		leverage_cutoff= 6/&N.;  
		if (rstudent > 2 and leverage > LEVERAGE_CUTOFF) then TYPE= 'HIGH OUTLIER AND LEVERAGE'; 
		if (rstudent > 2 and leverage < LEVERAGE_CUTOFF) then TYPE= 'HIGH OUTLIER';
		if ((rstudent < 2 and rstudent > -2) and leverage < LEVERAGE_CUTOFF) then TYPE= 'NON-INFLUENTIAL';
		if ((rstudent < 2 and rstudent > -2) and leverage > LEVERAGE_CUTOFF) then TYPE= 'LEVERAGE';
		if (rstudent < -2 and leverage > LEVERAGE_CUTOFF) then TYPE= 'LOW OUTLIER AND LEVERAGE'; 
		if (rstudent < -2 and leverage < LEVERAGE_CUTOFF) then TYPE= 'LOW OUTLIER';
	run; 
	proc sort 
		data= regout (where= (TYPE= 'HIGH OUTLIER' or TYPE= 'HIGH OUTLIER AND LEVERAGE')) 
		out= regout_by_abs_rstudent; 
		by descending rstudent; 
	run; 

	title; 

%mend regress_with_diagnostics; 
		
*** UNSUPERVISED LEARNING APPROACH; 
*** IMPORT FUNCTION TO ESTIMATE THE BEST NUMBER OF CLUSTERS; 
*** BY ALIGNED BOX CRITERION; 
filename ABC '\\sashq\root\u\pathal\rapidSegmentation\rapidSegmentationSASCode\ABC.sas'; 
%include ABC /source2; 
filename ABC; 

%macro find_farthest_points(DS, ID_VAR, DROP_ID_VAR, TITLE, _BEST_K, N_DISPLAY, KEEP_LIST, WHERE=); 

	*** DETERMINE INPUTS; 
	filename emutil catalog 'sashelp.emutil.em_varmacro.source'; 
	%include emutil; 
	filename emutil; 
	proc contents data= &DS out= names (keep= name 
                                        where= (strip(name)^= "&ID_VAR" and strip(name)^= "&DROP_ID_VAR")); 
	run; 
	%EM_VARMACRO(
		name= INPUTS,
		metadata= names, 
		nummacro= NUM_INPUTS
	);

	*** STANDARDIZE SET;
	proc stdize
		data= &DS
		out= std_&DS
		method= std; 
		var %INPUTS; 
	run;

	*** ESTIMATE BEST NUMBER OF CLUSTERS *************************************; 

	data x; 
		set std_&DS. (drop= &DROP_ID_VAR); 
	run; 
	%if %CMPRES("&_BEST_K")= "" %then %do; 
		%getABC(
			TRAINING_TABLE= x,
			ID_VAR= &ID_VAR,
			PRECLUSTER= N 
		);
	%end; 

	*** ASSIGN CLUSTER LABELS ************************************************; 

	proc fastclus
		data= x
		maxclusters= &_BEST_K
		maxiter= 100
		out= outc (drop= DISTANCE);
        var %INPUTS; 
	run;

	*** PROJECT ONTO 2-D USING A DENOISING AUTOENCODER ***********************; 

	%if ^(%sysfunc(exist(outc_2dscore))) %then %do; 

		*** REQUIRED CATALOG FOR PROC NEURAL; 
		proc dmdb 
	      	data= outc 
	      	out= outc_dmdb
			dmdbcat= work.cat_outc_dmdb;
	     	var %INPUTS;
	      	id &ID_VAR CLUSTER;
	      	target %INPUTS;
		run;

		filename out "%sysfunc(pathname(WORK))\clusterout%sysfunc(compress(%sysfunc(datetime(), datetime23.),:)).txt"; 
		proc printto print= out; run; 

		*** TRAIN DENOISING AUTOENCODER; 
		proc neural
	    	data= outc
	      	dmdbcat= work.cat_outc_dmdb;
	  		performance compile details cpucount= 4 threads=yes;

			nloptions fconv= 0.00001;
			netoptions decay= 1.0; /* DENOISING */

	  		archi MLP hidden= 5;
			hidden &NUM_INPUTS / id= h1; 
			hidden %eval(&NUM_INPUTS/2) / id= h2; 
	      	hidden 2 / id= h3 act= linear;
			hidden %eval(&NUM_INPUTS/2) / id= h4;
			hidden &NUM_INPUTS / id= h5; 

	      	input %INPUTS / std= no id= i level= int;
	      	target %INPUTS / std= no id= t level= int;

	  		initial random= 123; 
			prelim 20 preiter= 20; 

			/* TRAIN LAYERS SEPARATELY */ 
			freeze h1->h2; 
			freeze h2->h3; 
			freeze h3->h4; 
			freeze h4->h5; 
		    train maxtime= 10000 maxiter= 5000;

			freeze i->h1; 
			thaw h1->h2; 
		    train maxtime= 10000 maxiter= 2000;
		 
			freeze h1->h2; 
			thaw h2->h3; 
			train maxtime= 10000 maxiter= 2000;

			freeze h2->h3; 
			thaw h3->h4; 
			train maxtime= 10000 maxiter= 2000;

			freeze h3->h4; 
			thaw h4->h5; 
			train maxtime= 10000 maxiter= 2000;
			 
			/* RETRAIN ALL LAYERS SIMULTANEOUSLY */ 		
			thaw i->h1;
			thaw h1->h2; 
			thaw h2->h3; 
			thaw h3->h4;
		    train maxtime= 10000 maxiter= 2000;

			%let NEURAL_SCORE_CODE= %sysfunc(pathname(WORK))\neuralscore%sysfunc(compress(%sysfunc(datetime(), datetime23.),:)).sas; 
	      	code file= "&NEURAL_SCORE_CODE";
		run; 
		data outc_2dscore; 
			set outc; 
			%include "&NEURAL_SCORE_CODE"; 
		run; 

		proc printto; run;

	%end; 
	
	*** NEED AN AVERAGE ROW; 
	data last_row; 
		set std_&DS.; 
		if _n_= 1 then stop; 
	run;  
	data last_row; 
		if _n_= 0 then set last_row; 
		&ID_VAR.= 1000000; 
		&DROP_ID_VAR.= 'ORIGIN'; 
		array x %INPUTS (&NUM_INPUTS.*0); 
	run; 
	proc append base= std_&DS. data= last_row force; run;  
	 
	*** FIND PAIRWISE DISTANCES FOR EVERY POINT INCLUDING ORIGIN *****************;  
	proc distance 
		data= std_&DS.
		out= dist_std_&DS.
		method= euclid
		shape= square
		nostd; 
		var interval(%INPUTS);
		id &DROP_ID_VAR.; 
		copy &ID_VAR.;
	run; 

	*** MERGE WITH NAMES TO PROFILE AND DISPLAY; 
	proc sort data= &DS.; by &ID_VAR.; run; 
	proc sort data= dist_std_&DS.; by &ID_VAR.; run;
 	proc sort data= outc_2dscore; by &ID_VAR.; run;
	data dist_std_&DS.; 
		merge dist_std_&DS. &DS.; 
		by &ID_VAR.;  
	run; 
	data profile_&DS.; 
		merge dist_std_&DS. (keep= &ID_VAR &DROP_ID_VAR %INPUTS origin) outc_2dscore (keep= &ID_VAR. CLUSTER); 
		by &ID_VAR.;
	run; 
	proc sort
		data=profile_&DS.;  
		by descending origin; 
	run;
	proc print data= profile_&DS. (obs= &N_DISPLAY); run; 
	proc sort
		data= dist_std_&DS. (keep= origin &ID_VAR. &DROP_ID_VAR.)
		out= label_&DS.; 
		by descending origin; 
	run;  
	data label_&DS.; 
		set label_&DS.;
		if ((_n_ > &N_DISPLAY.) and (&ID_VAR not in (&KEEP_LIST))) then &DROP_ID_VAR.= ''; 
	run; 
	proc sort; by &ID_VAR.; run; 
	data outc_2dscore; 
		merge outc_2dscore label_&DS. (keep= &ID_VAR &DROP_ID_VAR origin); 
		by &ID_VAR.; 
	run;

	*** GRAPHICAL OUTPUT *****************************************************; 

	title "&TITLE"; 
	ods graphics / labelmax= 3400;
	proc sgplot data= outc_2dscore &WHERE. ;
		scatter x= h31 y= h32 / 
		transparency= 0.35 
		group= CLUSTER 
		markerattrs= (size=  9 symbol= circleFilled)
		datalabel= &DROP_ID_VAR. 
		nomissinggroup; 
	run;
	title; 

%mend find_farthest_points; 

*** EXECUTE FOR PROVIDERS; 
%regress_with_diagnostics(provider_summary, provider_id, PROVIDER REGRESSION, &NUM_PROVIDER); 

*** DROP ADDITIONAL CORRELATED VAR; 
data provider_summary; 
	set provider_summary(drop= ave_num_service); 
run; 

%find_farthest_points(provider_summary, provider_id, name, Provider Clusters, 16, 10, ., WHERE= ); 
%find_farthest_points(provider_summary, provider_id, name, Provider Clusters, 16, 7, %str(390081, 310025, 50118), WHERE= ); 
%find_farthest_points(provider_summary, provider_id, name, Provider Clusters, 16, 0, %str(390081, 310025, 50118), WHERE= %str((where= (CLUSTER= 2)))); 

proc export 
	data= Profile_provider_summary
	outfile= "&OUT_DIR.&DSEP.ProviderClustersProfile.csv"
	dbms= csv
	replace; 
run;
proc export 
	data= Regout_by_abs_rstudent
	outfile= "&OUT_DIR.&DSEP.ProviderRegressionOutliers.csv"
	dbms= csv
	replace; 
run;

*** COLLAPSE SET BY REGION ID ************************************************;
 
proc sql noprint; 
	create table ref_summary as
	select unique ref_region,
		   mean(ave_provider_charge) as AVE_ave_provider_charge,
		   mean(ave_medicare_payment) as AVE_ave_medicare_payment,
		   mean(num_service) as AVE_num_service,
		   mean(level) as AVE_level,
		   /* sum(level0) as SUM_level0, */ /* 0,6,7 CORRELATED */
		   sum(level1) as SUM_level1,
		   /* sum(level2) as SUM_level2, */
		   sum(level3) as SUM_level3,
		   /* sum(level4) as SUM_level4, */ /* 2,3,4 CORRELATED */
		   sum(level5) as SUM_level5,
		   sum(level6) as SUM_level6,
		   /* sum(level7) as SUM_level7, */
		   MAX(university_flag) as MAX_university_flag 
	from summary
	group by ref_region;
quit; 

*** EXECUTE FOR REGIONS; 
%regress_with_diagnostics(ref_summary, ref_region, REGION REGRESSION, &NUM_REGION);  

data ref_summary; 
	set ref_summary (drop= ave_num_service);/* DROP ADDITIONAL CORRELATED VAR */
	ref_id= _n_; 							/* CREATE REQUIRED NUMERIC ID */
run; 

%find_farthest_points(ref_summary, ref_id, ref_region, Region Clusters, 7, 15, ., WHERE= );
%find_farthest_points(ref_summary, ref_id, ref_region, Region Clusters, 7, 15, %str(34, 58, 162), WHERE= ); 
%find_farthest_points(ref_summary, ref_id, ref_region, Region Clusters, 7, 5, %str(34, 58, 162), WHERE= %str((where= (CLUSTER= 2)))); 

proc export 
	data= Profile_ref_summary
	outfile= "&OUT_DIR.&DSEP.RegionClustersProfile.csv"
	dbms= csv
	replace; 
run;
proc export 
	data= Regout_by_abs_rstudent
	outfile= "&OUT_DIR.&DSEP.RegionRegressionOutliers.csv"
	dbms= csv
	replace; 
run;

******************************************************************************; 
* PART 3                                                                     *; 
******************************************************************************; 

*** IMPORT MANUAL REVIEW LIST ************************************************; 

%macro get_review_list(ASCII_FILE, DS); 

	%if ^%sysfunc(exist(net.&DS)) %then %do;
		data net.review; 
			
			infile "&ASCII_FILE" recfm= f lrecl= 1 end= eof; 
			length accum $32 id 8;  /* XML LIBNAME IS TREATING ID AS A NUMERIC, BE CAREFUL OF PREPADDED ZEROS */
			retain accum ' ';	    /* DOING THE SAME HERE FOR LATER MERGING */ 
			input x $char1.;  

			delim= x in ('0a'x);  
			if not delim then accum= trimn(accum)||x; 
			if not delim and not eof then return;  
			if x= '0a'x or eof then do; 
				id= strip(accum); 
				output; 
				accum= ' '; 
			end; 
			keep id; 

		run;
		proc sort; by id; run; 
	%end; 
 
	*** IF SET IS NOT ON LOCAL MACHINE, GET IT; 
	%get_local_copy(&DS);
	

%mend get_review_list; 
%get_review_list(&RAW_DIR.&DSEP.REVIEW.TXT, review);

*** CREATE PATIENT-PROCEDURE TRANSACTION TABLE *******************************;

%macro build_full_transaction_table; 

	%if ^%sysfunc(exist(net.transaction)) %then %do;
		%do i= 1 %to 12; 
			%if %eval(&i < 10) %then %let SUFFIX= 0&i; 
			%else %let SUFFIX= &i;  
			proc append base= net.transaction data= net.pcdr11&SUFFIX.; run; 
		%end; 
		proc sort sortsize= MAX threads; by id; run;
	%end; 
	%get_local_copy(transaction)

	%if ^%sysfunc(exist(net.fraudulent_transaction)) %then %do;
		data net.fraudulent_transaction; 
			merge net.transaction(in= a) net.review (in= b);
			by id; 
			if b; 
		run; 
 		proc sort; by id date; run;
	%end; 
	%get_local_copy(fraudulent_transaction);

%mend; 
%build_full_transaction_table; 

*** CREATE PATIENT SUMMARY TABLE *********************************************; 

%macro build_patient_summary_table; 

	%if ^%sysfunc(exist(net.patient_history)) %then %do;
		%do i= 1 %to 11;  
			proc append base= net.patient_history data= net.pntdump&i.; run; 
		%end; 
		proc sort nodupkey sortsize= MAX threads; by id; run;  
	%end;
	%get_local_copy(patient_history);

%mend; 
%build_patient_summary_table; 

******************************************************************************;
*** PART1 - PHASE 1: CLUSTER ANALYSIS ****************************************; 
******************************************************************************;

*** FLAG PATIENTS REVIEWED FOR FRAUD *****************************************; 

data patient_history; 
	merge patient_history review (in= x); 
	by id; 
	if x then REVIEW_FLAG= 1; 
	else REVIEW_FLAG= 0; 
	output; 
run; 

*** ENCODE PATIENT HISTORY INFO FOR LATER CLUSTERING *************************; 

*** GENDER; 
proc dmdb 
	data= patient_history (keep= id gender)
	out= patient_history_dmdb
	dmdbcat= work.patient_history_cat;
	class gender;  
	id id; 
run;
data patient_history; 
	merge patient_history patient_history_dmdb (rename= (gender= ENCODED_GENDER)); 
	by id; 
run; 

*** AGE; 
proc sort 
	data= patient_history (keep= id age) 
	out= age
	sortsize= MAX 
	threads
	sortseq= linguistic(numeric_collation= on);
	by age; 
run; 
proc dmdb 
	data= age 
	out= patient_history_dmdb
	dmdbcat= work.patient_history_cat;
	class age (data);  
	id id; 
run;
proc sort
	data= patient_history_dmdb
	sortsize= MAX 
	threads;
	by id;  
run; 
data patient_history; 
	merge patient_history patient_history_dmdb (rename= (age= ENCODED_AGE)); 
	by id; 
run; 

*** INCOME; 
proc sort 
	data= patient_history (keep= id income) 
	out= income
	sortsize= MAX 
	threads
	sortseq= linguistic(numeric_collation= on);
	by income; 
run; 
proc dmdb 
	data= income 
	out= patient_history_dmdb
	dmdbcat= work.patient_history_cat;
	class income (data);  
	id id; 
run;
proc sort
	data= patient_history_dmdb
	sortsize= MAX 
	threads;
	by id;  
run; 
data patient_history; 
	merge patient_history patient_history_dmdb (rename= (income= ENCODED_INCOME)); 
	by id; 
run; 

*** STANDARDIZE FOR LATER CLUSTER ANALYSIS; 
proc stdize 
	data= patient_history 
	out= patient_history_std
	outstat= os 
	method= mean;
    var ENCODED_:;
run;

*** CREATE DENSE (COO) REPRESENTATION OF SPARSE PATIENT-PROCEDURE MATRIX *****; 
*** FROM WHICH TO EFFICIENTLY GENERATE SVD FEATURES **************************; 

data proc_key_map; 
	set proc_key_map; 
	length procedure_code_short $6.;
	procedure_code_short= left(strip(substr(procedure_code,1,4))); 
run; 
proc sort; by procedure_code_short; run; 

proc sort 
	data= transaction(drop= date)
	out= transaction_coo
	sortsize= MAX
	threads; 
	by procedure_code; 
run; 
data transaction_coo; 
	merge transaction_coo proc_key_map (rename= (procedure_code_short= procedure_code) keep= global_proc_id procedure_code_short); 
	by procedure_code; 
	count= 1;
	drop procedure_code; 
run;  
proc sort data= transaction_coo sortsize= MAX threads; by id global_proc_id; run; 

*** CREATE SVD FEATURES FOR PATIENTS FROM DENSE REPRESENTATION ***************; 
*** FOR LATER CLUSTERING *****************************************************; 

*** THIS WILL REQUIRE DISTRIBUTED PROCESSING;
*** PUSH DATA TO DISTRIBUTED ENVIRONMENT;  
libname gridlib teradata
	server= 'tera2650' 
	user= pathal
	password= pathal
	database= hps;
option set= GRIDHOST= 'tms2650.unx.sas.com';
option set= GRIDATASERVER= 'tera2650';
option set= GRIDINSTALLLOC= '/opt/v940m2/laxno/TKGrid';
option set= GRIDMODE= 'sym';
data gridlib.ccp_transaction_coo (bulkload= yes 
                    			  dbcommit= 10000000 
                    		      dbcreate_table_opts= 'PRIMARY INDEX (id)'); 
    set transaction_coo;
run;

*** CREATE SVDS USING DISTRIBUTED PROCEDURE; 
%let GRID_TEXTANALYTICS_BIN_LOC=/rdstore/tktg/misc; 
proc hptmine 
	data= gridlib.ccp_transaction_coo; 
	svd 
		k= 10 
 		row= global_proc_id 
 		col= id 
 		entry= count
 		outdocpro= svdpro;  
	performance nodes= all; /* USE ALL AVAILABLE COMPUTE NODES */
run; 

*** MINOR POST PROCESSING; 
proc sort
	data= svdpro
	sortsize= MAX
	threads; 
	by id; 
run; 

*** CREATE SET OF CLUSTERING INPUTS ******************************************;  

data patient_history_std; 
	merge patient_history_std svdpro; 
	by id; 
run; 

*** CREATE MANY SMALL CLUSTERS ***********************************************; 

*** THIS WILL REQUIRE DISTRIBUTED PROCESSING; 
*** PUSH DATA TO DISTRIBUTED ENVIRONMENT; 
data gridlib.ccp_patient_history_std (bulkload= yes 
                    			      dbcommit= 10000000 
                    		          dbcreate_table_opts= 'PRIMARY INDEX (id)'); 
    set patient_history_std;
run;

*** CREATE CLUSTERS USING DISTRIBUTED PROCEDURE; 
proc hpclus 
	data= gridlib.ccp_patient_history_std
	outstat= patient_cluster_profile1000
	maxclusters= 1000
	maxiter= 100
	seed= 12345
	standardize= none
	impute= none
	noc= none; /* DONT ATTEMPT TO DETERMINE THE NUMBER OF CLUSTERS - VERY EXPENSIVE */
	input 
		ENCODED_GENDER 
		ENCODED_AGE 
		ENCODED_INCOME 
		COL1   /* SVD FEATURES: COL1-COL10 */  
		COL2
		COL3
		COL4
		COL5
		COL6
		COL7
		COL8
		COL9
		COL10; 
	id         /* COPY THESE TO THE OUTPUT SET */
		id
		REVIEW_FLAG
		FRAUD_RANK
		GENDER
		AGE
		INCOME
		COL1 
		COL2
		COL3
		COL4
		COL5
		COL6
		COL7
		COL8
		COL9
		COL10; 		
	score out= patient_cluster_label1000; 
	performance nodes= all; 
run; 

*** ANALYZE LOCATION OF REVIEW PATIENTS **************************************; 

proc freq data= patient_cluster_label1000(keep= REVIEW_FLAG _CLUSTER_ID_) noprint; 
	tables REVIEW_FLAG*_CLUSTER_ID_ / out= review_flag_freq; 
run; 
data review_flag_freq0; 
	set review_flag_freq (where= (REVIEW_FLAG= 0)
						  rename= (count= count0)
						  keep= _CLUSTER_ID_ count REVIEW_FLAG);
	drop REVIEW_FLAG; 
run;  
data review_flag_freq1; 
	set review_flag_freq (where= (REVIEW_FLAG= 1)
						  rename= (count= count1)
						  keep= _CLUSTER_ID_ count REVIEW_FLAG);
	drop REVIEW_FLAG; 
run;  

*** SCORE SUSPICIOUS CLUSTERS; 
data review_clusters; 
	merge review_flag_freq0 review_flag_freq1 (in= x);
	by _CLUSTER_ID_; 
	if x; 
	PERCENT_REVIEW= count1/(count0+count1); 
	CLUSTER_REVIEW_SCORE= floor(1000*PERCENT_REVIEW); 
run; 
proc sort; by descending PERCENT_REVIEW; run; 
data patient_cluster_fraud_rank (keep= id _CLUSTER_ID_); 
	set patient_cluster_label1000; 
run; 

*** SCORE SUSPICIOUS INDIVIDUALS USING SUSPICIOUS CLUSTER SCORES; 
proc sort sortsize= MAX threads; by _CLUSTER_ID_; run; 
proc sort data= review_clusters; by _CLUSTER_ID_; run; 
data patient_cluster_fraud_rank; 
	merge patient_cluster_fraud_rank review_clusters(keep= _CLUSTER_ID_ CLUSTER_REVIEW_SCORE);
	by _CLUSTER_ID_; 
	if CLUSTER_REVIEW_SCORE= . then  CLUSTER_REVIEW_SCORE= 0; 
run;
proc sort sortsize= MAX threads; by id; run;

******************************************************************************;
*** PHASE 2: ASSOCIATION ANALYSIS ********************************************; 
******************************************************************************; 
 
*** FIND FREQUENT ITEM SETS IN THE REVIEWED PATIENT TRANSACTIONS *************; 
*** 2 ITEM SETS/0.1 PERCENT SUPPORT ******************************************; 

proc dmdb 
	data= fraudulent_transaction(keep= id procedure_code)
	out= fraudulent_transaction_dmdb
	dmdbcat= work.fraudulent_transaction_cat;
	class procedure_code;  
	var id;  
run;
proc assoc 
	data= fraudulent_transaction(keep= id procedure_code)
	dmdbcat= work.fraudulent_transaction_cat
	out= freq_fraud_trans_group
	items= 2
	support= 150;  
	customer id; 
	target procedure_code; 
run; 

*** FIND FREQUENT ITEM SETS IN ALL TRANSACTIONS  *****************************; 
*** 2 ITEM SETS/0.1 PERCENT SUPPORT ******************************************;

proc dmdb 
	data= transaction(keep= id procedure_code)
	out= transaction_dmdb
	dmdbcat= work.transaction_cat;
	class procedure_code;
	var id;  
run;
proc assoc 
	data= transaction(keep= id procedure_code)
	dmdbcat= work.transaction_cat
	out= freq_trans_group
	items= 2
	support= 300000;  
	customer id; 
	target procedure_code; 
run; 

*** COMMON REVIEWED 2-ITEM SETS;
proc sort 
	data= freq_trans_group(where= (SET_SIZE= 2) keep= SET_SIZE ITEM1 ITEM2)
	out= freq_2_item_trans(keep= ITEM1 ITEM2); 
	by ITEM1 ITEM2; 
run; 
proc sort 
	data= freq_fraud_trans_group(where= (SET_SIZE= 2) keep= SET_SIZE ITEM1 ITEM2)
	out= freq_fraud_2_item_trans(keep= ITEM1 ITEM2); 
	by ITEM1 ITEM2; 
run; 
data suspicious_2_item; 
	merge freq_2_item_trans(in= a) freq_fraud_2_item_trans(in= b); 
	by ITEM1 ITEM2;
	if ^a and b;  
run; 

*** FIND WHICH PATIENTS HAVE THESE SAME 2-ITEM SETS ****************************; 

*** INIT TRANS SET FOR LATER MERGES; 
proc sort data= transaction sortsize= MAX threads; by procedure_code; run; 

*** FIND UNIQUE ITEMS IN SUSPICIOUS 2-ITEM SETS; 
*** JUST TO SUBSET TRANSACTIONS FOR FASTER PROCESSING; 
data _1; 
	set suspicious_2_item; 
	keep ITEM1; 
run; 
data _2; 
	set suspicious_2_item(drop= ITEM1 rename= (ITEM2= ITEM1));
	keep ITEM1; 
run; 
proc append base= _1 data= _2; run;  
proc sort data= _1 nodupkey; by ITEM1; run; 
data one_item_patient_proc;  
	merge transaction(in= a drop= date) _1(rename= (ITEM1= procedure_code) in= b);
	by procedure_code; 
	if a and b; 
run;

*** INIT ASSOC_REVIEW_SCORE VAR AND PATIENT_HISTORY_ASSOC_REVIEW SET; 
data patient_history_assoc_review; 
	set net.patient_history_std (keep= id); 
	ASSOC_REVIEW_SCORE= 0; 
run;  
proc sort sortsize= MAX threads; by id; run; 

*** FIND TWO ITEM SETS-MUCH MORE RARE, USEFUL;  
%macro two_item_set; 

	%let DSID= %sysfunc(open(suspicious_2_item)); 
	%let NOBS= %sysfunc(attrn(&DSID, NLOBS)); 
	%let rc= %sysfunc(close(&DSID)); 

	%do i= 1 %to &NOBS; 

		data two_item_set&i.; 
			set suspicious_2_item; 
			if _n_= &i; 
		run; 
		proc transpose 
			out= two_item_set&i. (rename= (COL1= procedure_code) drop= _NAME_); 
			var item:; 
		run;
		data two_item_patient&i.;  
			merge one_item_patient_proc(in= a) two_item_set&i. (in= b);
			by procedure_code; 
			if a and b; 
		run;
		proc sort data= two_item_patient&i. sortsize= MAX threads; by id procedure_code; run; 
		data two_item_patient&i.; 
			set two_item_patient&i.; 
			by id procedure_code; 
			if ^(first.id and last.id);  
			drop procedure_code; 
		run; 
		proc sort sortsize= MAX threads nodupkey; by id; run; 
		data patient_history_assoc_review; 
			merge patient_history_assoc_review two_item_patient&i. (in= x);
			by id; 
			if x then ASSOC_REVIEW_SCORE= ASSOC_REVIEW_SCORE+1;
			output;  
		run; 

	%end; 

	*** CHECK/SUMMARIZE RESULTS; 
	proc freq data= patient_history_assoc_review(keep= ASSOC_REVIEW_SCORE);
		tables ASSOC_REVIEW_SCORE; 
	run;

	*** SAVE RESULTS OUT TO NETWORK STORAGE; 
	data net.patient_history_assoc_review; 
		set patient_history_assoc_review; 
	run; 

%mend; 
%two_item_set; 

*** CREATE FINAL FRAUD RANKING ***********************************************; 
 
data patient_history_std; 
	merge patient_history_std 
		  patient_history_assoc_review; 
	by id; 
	FRAUD_RANK= CLUSTER_REVIEW_SCORE + ASSOC_REVIEW_SCORE; 
run; 

*** CHECK/SUMMARIZE RESULTS; 
proc freq data= patient_history_std (keep= REVIEW_FLAG);
	tables REVIEW_FLAG; 
run; 
proc freq data= patient_history_std (keep= FRAUD_RANK REVIEW_FLAG where= (REVIEW_FLAG= 1));
	tables FRAUD_RANK; 
run;
proc freq data= patient_history_std (keep= FRAUD_RANK REVIEW_FLAG where= (REVIEW_FLAG= 0));
	tables FRAUD_RANK; 
run;
proc freq data= patient_history_std (keep= ASSOC_REVIEW_SCORE REVIEW_FLAG where= (REVIEW_FLAG= 1));
	tables ASSOC_REVIEW_SCORE; 
run;
proc freq data= patient_history_std (keep= ASSOC_REVIEW_SCORE REVIEW_FLAG where= (REVIEW_FLAG= 0));
	tables ASSOC_REVIEW_SCORE; 
run;  
proc freq data= patient_history_std (keep= CLUSTER_REVIEW_SCORE REVIEW_FLAG where= (REVIEW_FLAG= 1));
	tables CLUSTER_REVIEW_SCORE; 
run;
proc freq data= patient_history_std (keep= CLUSTER_REVIEW_SCORE REVIEW_FLAG where= (REVIEW_FLAG= 0));
	tables CLUSTER_REVIEW_SCORE; 
run;  

*** CREATE SUBMISSION ********************************************************;

proc sort data= review; by id; run;  
data part3; 
	length cid $9.;
	merge patient_history_std(keep= FRAUD_RANK ASSOC_REVIEW_SCORE CLUSTER_REVIEW_SCORE id) review(in= x); 
	if ^x; 
	by id;
	cid= translate(right(put(id, 9.)),'0',' ');
	drop id; 
run; 
proc sort 
	data= part3 
	sortsize= MAX 
	threads; 
	by descending FRAUD_RANK; 
run; 
data _null_; 
	length line $9;
	set part3 (obs= 10000);
	file "&OUT_DIR.&DSEP.part3.csv"; 
	line= strip(cid);
	put line;
run; 
