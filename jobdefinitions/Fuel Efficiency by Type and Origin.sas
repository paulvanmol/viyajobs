/* _STARTMACROCODE_ */
/*----------------------------------------------------------------------
/
/ SASStudio initialization file for SAS workspace connections
/
*----------------------------------------------------------------------*/

/* Get the Git version */
DATA _NULL_;
  GITVERSION = GIT_VERSION();
  CALL SYMPUT('_GITVERSION', GITVERSION);
RUN;


%MACRO resolveHomeDirectory;
	%GLOBAL _USERHOME;
	%LOCAL _HOMEVAR;

	%IF (&SYSSCP=WIN) %THEN
		%DO;
			%LET _HOMEVAR=USERPROFILE;
		%END;
	%ELSE
		%DO;
			%LET _HOMEVAR=HOME;
		%END;

	%LET _USERHOME= %SYSFUNC(SYSGET(&_HOMEVAR));

%MEND;

%resolveHomeDirectory;


%macro web_open_file(name,type);
%global _DATAOUT_NAME;
%global _DATAOUT_MIME_TYPE;
%let _DATAOUT_NAME=&name;
%let _DATAOUT_MIME_TYPE=&type;
%mend;

%macro web_open_table(table);
%global _DATAOUT_TABLE;
%if %length(&_dataout_table)=0 %then %let _DATAOUT_TABLE=&table;
%else %let _DATAOUT_TABLE=&_DATAOUT_TABLE,&table;
%mend;

%macro web_open_url(url);
%global _DATAOUT_URL;
%let _DATAOUT_URL=&url;
%mend;

%macro sgdesign();
%put ERROR: SGDesigner macro can not be invoked from SAS Studio.;
%mend;

%macro sgedit();
%put ERROR: SGEdit macro can not be invoked from SAS Studio.;
%mend;

%macro web_list_entries(catalog,type);

%let typearg=;
%let type=%upcase(&type);
%if &type^=_ALL_ and &type^=_all_ %then %let typearg= entrytype=&type;

proc catalog catalog=&catalog &typearg;
contents;
title "Catalog Entries in &catalog";
run;
quit;

%mend;

%macro web_list_catalogs(library);
	%let library=%upcase(&library);
    proc sql ;
        create table work.catalogs as select memname as Catalog, memtype as 
            Type, engine as Engine from sashelp.vmember where 
            libname="&library" and memtype="CATALOG";
        run;
        quit;
        title "Catalogs in &library";

    proc print data=work.catalogs;
    run;
%mend;

%macro web_replay_grseg(catalog,entry);
proc greplay nofs igout=&catalog;
replay &entry;
run;
%mend;

%macro sasstudio_get_zos_ds_info(DSN); 

OPTIONS FILESYSTEM=MVS;
                                                                                             
DATA work._sasstudio_dataset_info_;                                                                                                                                                                                                                                    
                                                                                                              
  LENGTH XDSN $32000;                                                                                                                                                                                                                            
  XDSN=ZDSLIST('CATLG', &DSN, XVOLS, '');                                         
                                                                                                                           
  LENGTH XNUM 8;                                                                                                                
  XNUM=ZDSNUM(XDSN);                                                                                                            
  LENGTH XIDNM $4096;                                                                                                           
  LENGTH XATTR $4096;                                                                                                           
  LENGTH XATTRC $72;                                                                                                            
  LENGTH XATTRN 8;  
  LENGTH attributeType $10;
                                                                                                            
  DO I=1 to XNUM;                                                                                                               
    XIDNM=ZDSIDNM(XDSN, I);                                                                                                     
    XATTR=ZDSATTR(XIDNM, 'ALL');                                                                                                
    XATTRN=ZDSXATT(XATTR);                                                                                                      
                                                                                                                                           
    do j=1 to xattrn;                                                                                                           
     XATTRC=ZDSYATT(XATTR, j);                                                                                                  
     blank_pos = index(xattrc," ");
		   key = substr(xattrc, blank_pos,99);
		   val= substr(xattrc,1,blank_pos);
		   pos = index(key, 'FILE ATTRIBUTES');
		   if (pos > 0) then do;
		      attributeType = 'FILE';
		   end;
		   else do;
			   pos = index(key, 'VOLUME ATTRIBUTES');
			   if (pos > 0) then do;
			      attributeType = 'VOLUME';
			   end;
			   else do;
				   pos = index(key, 'MIGRATED DATA SET ATTRIBUTES');
				   if (pos > 0) then do;
				      attributeType = 'MIGRATED';
				   end;
				   else do;
				      pos = index(key, '*****');
				   end;
			   end;		   
		   end;	   
		   if (pos = 0) then do;
		      output;
		   end; 
		   keep key val attributeType;                                                                                                            
    end;                                                                                                                        
  END;                                                                                                                          
 RUN;                                                                                                                           
%mend sasstudio_get_zos_ds_info;

%macro show_zos_dataset_attributes(dsn);
   %sasstudio_get_zos_ds_info(&dsn);
   
   data work._sasstudio_dataset_info_ds_;
       set work._sasstudio_dataset_info_;
      
       if strip(val)='.' then do; 
           val='***NONE***';
       end;
     
       position = index(attributeType, 'FILE');
       if position > 0 then do;
           output;
       end;
       drop position attributeType;
   run;
   
   data work._sasstudio_dataset_info_vol_;
       set work._sasstudio_dataset_info_;
       
       if strip(val)='.' then do; 
           val='***NONE***';
       end;
       
       position = index(attributeType, 'VOLUME');
       if position > 0 then do;
           output;
       end;
       drop position attributeType;
   run;
	
	proc print data=work._sasstudio_dataset_info_ds_ noobs label;
	LABEL key='Dataset Attribute'  val='00'x;
	   title1 &dsn;
	run;
	
	proc print data=work._sasstudio_dataset_info_vol_ noobs label;
	    title1;
		LABEL key='Volume Attribute'  val='00'x;
	run;

	proc delete data=work._sasstudio_dataset_info_
	work._sasstudio_dataset_info_ds_
	work._sasstudio_dataset_info_vol_;

%mend;

%macro validCasEnvironment( sessionName=, createSession=, returnCode=RC);

/* This section is just to give information about the CAS connection */
%let CASHOST = %SYSFUNC(GETOPTION(CASHOST));

%if (%LENGTH(&CASHOST)=0) %then %do;
    %put CASHOST is not set;
%end; %else %do;
    %put CASHOST is &CASHOST;
%end;

%let CASPORT = %SYSFUNC(GETOPTION(CASPORT));

%if (&CASPORT=0) %then %do;
    %put CASPORT is not set;
%end; %else %do;
    %put CASPORT is &CASPORT;
%end;

%let CASHOST = %SYSFUNC(GETOPTION(_CASHOST_));
%if (%LENGTH(&CASHOST)=0) %then %do;
    %put _CASHOST_ is not set;
%end; %else %do;
    %put _CASHOST_ is &CASHOST;
%end;

%let CASPORT = %SYSFUNC(GETOPTION(_CASPORT_));

%if (%LENGTH(&CASPORT)=0) %then %do;
    %put _CASPORT_ is not set;
%end; %else %do;
    %put _CASPORT_ is &CASPORT;
%end;

%let CASUSER = %SYSFUNC(GETOPTION(CASUSER));

%if (%LENGTH(&CASUSER) = 0) %then %do;
    %put CASUSER is not set;
%end; %else %do;
    %put CASUSER is &CASUSER;
%end;


%if &sessionName = %then %do;
 	%let DEFAULTSESS= %SYSFUNC(GETOPTION(SESSREF)); 
	%let SESSIONFOUND = %SYSFUNC(SESSFOUND(&DEFAULTSESS));
	%if (&SESSIONFOUND = 0) %then %do;
	   %put Default session &DEFAULTSESS is not available.;
       %let &returnCode=0;
    %end;
    %else %do;
	   %put Default session &DEFAULTSESS is available.;
       %let &returnCode=3;
    %end;
%end; %else %do;                                                /* Session name was specified */
	%if &createSession = %then %do;                             /* createSession not specified */
	   %let SESSIONFOUND = %SYSFUNC(SESSFOUND(&sessionName));   /* Default to false (don't create) */
	   %if (&SESSIONFOUND = 0) %then %do;
	      %put Session &sessionName is not available.;
          %let &returnCode=0;
       %end;	
       %else %do;
          %put Session &sessionName already exists.;
          %let &returnCode=2;
       %end;
    %end; 
    %else %do;
	   %let SESSIONFOUND = %SYSFUNC(SESSFOUND(&sessionName));  /* Does session already exist? */
       %if (&createSession = false) %then %do;                 /* User does not want to create it. */
          %put createSession is false;
	      %if (&SESSIONFOUND = 0) %then %do;
	         %put Session &sessionName is not available.;      /* Tell user it does not exist. */
             %let &returnCode=0;
          %end;	
          %else %do;
             %put Session &sessionName exists;
             %let &returnCode = 2;
          %end;	      
	   %end;
	   %else %do;
         %put createSession is true;
         %if (&SESSIONFOUND = 0) %then %do;                    /* Session does not exist, so create it */
             cas &sessionName;
             %let SESSIONFOUND = %SYSFUNC(SESSFOUND(&sessionName)); 
             %if (&SESSIONFOUND = 0) %then %do;               /* Session created successfully */
                 %let &returnCode=1;
             %end;
             %else %do;
                 %let &returnCode=4;                          /* Session creation failed. */
             %end;
         %end;
         %else %do;                                           /* Session already exists, so user cannot create it */                                           
             %put Session &sessionName already exists;
             %let &returnCode=2;
         %end;
	   %end;
	%end;
%end;


%mend validCasEnvironment;

/* This macro is used by code generated for Query nodes in an EG Process flow *
/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;

/* Given a fileref and a memname and memtype, we attempt to open the
member of the directory (catalog or file system directory). We
set &member_found to 1 if it can be opened, 0 if not. */
%macro _entry_exists(fileref,memname,memtype);
%global _macro_found;
%let _macro_found = 0;
data _null_;
*-----open the directory and proceed if it can be opened-----*;
handle = dopen("&fileref.");
if handle ne 0;
*-----open the member and set the macro variable based on result-----*;
mem_handle = mopen(handle,"&memname..&memtype.",'i');
call symputx('_macro_found',mem_handle ne 0);
*-----close the member if it were opened successfully-----*;
if mem_handle then rc = fclose(mem_handle);
*-----close the directory-----*;
rc = dclose(handle);
run;
%mend _entry_exists;

/* Given a macro name, we determine if it has already been
compiled. We first look in work.sasmacr, then in the sasmacr
referenced by sasmstore (if given) and then in work.sasmacX. */
%macro _compiled_macro_exists(macro_name);
options nonotes;
%global _macro_found;
*-----try work.sasmacr first to see if the compiled macro is there-----*;
filename maclib catalog "work.sasmacr";
%_entry_exists(maclib,&macro_name.,macro);
filename maclib clear;
%if &_macro_found %then %goto done;
*-----try sasmacr referenced by sasmstore if it were specified-----*;
%let sasmstore_option = %sysfunc(getoption(sasmstore));
%if %sysfunc(getoption(mstored))=MSTORED and %length(&sasmstore_option) > 0 %then %do;
filename maclib catalog "&sasmstore_option..sasmacr";
%_entry_exists(maclib,&macro_name.,macro);
%end;

%do i=1 %to 9;
%if &_macro_found %then %goto done;
filename maclib catalog "work.sasmac&i."; 
%_entry_exists(maclib,&macro_name.,macro);
filename maclib clear; 
%end;

%done: options notes;
%mend _compiled_macro_exists;

%macro studio_cas_start;

%global _macro_found;
%global syscasinit;

%let syscasinit=0;

%_compiled_macro_exists(studio_cas_init);

%if &_macro_found %then %do;

%let syscasinit=1;

%studio_cas_init;

%end;
%mend studio_cas_start;

%macro studio_hide_wrapper;
	%global _studionotes;
	%global _studiosource;
	%global _studiostimer;

    %if &_studionotes = %then %do;
	    %let _studionotes=%sysfunc(getoption(notes));
	%end;
	options nonotes;

    %if &_studiosource = %then %do;
	    %let _studiosource=%sysfunc(getoption(source));
	%end;
	options nosource;

    %if &_studiostimer = %then %do;
	    %let _studiostimer=%sysfunc(getoption(stimer));
	%end;
    options nostimer;
%mend studio_hide_wrapper;

%macro studio_show_wrapper;
	%global _studionotes;
	%global _studiosource;
	%global _studiostimer;

    %if &_studionotes = %then %do;
	    %let _studionotes=%sysfunc(getoption(notes));
	%end;
	options notes;

    %if &_studiosource = %then %do;
	    %let _studiosource=%sysfunc(getoption(source));
	%end;
	options source;

    %if &_studiostimer = %then %do;
	    %let _studiostimer=%sysfunc(getoption(stimer));
	%end;
    options stimer;
%mend studio_show_wrapper;

%macro studio_show_only_notes_wrapper;
	%global _studionotes;
	%global _studiosource;
	%global _studiostimer;

    %if &_studionotes = %then %do;
	    %let _studionotes=%sysfunc(getoption(notes));
	%end;
	options notes;

    %if &_studiosource = %then %do;
	    %let _studiosource=%sysfunc(getoption(source));
	%end;
	options nosource;

    %if &_studiostimer = %then %do;
	    %let _studiostimer=%sysfunc(getoption(stimer));
	%end;
    options nostimer;
%mend studio_show_only_notes_wrapper;


%macro studio_restore_wrapper;
    %global _studionotes;
    %global _studiosource;
    %global _studiostimer;

	options &_studionotes;
	options &_studiosource;
	options &_studiostimer;

	/* Clear out values so we know they have been restored */
	%let _studionotes=;
	%let _studiosource=;
	%let _studiostimer=;
%mend studio_restore_wrapper;

options timezone='GMT-04:00';
%let requestedlocale=en-US;
%let validlocale=%SYSFUNC(getpxlocale(&requestedlocale));
%let validlocale=%trim(&validlocale);
%if %length(&validlocale) > 0 %then %do;options locale=&validlocale DFLANG=LOCALE;%end; %else %do;options locale=en_US DFLANG=LOCALE;%end;DATA _NULL_;
	ID=SYMGET("SYSUSERID");
	CALL SYMPUT("SYSUSERNAME",ID);
	RC=TSLVL('UWUUSERN','N');
	_ERROR_=0;
	IF (RC^=' ') THEN DO;
		call execute("DATA _NULL_;NAME=USERNAME();CALL SYMPUT('SYSUSERNAME',NAME);RUN;");
	END;
RUN;

FILENAME _keepalv TEMP;

options validvarname=any;
options validmemname=extend;

/* _ENDMACROCODE_ */
*  Begin EG generated code (do not edit this line); 
* 
*  Stored process registered by 
*  Enterprise Guide Stored Process Manager V8.3 
* 
*  ==================================================================== 
*  Stored process name: Fuel Efficiency by Type and Origin 
*  ==================================================================== 
* 
*  Stored process prompt dictionary: 
*  ____________________________________ 
*  ORIGIN 
*       Type: Text 
*      Label: origin 
*       Attr: Visible, Required 
*    Default: <ALL> 
*  ____________________________________ 
*; 
 
 
*ProcessBody; 
 
%global ORIGIN; 
 
%STPBEGIN; 
 
*  End EG generated code (do not edit this line); 
 
 
/* --- Start of shared macro functions. --- */ 
/* Conditionally delete set of tables or views, if they exists          */ 
/* If the member does not exist, then no action is performed   */ 
%macro _eg_conditional_dropds /parmbuff; 
  
    %local num; 
    %local stepneeded; 
    %local stepstarted; 
    %local dsname; 
 %local name; 
 
    %let num=1; 
 /* flags to determine whether a PROC SQL step is needed */ 
 /* or even started yet                                  */ 
 %let stepneeded=0; 
 %let stepstarted=0; 
    %let dsname= %qscan(&syspbuff,&num,',()'); 
 %do %while(&dsname ne);  
  %let name = %sysfunc(left(&dsname)); 
  %if %qsysfunc(exist(&name)) %then %do; 
   %let stepneeded=1; 
   %if (&stepstarted eq 0) %then %do; 
    proc sql; 
    %let stepstarted=1; 
 
   %end; 
    drop table &name; 
  %end; 
 
  %if %sysfunc(exist(&name,view)) %then %do; 
   %let stepneeded=1; 
   %if (&stepstarted eq 0) %then %do; 
    proc sql; 
    %let stepstarted=1; 
   %end; 
    drop view &name; 
  %end; 
  %let num=%eval(&num+1); 
       %let dsname=%qscan(&syspbuff,&num,',()'); 
 %end; 
 %if &stepstarted %then %do; 
  quit; 
 %end; 
%mend _eg_conditional_dropds; 
 
/* Build where clauses from stored process parameters */ 
%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=-ALL-, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0, MATCH_CASE=1); 
 
  %local q1 q2 sq1 sq2; 
  %local isEmpty; 
  %local isEqual isNotEqual; 
  %local isIn isNotIn; 
  %local isString; 
  %local isBetween; 
 
  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "="); 
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>"); 
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN"); 
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN"); 
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING ); 
  %if &isString %then 
  %do; 
 %if "&MATCH_CASE" eq "0" %then %do; 
  %let COLUMN = %str(UPPER%(&COLUMN%)); 
 %end; 
 %let q1=%str(%"); 
 %let q2=%str(%"); 
 %let sq1=%str(%');  
 %let sq2=%str(%');  
  %end; 
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then  
  %do; 
    %let q1=%str(%"); 
    %let q2=%str(%"d); 
 %let sq1=%str(%');  
    %let sq2=%str(%');  
  %end; 
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then 
  %do; 
    %let q1=%str(%"); 
    %let q2=%str(%"t); 
 %let sq1=%str(%');  
    %let sq2=%str(%');  
  %end; 
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then 
  %do; 
    %let q1=%str(%"); 
    %let q2=%str(%"dt); 
 %let sq1=%str(%');  
    %let sq2=%str(%');  
  %end; 
  %else 
  %do; 
    %let q1=; 
    %let q2=; 
 %let sq1=; 
    %let sq2=; 
  %end; 
   
  %if "&PARM" = "" %then %let PARM=&COLUMN; 
 
  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN"); 
 
  %if "&MAX" = "" %then %do; 
    %let MAX = &parm._MAX; 
    %if &isBetween %then %let PARM = &parm._MIN; 
  %end; 
 
  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do; 
    %if &IS_EXPLICIT=0 %then %do; 
  not &MATCHALL_CLAUSE 
 %end; 
 %else %do; 
     not 1=1 
 %end; 
  %end; 
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do; 
    %if &IS_EXPLICIT=0 %then %do; 
     &MATCHALL_CLAUSE 
 %end; 
 %else %do; 
     1=1 
 %end;  
  %end; 
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do; 
    %let isEmpty = ("&&&PARM" = ""); 
    %if (&isEqual AND &isEmpty AND &isString) %then 
       &COLUMN is null; 
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then 
       &COLUMN is not null; 
    %else %do; 
    %if &IS_EXPLICIT=0 %then %do; 
           &COLUMN &OPERATOR  
   %if "&MATCH_CASE" eq "0" %then %do; 
    %unquote(&q1)%QUPCASE(&&&PARM)%unquote(&q2) 
   %end; 
   %else %do; 
    %unquote(&q1)&&&PARM%unquote(&q2) 
   %end; 
    %end; 
    %else %do; 
        &COLUMN &OPERATOR  
   %if "&MATCH_CASE" eq "0" %then %do; 
    %unquote(%nrstr(&sq1))%QUPCASE(&&&PARM)%unquote(%nrstr(&sq2)) 
   %end; 
   %else %do; 
    %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)) 
   %end; 
    %end; 
       %if &isBetween %then  
          AND %unquote(&q1)&&&MAX%unquote(&q2); 
    %end; 
  %end; 
  %else  
  %do; 
 %local emptyList; 
   %let emptyList = %symexist(&PARM._count); 
   %if &emptyList %then %let emptyList = &&&PARM._count = 0; 
 %if (&emptyList) %then 
 %do; 
  %if (&isNotin) %then 
     1; 
  %else 
   0; 
 %end; 
 %else %if (&&&PARM._count = 1) %then  
    %do; 
      %let isEmpty = ("&&&PARM" = ""); 
      %if (&isIn AND &isEmpty AND &isString) %then 
        &COLUMN is null; 
      %else %if (&isNotin AND &isEmpty AND &isString) %then 
        &COLUMN is not null; 
      %else %do; 
     %if &IS_EXPLICIT=0 %then %do; 
   %if "&MATCH_CASE" eq "0" %then %do; 
    &COLUMN &OPERATOR (%unquote(&q1)%QUPCASE(&&&PARM)%unquote(&q2)) 
   %end; 
   %else %do; 
    &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2)) 
   %end; 
     %end; 
  %else %do; 
      &COLUMN &OPERATOR ( 
   %if "&MATCH_CASE" eq "0" %then %do; 
    %unquote(%nrstr(&sq1))%QUPCASE(&&&PARM)%unquote(%nrstr(&sq2))) 
   %end; 
   %else %do; 
    %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))) 
   %end; 
  %end; 
   %end; 
    %end; 
    %else  
    %do; 
       %local addIsNull addIsNotNull addComma; 
       %let addIsNull = %eval(0); 
       %let addIsNotNull = %eval(0); 
       %let addComma = %eval(0); 
       (&COLUMN &OPERATOR (  
       %do i=1 %to &&&PARM._count;  
          %let isEmpty = ("&&&PARM&i" = ""); 
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then 
          %do; 
             %if (&isIn) %then %let addIsNull = 1; 
             %else %let addIsNotNull = 1; 
          %end; 
          %else 
          %do;        
            %if &addComma %then %do;,%end; 
   %if &IS_EXPLICIT=0 %then %do; 
    %if "&MATCH_CASE" eq "0" %then %do; 
     %unquote(&q1)%QUPCASE(&&&PARM&i)%unquote(&q2) 
    %end; 
    %else %do; 
     %unquote(&q1)&&&PARM&i%unquote(&q2) 
    %end; 
   %end; 
   %else %do; 
    %if "&MATCH_CASE" eq "0" %then %do; 
     %unquote(%nrstr(&sq1))%QUPCASE(&&&PARM&i)%unquote(%nrstr(&sq2)) 
    %end; 
    %else %do; 
     %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
    %end;  
   %end; 
            %let addComma = %eval(1); 
          %end; 
       %end;)  
       %if &addIsNull %then OR &COLUMN is null; 
       %else %if &addIsNotNull %then AND &COLUMN is not null; 
       %do;) 
       %end; 
    %end; 
  %end; 
%mend _eg_WhereParam; 
 
/* --- End of shared macro functions. --- */ 
 
/* --- Start of code for "Query Builder". --- */ 
%_eg_conditional_dropds(WORK.QUERY_FOR_CARS); 
 
PROC SQL; 
   CREATE TABLE WORK.QUERY_FOR_CARS AS  
   SELECT t1.Make,  
          t1.Model,  
          t1.Type,  
          t1.Origin,  
          t1.DriveTrain,  
          t1.MSRP,  
          t1.Invoice,  
          t1.EngineSize,  
          t1.Cylinders,  
          t1.Horsepower,  
          t1.MPG_City,  
          t1.MPG_Highway,  
          t1.Weight,  
          t1.Wheelbase,  
          t1.Length 
      FROM SASHELP.CARS t1 
      WHERE %_eg_WhereParam( t1.Origin, origin, EQ, TYPE=S, IS_EXPLICIT=0 ); 
QUIT; 
/* --- End of code for "Query Builder". --- */ 
 
/* --- Start of code for "Summary Statistics". --- */ 
/* ------------------------------------------------------------------- 
   Code generated by SAS Task 
 
   Generated on: Monday, August 28, 2023 at 2:12:08 AM 
   By task: Summary Statistics 
 
   Input Data: SASApp:WORK.QUERY_FOR_CARS 
   Server:  SASApp 
   ------------------------------------------------------------------- */ 
 
 
/* Start of custom user code */ 
ods noproctitle; 
/* End of custom user code */ 
%_eg_conditional_dropds(WORK.SORTTempTableSorted); 
/* ------------------------------------------------------------------- 
   Sort data set SASApp:WORK.QUERY_FOR_CARS 
   ------------------------------------------------------------------- */ 
 
PROC SQL; 
 CREATE VIEW WORK.SORTTempTableSorted AS 
  SELECT T.MPG_Highway, T.Type 
 FROM WORK.QUERY_FOR_CARS as T 
; 
QUIT; 
/* ------------------------------------------------------------------- 
   Run the Means Procedure 
   ------------------------------------------------------------------- */ 
TITLE; 
TITLE1 "Summary statistics grouped by Type  for Origin &origin"; 
FOOTNOTE; 
PROC MEANS DATA=WORK.SORTTempTableSorted 
 FW=12 
 PRINTALLTYPES 
 CHARTYPE 
 NWAY 
 VARDEF=DF   
  MEAN  
  STD  
  MIN  
  MAX  
  N ; 
 VAR MPG_Highway; 
 CLASS Type / ORDER=UNFORMATTED ASCENDING; 
 
RUN; 
ODS GRAPHICS ON; 
TITLE; 
TITLE1 "Summary Statistics"; 
TITLE2 "Box and Whisker Plots"; 
PROC SGPLOT DATA=WORK.SORTTempTableSorted ; 
 VBOX MPG_Highway / category=Type; 
RUN;QUIT; 
ODS GRAPHICS OFF; 
/* ------------------------------------------------------------------- 
   End of task code 
   ------------------------------------------------------------------- */ 
RUN; QUIT; 
%_eg_conditional_dropds(WORK.SORTTempTableSorted); 
TITLE; FOOTNOTE; 
 
/* --- End of code for "Summary Statistics". --- */ 
 
*  Begin EG generated code (do not edit this line); 
;*';*";*/;quit; 
%STPEND; 
 
*  End EG generated code (do not edit this line); 

