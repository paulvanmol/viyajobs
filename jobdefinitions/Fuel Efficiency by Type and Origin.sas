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
%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0, MATCH_CASE=1); 
 
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

