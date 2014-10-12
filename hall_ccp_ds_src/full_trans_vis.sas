*------------------------------------------------------------*;
* EM Version: 13.2;
* SAS Release: 9.04.01M2P062414;
* Host: LINCOLN;
* Project Path: D:\All_Users\pathal\EMProj;
* Project Name: ccp;
* Diagram Id: EMWS3;
* Diagram Name: assoc/rules;
* Generated by: pathal;
* Date: 26JUN2014:01:49:47;
*------------------------------------------------------------*;
*------------------------------------------------------------*;
* Macro Variables;
*------------------------------------------------------------*;
%let EM_PROJECT =;
%let EM_PROJECTNAME =;
%let EM_WSNAME =;
%let EM_WSDESCRIPTION =assoc/rules;
%let EM_SUMMARY =WORK.SUMMARY;
%let EM_NUMTASKS =SINGLE;
%let EM_EDITMODE =R;
%let EM_DEBUGVAL =;
%let EM_ACTION =run;
*------------------------------------------------------------*;
%macro em_usedatatable;
%if ^%symexist(EM_USEDATATABLE) %then %do;
%let EM_USEDATATABLE = Y;
%end;
%if "&EM_USEDATATABLE" ne "N" %then %do;
%global Ids_data Ids_newdata;
*------------------------------------------------------------*;
* Data Tables;
*------------------------------------------------------------*;
%let Ids_data = CCP.TRANSACTION;
%let Ids_newdata =;
*------------------------------------------------------------*;
%end;
%global Ids_source;
%if "&Ids_newdata" ne "" %then %do;
%let Ids_source = USERTABLE;
%end;
%else %do;
%let Ids_source = DATASOURCE;
%end;
%mend em_usedatatable;
%em_usedatatable;
*------------------------------------------------------------*;
* Create workspace data set;
*------------------------------------------------------------*;
data workspace;
length property $64 value $200;
property= 'PROJECTLOCATION';
value= "&EM_PROJECT";
output;
property= 'PROJECTNAME';
value= "&EM_PROJECTNAME";
output;
property= 'WORKSPACENAME';
value= "&EM_WSNAME";
output;
property= 'WORKSPACEDESCRIPTION';
value= "&EM_WSDESCRIPTION";
output;
property= 'SUMMARYDATASET';
value= "&EM_SUMMARY";
output;
property= 'NUMTASKS';
value= "&EM_NUMTASKS";
output;
property= 'EDITMODE';
value= "&EM_EDITMODE";
output;
property= 'DEBUG';
value= "&EM_DEBUGVAL";
output;
run;
*------------------------------------------------------------*;
* Create nodes data set;
*------------------------------------------------------------*;
data nodes;
length id $12 component $32 description $64 X 8 Y 8 diagramID $32 parentID $32;
id= "Ids";
component="DataSource";
description= "Transaction";
diagramID="_ROOT_";
parentID="";
X=101;
Y=86;
output;
id= "Assoc";
component="Association";
description= "Association";
diagramID="_ROOT_";
parentID="";
X=292;
Y=86;
output;
run;
*------------------------------------------------------------*;
* DataSource Properties;
*------------------------------------------------------------*;
data WORK.transaction_P;
  length   Property                         $ 32
           Value                            $ 200
           ;

Property="Name";
Value="TRANSACTION";
output;
Property="CreateDate";
Value="1719144556.8";
output;
Property="ModifyDate";
Value="1719144556.8";
output;
Property="CreatedBy";
Value="pathal";
output;
Property="ModifiedBy";
Value="pathal";
output;
Property="SampleSizeType";
Value="";
output;
Property="SampleSize";
Value="";
output;
;
run;
*------------------------------------------------------------*;
* Variable Attributes for Ids;
*------------------------------------------------------------*;
data WORK.Ids_VariableAttribute;
length Variable $64 AttributeName $32 AttributeValue $64;
Variable='date';
AttributeName="ROLE";
AttributeValue='REJECTED';
Output;
Variable='date';
AttributeName="COMMENT";
AttributeValue='Exceeds maximum number of levels cutoff';
Output;
Variable='procedure_code';
AttributeName="ROLE";
AttributeValue='TARGET';
Output;
Variable='procedure_code';
AttributeName="COMMENT";
AttributeValue='Exceeds maximum number of levels cutoff';
Output;
run;
*------------------------------------------------------------*;
* EMNOTES File for Ids;
*------------------------------------------------------------*;
data _null_;
if symget('sysscp')=:'WIN' then dsep='\';
else if symget('sysscp')=:'DNT' then dsep='\';
else dsep = '/';
filepath = pathname('work')!!dsep!!"Ids_EMNOTES.txt";
call symput('DSPATH', filepath);
run;
data _null_;
filename dspath "&dspath" encoding="utf-8" NOBOM;
file dspath;
run;
*------------------------------------------------------------*;
* RULESFILTER Data Set for Assoc;
*------------------------------------------------------------*;
*------------------------------------------------------------*;
* EMNOTES File for Assoc;
*------------------------------------------------------------*;
data _null_;
if symget('sysscp')=:'WIN' then dsep='\';
else if symget('sysscp')=:'DNT' then dsep='\';
else dsep = '/';
filepath = pathname('work')!!dsep!!"Assoc_EMNOTES.txt";
call symput('DSPATH', filepath);
run;
data _null_;
filename dspath "&dspath" encoding="utf-8" NOBOM;
file dspath;
run;
*------------------------------------------------------------*;
* Create node properties data set;
*------------------------------------------------------------*;
data nodeprops;
length id $12 property $64 value $400;
id= "Ids";
property="DataSource";
value= "transaction";
output;
id= "Ids";
property="Scope";
value= "LOCAL";
output;
id= "Ids";
property="Role";
value= "TRANSACTION";
output;
%let Ids_lib = %scan(&Ids_data, 1, .);
id= "Ids";
property="Library";
value= "&Ids_lib";
output;
%let Ids_member = %scan(&Ids_data, 2, .);
id= "Ids";
property="Table";
value= "&Ids_member";
output;
id= "Ids";
property="NCols";
value= "3";
output;
id= "Ids";
property="NObs";
value= "300000003";
output;
id= "Ids";
property="NBytes";
value= "7241597952";
output;
id= "Ids";
property="Segment";
value= "";
output;
id= "Ids";
property="DataSourceRole";
value= "TRANSACTION";
output;
id= "Ids";
property="OutputType";
value= "VIEW";
output;
id= "Ids";
property="ForceRun";
value= "N";
output;
id= "Ids";
property="ComputeStatistics";
value= "N";
output;
id= "Ids";
property="DataSelection";
value= "&Ids_source";
output;
id= "Ids";
property="NewTable";
value= "&Ids_newdata";
output;
id= "Ids";
property="MetaAdvisor";
value= "BASIC";
output;
id= "Ids";
property="ApplyIntervalLevelLowerLimit";
value= "Y";
output;
id= "Ids";
property="IntervalLowerLimit";
value= "20";
output;
id= "Ids";
property="ApplyMaxPercentMissing";
value= "Y";
output;
id= "Ids";
property="MaxPercentMissing";
value= "50";
output;
id= "Ids";
property="ApplyMaxClassLevels";
value= "Y";
output;
id= "Ids";
property="MaxClassLevels";
value= "20";
output;
id= "Ids";
property="IdentifyEmptyColumns";
value= "Y";
output;
id= "Ids";
property="VariableValidation";
value= "STRICT";
output;
id= "Ids";
property="NewVariableRole";
value= "REJECT";
output;
id= "Ids";
property="DropMapVariables";
value= "Y";
output;
id= "Ids";
property="DsId";
value= "transaction";
output;
id= "Ids";
property="DsSampleName";
value= "";
output;
id= "Ids";
property="DsSampleSizeType";
value= "";
output;
id= "Ids";
property="DsSampleSize";
value= "";
output;
id= "Ids";
property="DsCreatedBy";
value= "pathal";
output;
id= "Ids";
property="DsCreateDate";
value= "1719144556.8";
output;
id= "Ids";
property="DsModifiedBy";
value= "pathal";
output;
id= "Ids";
property="DsModifyDate";
value= "1719144556.8";
output;
id= "Ids";
property="DsScope";
value= "LOCAL";
output;
id= "Ids";
property="Sample";
value= "D";
output;
id= "Ids";
property="SampleSizeType";
value= "PERCENT";
output;
id= "Ids";
property="SampleSizePercent";
value= "20";
output;
id= "Ids";
property="SampleSizeObs";
value= "10000";
output;
id= "Ids";
property="DBPassThrough";
value= "Y";
output;
id= "Ids";
property="RunAction";
value= "Train";
output;
id= "Ids";
property="Component";
value= "DataSource";
output;
id= "Ids";
property="Description";
value= "";
output;
id= "Ids";
property="EM_VARIABLEATTRIBUTES";
value= "WORK.Ids_VariableAttribute";
output;
id= "Ids";
property="EM_FILE_EMNOTES";
value= "Ids_EMNOTES.txt";
output;
id= "Assoc";
property="ItemCount";
value= "3";
output;
id= "Assoc";
property="AssocSupportType";
value= "COUNT";
output;
id= "Assoc";
property="SeqSupportType";
value= "PERCENT";
output;
id= "Assoc";
property="SupportA";
value= "500";
output;
id= "Assoc";
property="MinConf";
value= "1";
output;
id= "Assoc";
property="PctsupA";
value= "5";
output;
id= "Assoc";
property="PctsupS";
value= "2";
output;
id= "Assoc";
property="SupportS";
value= ".";
output;
id= "Assoc";
property="ChainCount";
value= "3";
output;
id= "Assoc";
property="ConsolTime";
value= "0";
output;
id= "Assoc";
property="MaxDur";
value= ".";
output;
id= "Assoc";
property="Criterion";
value= "DEFAULT";
output;
id= "Assoc";
property="NumRules";
value= "500";
output;
id= "Assoc";
property="NumTransposeRules";
value= "200";
output;
id= "Assoc";
property="MaxItems";
value= "100000";
output;
id= "Assoc";
property="ExportIdRule";
value= "N";
output;
id= "Assoc";
property="Recommendation";
value= "N";
output;
id= "Assoc";
property="ForceRun";
value= "N";
output;
id= "Assoc";
property="RunAction";
value= "Train";
output;
id= "Assoc";
property="Component";
value= "Association";
output;
id= "Assoc";
property="EM_FILE_EMNOTES";
value= "Assoc_EMNOTES.txt";
output;
run;
*------------------------------------------------------------*;
* Create connections data set;
*------------------------------------------------------------*;
data connect;
length from to $12;
from="Ids";
to="Assoc";
output;
run;
*------------------------------------------------------------*;
* Create actions to run data set;
*------------------------------------------------------------*;
%macro emaction;
%let actionstring = %upcase(&EM_ACTION);
%if %index(&actionstring, RUN) or %index(&actionstring, REPORT) %then %do;
data actions;
length id $12 action $40;
id="Assoc";
%if %index(&actionstring, RUN) %then %do;
action='run';
output;
%end;
%if %index(&actionstring, REPORT) %then %do;
action='report';
output;
%end;
run;
%end;
%mend;
%emaction;
*------------------------------------------------------------*;
* Execute the actions;
*------------------------------------------------------------*;
%em5batch(execute, workspace=workspace, nodes=nodes, connect=connect, datasources=datasources, nodeprops=nodeprops, action=actions);