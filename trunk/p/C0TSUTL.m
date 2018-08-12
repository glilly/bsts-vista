C0TSUTL ; GPL - Utilities for C0TS; 7/4/15 6:03pm
 ;;1.0;C0TS VISTA TERMINOLOGY SERVER;;Mar 21, 2016;Build 1
 ;
 Q
 ;
SCT2ICD9(CDE) ; extrinsic which returns the ICD9 code mapped to the 
 ; snomed code CDE
 I $G(CDE)="" Q ""
 N FN,RTN,IEN,REC
 S FN=9002318.4
 S IEN=$O(^BSTS(FN,"CODE",CDE,""))
 Q:IEN="" ""
 D FMX^C0TSFM("REC",,FN,IEN) ; pull the fileman record for the snomed code
 S RTN=$G(REC("ICD9_TO_SNOMED_MAP",1,"ICD9_TO_SNOMED_MAP"))
 Q RTN
 ;
SCT2ICD10(CDE) ; extrinsic which returns the ICD10 code mapped to the 
 ; snomed code CDE
 I $G(CDE)="" Q ""
 N FN,RTN,IEN,REC
 S RTN=""
 S FN=9002318.4
 S IEN=$O(^BSTS(FN,"CODE",CDE,""))
 Q:IEN="" ""
 D FMX^C0TSFM("REC",,FN,IEN) ; pull the fileman record for the snomed code
 N DONE S DONE=0
 N ZI S ZI=0
 F  S ZI=$O(REC("ICD_MAPPING",ZI)) Q:+ZI=0  Q:DONE  D  ;
 . I REC("ICD_MAPPING",ZI,"CODE_TYPE")["ICD10" D  ;
 . . S RTN=$G(REC("ICD_MAPPING",ZI,"CODE"))
 . . S DONE=1
 Q RTN
 ;
