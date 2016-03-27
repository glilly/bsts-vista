C0TSFM ; GPL - Utilities for C0TS; 7/4/15 6:03pm
 ;;1.0;C0TS VISTA TERMINOLOGY SERVER;;Mar 21, 2016;Build 1
 ;
 Q
 ;
FMX(RTN,PTR,FILE,IEN) ; return an array of a fileman record for external use in RTN,
 ; which is passed by name. Input is either a fileman pointer ie 192;GMRD(120.82,
 ; or the file number and the ien. If PTR is passed, FILE and IEN are ignored.
 K @RTN
 I $D(PTR) D  ;
 . S FILE=$P($P(PTR,"(",2),",",1)
 . S IEN=$P(PTR,";",1)
 N TREC,FILENM
 D GETS^DIQ(FILE,IEN_",","**","ENR","TREC")
 S FILENM=$O(^DD(FILE,0,"NM",""))
 S FILENM=$TR(FILENM," ","_")
 ;ZWR TREC
 I $G(DEBUG)=1 B
 N % S %=$Q(TREC(""))
 F  D  Q:%=""  ;
 . N FNUM,FNAME,IENS,FIELD,VAL
 . S FNUM=$QS(%,1)
 . S FNAME=$O(^DD(FNUM,0,"NM",""))
 . S FNAME=$TR(FNAME," ","_")
 . S IENS=$QS(%,2)
 . S FIELD=$QS(%,3)
 . S FIELD=$TR(FIELD," ","_")
 . S VAL=@%
 . I FNUM=FILE D  ; not a subfile
 . . S @RTN@(FNAME,FIELD)=VAL
 . . S @RTN@(FNAME,"ien")=$P(IENS,",",1)
 . E  D  ;
 . . N I2 S I2=$O(@RTN@(FNAME,""),-1)+1
 . . S @RTN@(FNAME,I2,FIELD)=VAL
 . . ;S @RTN@(FNAME,I2,"iens")=IENS
 . W:$G(DEBUG)=1 !,%,"=",@%
 . S %=$Q(@%)
 Q
 ;EXAMPLE
 ;G("BSTS_CONCEPT","CODESET")=36
 ;G("BSTS_CONCEPT","CONCEPT_ID")=370206005
 ;G("BSTS_CONCEPT","COUNTER")=75
 ;G("BSTS_CONCEPT","DTS_ID")=370206
 ;G("BSTS_CONCEPT","FULLY_SPECIFIED_NAME")="Asthma limits walking on the flat (finding)"
 ;G("BSTS_CONCEPT","LAST_MODIFIED")="MAY 11, 2015"
 ;G("BSTS_CONCEPT","OUT_OF_DATE")="NO"
 ;G("BSTS_CONCEPT","PARTIAL_ENTRY")="NON-PATIAL (FULL ENTRY)"
 ;G("BSTS_CONCEPT","REVISION_IN")="MAR 01, 2012"
 ;G("BSTS_CONCEPT","REVISION_OUT")="JAN 01, 2050"
 ;G("BSTS_CONCEPT","VERSION")=20140901
 ;G("BSTS_CONCEPT","ien")="75"
 ;G("IS_A_RELATIONSHIP",1,"IS_A_RELATIONSHIP")=2
 ;G("SUBSETS",1,"SUBSETS")="EHR IPL ASTHMA DXS"
 ;G("SUBSETS",2,"SUBSETS")="SRCH Cardiology"
 ;G("SUBSETS",3,"SUBSETS")="IHS Problem List"
 ;
CONARY(ARY,CONID) ; returns a concept array for concept code CONID  in ARY, passed by name
 ;
 N CFN,CIEN,FILENM
 S CFN=9002318.4 ; concept file number
 S CIEN=$O(^BSTS(CFN,"CODE",CONID,""))
 D FMX(ARY,,CFN,CIEN)
 S FILENM=$O(^DD(CFN,0,"NM",""))
 S FILENM=$TR(FILENM," ","_")
 I $G(@ARY@(FILENM,"CONCEPT_ID"))'="" S @ARY@(FILENM,"uri")="concept?id="_CONID
 ;
 Q
 ;
TERMARY(ARY,CODE) ; returns a concept array for concept code CONID  in ARY, passed by name
 ;
 N TFN,TIEN,FILENM
 S TFN=9002318.3 ; term file number
 S TIEN=$O(^BSTS(TFN,"CODE",CODE,""))
 D FMX(ARY,,TFN,TIEN)
 S FILENM=$O(^DD(TFN,0,"NM",""))
 S FILENM=$TR(FILENM," ","_")
 I $G(@ARY@(FILENM,"DESCRIPTION_ID"))'="" S @ARY@(FILENM,"uri")="code?id="_CODE
 ;
 Q
 ;