C0TSWD ;GPL-VistA Terminology Server Web Services ; 21 Mar 2016  10:23 PM
 ;;1.0;C0TS VISTA TERMINOLOGY SERVER;;Mar 21, 2016;Build 1
 ;
 ; License Apache 2.0
 ;
 Q
 ;
GETSETS(RSETS,RAW) ; returns the array of codesets
 N OK,TMPSETS
 S OK=$$CODESETS^BSTSAPIA("TMPSETS","1^1")
 I +OK'=1 D  Q 0
 . W !,"ERROR GETTING CODESETS"
 I $G(RAW)=1 M @RSETS=TMPSETS Q 1
 N SID,SNAME,STEXT,REC
 N II  S II=0
 F  S II=$O(TMPSETS(II)) Q:+II=0  D  ;
 . S REC=TMPSETS(II)
 . S SID=$P(REC,"^",1)
 . S SNAME=$P(REC,"^",2)
 . S STEXT=$P(REC,"^",3)
 . N VERSION,OK
 . S OK=$$VERSIONS^BSTSAPIA("VERSION",SID_"^1^1")
 . S @RSETS@(II,"id")=SID
 . S @RSETS@(II,"url")="codelist?id="_SID
 . S @RSETS@(II,"name")=SNAME
 . S @RSETS@(II,"text")=STEXT
 . I $D(VERSION) M @RSETS@(II,"versions")=VERSION
 . S @RSETS@("ID",SID,II)=""
 . S @RSETS@("NAME",SNAME,II)=""
 . N SUBSET,SOK
 . S SOK=$$SUBSET^BSTSAPIA("SUBSET",SID_"^1^1")
 . I $D(SUBSET) D  ; subsets exist
 . . N JJ S JJ=0
 . . F  S JJ=$O(SUBSET(JJ)) Q:+JJ=0  D  ;
 . . . N NM
 . . . S NM=SUBSET(JJ)
 . . . S NM=$TR(NM," ","_")
 . . . S @RSETS@(II,"subset",JJ,"name")=NM
 . . . S @RSETS@(II,"subset",JJ,"url")="subset?subset="_NM
 . ;I $D(SUBSET) M @RSETS@(II,"subset")=SUBSET
 Q 1
 ;
GETLIST(RTN,ID) ; get a code list array for codeset ID
 ;
 N CFN,TFN,CSFN
 S TFN=9002318.3 ; term file number
 S CFN=9002318.4 ; concept file number
 S CSFN=9002318.1 ; codeset file number
 N NCON S NCON=$NA(^BSTS(CFN,"C")) ; BSTS CONCEPT
 N NTERM S NTERM=$NA(^BSTS(TFN,"C")) ; BSTS TERMONOLGY
 N SETCODE,SETIEN
 S SETIEN=$O(^BSTS(CSFN,"B",ID,""))
 S SETCODE=$$GET1^DIQ(CSFN,SETIEN_",",.02)
 ;
 N ZN S ZN=0 ; line count
 N I S I=0
 F  S I=$O(@NTERM@(ID,I)) Q:+I=0  D  ; for each item in the C index of the Term file
 . N TIEN,CIEN
 . S CIEN=I ; concept IEN
 . S TIEN=""
 . F  S TIEN=$O(@NTERM@(ID,CIEN,TIEN)) Q:TIEN=""  D  ; term ien
 . . S ZN=ZN+1
 . . N TERM,CODE
 . . S TERM=$$GET1^DIQ(TFN,TIEN_",",1)
 . . S CODE=$$GET1^DIQ(TFN,TIEN_",",.02)
 . . W:$G(DEBUG)=1 !,SETCODE," ",CODE," ",TERM
 . . N CONID,CONCEPT
 . . S CONID=$$GET1^DIQ(CFN,CIEN_",",.02)
 . . S CONCEPT=$$GET1^DIQ(CFN,CIEN_",",1)
 . . W:$G(DEBUG)=1 " ",CONID," ",CONCEPT
 . . S @RTN@(ZN,"setcode")=SETCODE
 . . S @RTN@(ZN,"setid")=ID
 . . S @RTN@(ZN,"term")=TERM
 . . S @RTN@(ZN,"code")=CODE
 . . S @RTN@(ZN,"conceptid")=CONID
 . . S @RTN@(ZN,"concept")=CONCEPT
 . ;B
 Q
 ;
CLEAR ; clear the cache
 K ^XTMP("C0TSBUF")
 Q
 ;
QTYSET(ID) ; extrinsic returns the number of items in codeset ID
 N GN S GN=$NA(^XTMP("C0TSBUF"))
 N RSLT
 S RSLT=$O(@GN@(ID,""),-1)
 I +RSLT>0 Q RSLT
 S CFN=9002318.4 ; concept file number 
 N NCON S NCON=$NA(^BSTS(CFN,"C")) ; BSTS CONCEPT
 S RSLT=0
 N % S %=""
 F  S %=$O(@NCON@(ID,%)) Q:%=""  S RSLT=RSLT+1 ; count em
 Q RSLT
 ;
LISTBUF(RTN,ID) ; returns a cached buffers of the list of codeset ID
 ;
 N GN S GN=$NA(^XTMP("C0TSBUF"))
 I '$D(@GN@(0)) D  ; work area doesn't exist
 . N X,Y
 . S X="T+999" ; a long time from now
 . D ^%DT ; covert to FM date format
 . S @GN@(0)=Y_"^"_$$NOW^XLFDT_"^C0TS Code List Buffer Area"
 ;
 S RTN=$NA(@GN@(ID))
 I '$D(@RTN) D GETLIST(RTN,ID)
 I $$QTYSET(ID)=0 S RTN=""
 Q
 ;
ADDSUB(ARY,NAME,SETID) ; add a subset list to the cache - extrinsic returns the location
 ; ARY is passed by name
 I '$D(SETID) S SETID=36
 N GN S GN=$NA(^XTMP("C0TSBUF",SETID,"SUBSET"))
 N WHERE
 S WHERE=$O(@GN@(" "),-1)+1
 M @GN@(WHERE)=@ARY
 S @GN@("B",NAME,WHERE)=""
 Q $NA(@GN@(WHERE))
 ;
SUBBUF(NAME,SETID) ; extrinsic returns the location of subset NAME
 ;
 I '$D(SETID) S SETID=36
 N GN S GN=$NA(^XTMP("C0TSBUF",SETID,"SUBSET"))
 N ZR
 S ZR=$O(@GN@("B",NAME,""))
 I ZR'="" Q $NA(@GN@(ZR))
 N SUBARY
 D GETSUB("SUBARY",NAME,SETID)
 Q $$ADDSUB("SUBARY",NAME,SETID)
 ;
GETSUB(RTN,NAME,SETID) ; return a subset list in RTN, passed by name
 ; SETID IS THE REFERENCED CODESET (DEFAULT 36 - SNOMEDCT)
 K @RTN
 I '$D(SETID) S SETID=36
 N OK,TMPARY
 S OK=$$SUBLST^BSTSAPIC("TMPARY",NAME_"^"_SETID_"^1")
 I 'OK D
 . D ^ZTER
 . W !,"ERROR GETTING SUBSET ",NAME
 ;B
 N % S %=0
 F  S %=$O(TMPARY(%)) Q:+%=0  D  ;
 . N REC,CONID
 . S REC=TMPARY(%)
 . S CONID=$P(REC,"^",1)
 . S @RTN@(%,"conceptid")=CONID
 . S @RTN@(%,"concept")=$$CONCEPT(CONID)
 . S @RTN@(%,"code")=$P(REC,"^",2)
 . S @RTN@(%,"term")=$P(REC,"^",3)
 ;M @RTN=TMPARY
 Q
 ;
CONCEPT(CID) ; extrinsic returns the Concept Fully Specified Name given the Concept ID
 ;
 N CFN,CIEN
 S CFN=9002318.4 ; concept file number
 S CIEN=$O(^BSTS(CFN,"CODE",CID,""))
 Q $$GET1^DIQ(CFN,CIEN_",",1)
 ;