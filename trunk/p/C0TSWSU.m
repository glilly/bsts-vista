C0TSWSU ; GPL - Utilities for C0TS; 7/4/15 6:03pm
 ;;1.0;C0TS VISTA TERMINOLOGY SERVER;;Mar 21, 2016;Build 1
 ;
 Q
 ;
STARTTM(ARY) ; timestamp the start time
 S @ARY@("startTimeFM")=$$TIMEI
 S @ARY@("startTime")=$$TIMEE(@ARY@("startTimeFM"))
 Q
 ;
ENDTM(ARY) ; timestamp the start time
 S @ARY@("endTimeFM")=$$TIMEI
 S @ARY@("endTime")=$$TIMEE(@ARY@("endTimeFM"))
 S @ARY@("elapsedTime")=$$ELAPSED(@ARY@("endTimeFM"),@ARY@("startTimeFM")) ;
 ; in seconds
 Q
 ;
ELAPSED(END,START) ; elapsed time in seconds. end and start are FM format
 Q $$FMDIFF^XLFDT(END,START,2)
TIMEI() ; internal time
 Q $$NOW^XLFDT
 ;
TIMEE(FMTIME) ; external time
 Q $$FMTE^XLFDT(FMTIME)
 ;
ADDAT(OUTARY,INARY,TAG) ; both passed by name..
 ;  INARY("ATTR")="XX" is converted to OUTARY("TAG@ATTR")="XX"
 ;   to make better xml - only works with simple arrays
 I '$D(TAG) S TAG="item"
 N ZI S ZI=""
 F  S ZI=$O(@INARY@(ZI)) Q:ZI=""  D  ;
 . S @OUTARY@(TAG_"@"_ZI)=@INARY@(ZI)
 Q
 ;
TESTADD ; test of ADDAT routine
 N GN S GN=$NA(^XTMP("EHEXPAT",1))
 N GPL
 D ADDAT("GPL",GN,"patient")
 ZWR GPL
 Q
 ;
GENHTML2(HOUT,HARY) ; generate an HTML table from array HARY
 ; HOUT AND HARY are passed by name
 ;
 ;  HARY("TITLE")="Problem List"
 ;  HARY("HEADER",1)="column 1 header"
 ;  HARY("HEADER",2)="col 2 header"
 ;  HARY(1,1)="row 1 col1 value"
 ;  HARY(1,2)="row 1 col2 value"
 ;  HARY(1,2,"ID")="the ID of the element" 
 ;  etc...
 ;
 N C0I,C0J
 D ADDTO(HOUT,"<div align=""center"">")
 ;I $D(@HARY@("TITLE")) D  ;
 ;. N X
 ;. S X="<title>"_@HARY@("TITLE")_"</title>"
 ;. D ADDTO(HOUT,X)
 D ADDTO(HOUT,"<text>")
 D ADDTO(HOUT,"<table border=""1"" style=""width:80%"">")
 I $D(@HARY@("TITLE")) D  ;
 . N X
 . S X="<caption><b>"_@HARY@("TITLE")_"</b></caption>"
 . D ADDTO(HOUT,X)
 I $D(@HARY@("HEADER")) D  ;
 . D ADDTO(HOUT,"<thead>")
 . D ADDTO(HOUT,"<tr>")
 . S C0I=0
 . F  S C0I=$O(@HARY@("HEADER",C0I)) Q:+C0I=0  D  ;
 . . D ADDTO(HOUT,"<th>"_@HARY@("HEADER",C0I)_"</th>")
 . D ADDTO(HOUT,"</tr>")
 . D ADDTO(HOUT,"</thead>")
 D ADDTO(HOUT,"<tbody>")
 I $D(@HARY@(1)) D  ;
 . S C0I=0 S C0J=0
 . F  S C0I=$O(@HARY@(C0I)) Q:+C0I=0  D  ;
 . . D ADDTO(HOUT,"<tr>")
 . . F  S C0J=$O(@HARY@(C0I,C0J)) Q:+C0J=0  D  ;
 . . . N UID S UID=$G(@HARY@(C0I,C0J,"ID"))
 . . . I UID'="" D ADDTO(HOUT,"<td style=""padding:5px;"" ID="""_UID_""">"_@HARY@(C0I,C0J)_"</td>")
 . . . E  D ADDTO(HOUT,"<td style=""padding:5px;"">"_@HARY@(C0I,C0J)_"</td>")
 . . D ADDTO(HOUT,"</tr>")
 D ADDTO(HOUT,"</tbody>")
 D ADDTO(HOUT,"</table>")
 D ADDTO(HOUT,"</text>")
 D ADDTO(HOUT,"</div>")
 Q
 ;
GENHTML(HOUT,HARY) ; generate an HTML table from array HARY
 ; HOUT AND HARY are passed by name
 ;
 ;  HARY("TITLE")="Problem List"
 ;  HARY("HEADER",1)="column 1 header"
 ;  HARY("HEADER",2)="col 2 header"
 ;  HARY(1,1)="row 1 col1 value"
 ;  HARY(1,2)="row 1 col2 value"
 ;  HARY(1,2,"ID")="the ID of the element" 
 ;  etc...
 ;
 N DIVCLASS,TBLCLASS
 S DIVCLASS=$G(@HARY@("DIVCLASS"))
 S TBLCLASS=$G(@HARY@("TABLECLASS"))
 I DIVCLASS="" S DIVCLASS="tables"
 I TBLCLASS="" S TBLCLASS="patient"
 N C0I,C0J
 D ADDTO(HOUT,"<div class=""tables"">")
 ;I $D(@HARY@("TITLE")) D  ;
 ;. N X
 ;. S X="<title>"_@HARY@("TITLE")_"</title>"
 ;. D ADDTO(HOUT,X)
 ;D ADDTO(HOUT,"<text>")
 N ZWIDTH S ZWIDTH=$G(@HARY@("WIDTH"))
 I ZWIDTH="" S ZWIDTH="80%"
 D ADDTO(HOUT,"<table class=""summary"" style=""width:"_ZWIDTH_""">")
 I $D(@HARY@("TITLE")) D  ;
 . N X
 . S X="<caption>"_@HARY@("TITLE")_"</caption>"
 . D ADDTO(HOUT,X)
 I $D(@HARY@("HEADER")) D  ;
 . D ADDTO(HOUT,"<thead>")
 . D ADDTO(HOUT,"<tr>")
 . N NUMCOL S NUMCOL=$O(@HARY@("HEADER",""),-1)
 . S C0I=0
 . F  S C0I=$O(@HARY@("HEADER",C0I)) Q:+C0I=0  D  ;
 . . ;N TH S TH="<th colspan="""_NUMCOL_""">"_@HARY@("HEADER",C0I)_"</th>"
 . . N TH S TH="<th>"_@HARY@("HEADER",C0I)_"</th>"
 . . D ADDTO(HOUT,TH)
 . D ADDTO(HOUT,"</tr>")
 . D ADDTO(HOUT,"</thead>")
 D ADDTO(HOUT,"<tbody>")
 I $D(@HARY@(1)) D  ;
 . S C0I=0 S C0J=0
 . F  S C0I=$O(@HARY@(C0I)) Q:+C0I=0  D  ;
 . . D ADDTO(HOUT,"<tr>")
 . . F  S C0J=$O(@HARY@(C0I,C0J)) Q:+C0J=0  D  ;
 . . . N UID S UID=$G(@HARY@(C0I,C0J,"ID"))
 . . . I UID'="" D ADDTO(HOUT,"<td ID="""_UID_""">"_@HARY@(C0I,C0J)_"</td>")
 . . . E  D ADDTO(HOUT,"<td>"_@HARY@(C0I,C0J)_"</td>")
 . . D ADDTO(HOUT,"</tr>")
 D ADDTO(HOUT,"</tbody>")
 D ADDTO(HOUT,"</table>")
 ;D ADDTO(HOUT,"</text>")
 D ADDTO(HOUT,"</div>")
 Q
 ;
GENVHTML(HOUT,HARY) ; generate a vertical HTML table from array HARY
 ; headers are in the first row
 ; HOUT AND HARY are passed by name
 ;
 ; format of the table:
 ;  HARY("TITLE")="Problem List"
 ;  HARY("HEADER",1)="row 1 column 1 header"
 ;  HARY("HEADER",2)="row 2 col 2 header"
 ;  HARY(1,1)="row 1 col2 value"
 ;  HARY(2,1)="row 2 col2 value"
 ;  etc...
 ;
 N DIVCLASS,TBLCLASS
 S DIVCLASS=$G(@HARY@("DIVCLASS"))
 S TBLCLASS=$G(@HARY@("TABLECLASS"))
 I DIVCLASS="" S DIVCLASS="tables"
 I TBLCLASS="" S TBLCLASS="patient"
 N C0I,C0J
 D ADDTO(HOUT,"<div class=""tables"">")
 ;D ADDTO(HOUT,"<div align=""center"">")
 N ZWIDTH S ZWIDTH=$G(@HARY@("WIDTH"))
 I ZWIDTH="" S ZWIDTH="80%"
 D ADDTO(HOUT,"<table class=""summary"" style=""width:"_ZWIDTH_""">")
 ;D ADDTO(HOUT,"<text>")
 ;D ADDTO(HOUT,"<table border=""1"" style=""width:40%"">")
 I $D(@HARY@("TITLE")) D  ;
 . N X
 . S X="<caption><b>"_@HARY@("TITLE")_"</b></caption>"
 . D ADDTO(HOUT,X)
 I $D(@HARY@("HEADER")) D  ;
 . D ADDTO(HOUT,"<thead>")
 . D ADDTO(HOUT,"<tr>")
 . N NUMCOL S NUMCOL=$O(@HARY@("HEADER",""),-1)
 . S C0I=0
 . F  S C0I=$O(@HARY@("HEADER",C0I)) Q:+C0I=0  D  ;
 . . D ADDTO(HOUT,"<th style=""padding:5px;"">"_@HARY@("HEADER",C0I)_"</th>")
 . . D ADDTO(HOUT,"<td style=""padding:5px;"">"_@HARY@(C0I,1)_"</td>")
 . D ADDTO(HOUT,"</tr>")
 D ADDTO(HOUT,"</table>")
 D ADDTO(HOUT,"</text>")
 D ADDTO(HOUT,"</div>")
 Q
 ;
TSTYLE1 ; table style template
 ;;<style>
 ;;table, th, td
 ;;{
 ;;border-collapse:collapse;
 ;;border:1px solid black;
 ;;}
 ;;th, td
 ;;{
 ;;padding:5px;
 ;;}
 ;;</style>
 Q
 ;
TESTHTML ;
 N HTML
 S HTML("TITLE")="Problem List"
 S HTML("HEADER",1)="column 1 header"
 S HTML("HEADER",2)="col 2 header"
 S HTML(1,1)="row 1 col1 value"
 S HTML(1,2)="row 1 col2 value"
 N GHTML
 D GENHTML("GHTML","HTML")
 ZWR GHTML
 Q
 ;
ADDTO(DEST,WHAT) ; adds string WHAT to list DEST 
 ; DEST is passed by name
 N GN
 S GN=$O(@DEST@("AAAAAA"),-1)+1
 S @DEST@(GN)=WHAT
 S @DEST@(0)=GN ; count
 Q
 ;
ADDARY(DEST,WHAT) ; adds array WHAT to list DEST 
 ; DEST and WHAT are passed by name
 N GN
 S GN=$O(@DEST@("AAAAAA"),-1)+1
 N ZZI S ZZI=0
 F  S ZZI=$O(@WHAT@(ZZI)) Q:'ZZI  D  ;
 . S @DEST@(GN)=$G(@WHAT@(ZZI))
 . S @DEST@(0)=GN ; count
 . S GN=GN+1
 Q
 ;
ORGOID() ; extrinsic which returns the Organization OID
 Q "2.16.840.1.113883.5.83" ; WORLDVISTA HL7 OID - 
 ; REPLACE WITH OID LOOKUP FROM INSTITUTION FILE
 ;
tree(where,prefix,docid,zout) ; show a tree starting at a node in MXML. 
 ; node is passed by name
 ; 
 i $g(prefix)="" s prefix="|--" ; starting prefix
 i '$d(KBAIJOB) s KBAIJOB=$J
 n node s node=$na(^TMP("MXMLDOM",KBAIJOB,docid,where))
 n txt s txt=$$CLEAN($$ALLTXT(node))
 w:'$G(DIQUIET) !,prefix_@node_" "_txt
 d oneout(zout,prefix_@node_" "_txt)
 n zi s zi=""
 f  s zi=$o(@node@("A",zi)) q:zi=""  d  ;
 . w:'$G(DIQUIET) !,prefix_"  : "_zi_"^"_$g(@node@("A",zi))
 . d oneout(zout,prefix_"  : "_zi_"^"_$g(@node@("A",zi)))
 f  s zi=$o(@node@("C",zi)) q:zi=""  d  ;
 . d tree(zi,"|  "_prefix,docid,zout)
 q
 ;
oneout(zbuf,ztxt) ; adds a line to zbuf
 n zi s zi=$o(@zbuf@(""),-1)+1
 s @zbuf@(zi)=ztxt
 q
 ;
ALLTXT(where) ; extrinsic which returns all text lines from the node .. concatinated 
 ; together
 n zti s zti=""
 n ztr s ztr=""
 f  s zti=$o(@where@("T",zti)) q:zti=""  d  ;
 . s ztr=ztr_$g(@where@("T",zti))
 q ztr
 ;
CLEAN(STR) ; extrinsic function; returns string - gpl borrowed from the CCR package
 ;; Removes all non printable characters from a string.
 ;; STR by Value
 N TR,I
 F I=0:1:31 S TR=$G(TR)_$C(I)
 S TR=TR_$C(127)
 N ZR S ZR=$TR(STR,TR)
 S ZR=$$LDBLNKS(ZR) ; get rid of leading blanks
 QUIT ZR
 ;
LDBLNKS(st) ; extrinsic which removes leading blanks from a string
 n pos f pos=1:1:$l(st)  q:$e(st,pos)'=" "
 q $e(st,pos,$l(st))
 ;
show(what,docid,zout) ;
 I '$D(C0XJOB) S C0XJOB=$J
 d tree(what,,docid,zout)
 q
 ; 
listm(out,in) ; out is passed by name in is passed by reference
 n i s i=$q(@in@(""))
 f  s i=$q(@i) q:i=""  d oneout(out,i_"="_@i)
 q
 ;
peel(out,in) ; compress a complex global into something simpler
 n i s i=$q(@in@(""))
 f  s i=$q(@i) q:i=""  d  ;
 . n j,k,l,m,n,m1
 . s (l,m)=""
 . s n=$$shrink($qs(i,$ql(i)))
 . s k=$qs(i,0)_"("""
 . f j=1:1:$ql(i)-1  d  ;
 . . i +$qs(i,j)>0 d  ;
 . . . i m'="" q
 . . . s m=$qs(i,j)
 . . . s m1=j
 . . . i j>1 s l=$qs(i,j-1)
 . . . e  s l=$qs(i,j)
 . . . i l["substanceAdministration" s l=$p(l,"substanceAdministration",2)
 . . s k=k_$qs(i,j)_""","""
 . . w:$g(DEBUG) !,j," ",k
 . s k=k_$qs(i,$ql(i))_""")"
 . w:$g(DEBUG) !,k,"=",@k
 . i l'="" d  q  ;
 . . d:$g(@out@(l,m,n))'=""
 . . . ;n jj,n2
 . . . ;f jj=2:1  w !,jj s n2=$qs(i,$ql(i)-1)_"["_jj_"]"_n q:$g(@out@(l,m,n2))=""  w !,n2
 . . . ;s n=n2
 . . . ;s n=$$shrink($qs(i,$ql(i)-1))_"_"_n
 . . . s n=$$mkxpath(i,m1)
 . . . b:$g(@out@(l,m,n))'=""
 . . s @out@(l,m,n)=@k
 . i @k'="" d  ;
 . . i $ql(i)>1 d  q  ;
 . . . s l=$$shrink($qs(i,$ql(i)-1))
 . . . d:$g(@out@(l,n))'=""
 . . . . ;n jj,n2
 . . . . ;f jj=2:1  s n2=$qs(i,$ql(i)-1)_"["_jj_"]"_"_"_n q:$g(@out@(l,n2))=""
 . . . . ;s n=n2
 . . . . ;b:$g(@out@(l,n))'=""
 . . . . s n=$$shrink($qs(i,$ql(i)-1))_"_"_n
 . . . s @out@(l,n)=@k
 . . s @out@(n)=@k
 q
 ;
shrink(x) ; reduce strings 
 n y,z
 s y=x
 s z="substanceAdministration"
 i x[z s y=$p(x,z,2)
 q y
 ;
mkxpath(zq,zm) ; extrinsic which returns the xpath derived from the $query value 
 ;passed by value. zm is the index to begin with
 ;
 n zr s zr=""
 n zi s zi=""
 f zi=1:1:$ql(zq) s zr=zr_"/"_$qs(zq,zi)
 q zr
 ;
ARY2XML(OUTXML,INARY,STK,CHILD) ; convert an array to xml
 I '$D(@OUTXML@(1)) S @OUTXML@(1)="<?xml version=""1.0"" encoding=""utf-8"" ?>"
 N II S II=""
 N DATTR S DATTR="" ; deffered attributes
 F  S II=$O(@INARY@(II),-1) Q:II=""  D  ;
 . N ATTR,TAG
 . S ATTR="" S TAG=""
 . I II["@" D  ;
 . . I TAG="" S TAG=$P(II,"@",1) S ATTR=$P(II,"@",2)_"="""_@INARY@(II)_""""
 . . W:$G(DEBUG) !,"TAG="_TAG_" ATTR="_ATTR
 . . ;I $O(@INARY@(II))["@" D  ;
 . . ;F  S II=$O(@INARY@(II),-1) Q:II=""  Q:$O(@INARY@(II),-1)'[(TAG_"@")  D  ;
 . . F  S II=$O(@INARY@(II),-1) Q:II=""  Q:II'[(TAG_"@")  D  ;
 . . . S ATTR=ATTR_" "_$P(II,"@",2)_"="""_@INARY@(II)_""""
 . . . W:$G(DEBUG) !,"ATTR= ",ATTR
 . . . W:$G(DEBUG) !,"II= ",II
 . . S II=$O(@INARY@(II)) ; reset to previous
 . . N ENDING S ENDING="/"
 . . I II["@" D  ;
 . . . I $O(@INARY@(II),-1)=TAG S DATTR=" "_ATTR Q  ; deffered attributes
 . . . I $D(@INARY@(TAG)) S ENDING=""
 . . . D ONEOUT(OUTXML,"<"_TAG_" "_ATTR_ENDING_">")
 . . . I ENDING="" D PUSH("STK","</"_TAG_">")
 . I II'["@" D  ;
 . . I +II=0 D  ;
 . . . D ONEOUT(OUTXML,"<"_II_DATTR_">")
 . . . S DATTR="" ; reinitialize after use
 . . . D PUSH("STK","</"_II_">")
 . I $D(@INARY@(II)) D ARY2XML(OUTXML,$NA(@INARY@(II)))
 I $D(STK) F  D ONEOUT(OUTXML,$$POP("STK")) Q:'$D(STK)
 Q
 ;
ONEOUT(ZBUF,ZTXT) ; ADDS A LINE TO ZBUF
 N ZI S ZI=$O(@ZBUF@(""),-1)+1
 S @ZBUF@(ZI)=ZTXT
 Q
 ;
PUSH(BUF,STR) ;
 D ONEOUT(BUF,STR)
 Q
 ;
POP(BUF) ; extrinsic returns the last element and then deletes it
 N NM,TX
 S NM=$O(@BUF@(""),-1)
 Q:NM="" NM
 S TX=@BUF@(NM)
 K @BUF@(NM)
 Q TX
 ;
