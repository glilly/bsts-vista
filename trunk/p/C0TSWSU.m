C0TSWSU ; GPL - Utilities for C0TS; 7/4/15 6:03pm
 ;;1.0;C0TS VISTA TERMINOLOGY SERVER;;Mar 21, 2016;Build 1
 ;
 Q
 ;
SUMMHTML(RTN,IARY) ; HTML rendering of prefetch summary
 N GTOP,GBOT
 ;S GTOP="<!DOCTYPE HTML><html><head></head><body>"
 S GTOP(1)="<!DOCTYPE HTML>"
 S GTOP(2)="<html>"
 S GTOP(3)="<head>"
 S GTOP(4)="<meta charset=""utf-8"">"
 S GTOP(5)="<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"" />"
 S GTOP(6)="<title>Prefetch</title>"
 S GTOP(7)="<link rel=""stylesheet"" type=""text/css"" href=""/resources/css/prefetch.css"" />"
 S GTOP(8)="</head>"
 S GTOP(9)="<body>"
 M @RTN=GTOP
 S GBOT="</body></html>"
 N GARY,GTMP1,GTMP2,GTMP3,GTMP4,GTMP5,GTMP6,GTMP7,GURL
 ;S GURL="http://52.11.197.74:9080/prefetchSummary"
 ;S GURL="http://prefetch.fiscientific.org/prefetchSummary"
 S GURL="/prefetchSummary"
 S GTMP1=" <a href="""_GURL_"?format=html"" > html</a>"
 S GTMP2=" <a href="""_GURL_"?format=xml"" target=""_blank""> xml</a>"
 S GTMP3=" <a href="""_GURL_"?format=json"" target=""_blank""> json</a>"
 S GTMP4=" <a href="""_GURL_"?format=outline"" target=""_blank""> outline</a>"
 S GTMP5="<p><a href=""/processPatient"" class=""buttonlink"" >Schedule PreFetch Test Cycle</a></p>"
 S GTMP6="<p><a href=""/loadTestPatients"" class=""buttonlink"" >Load Test Patients</a>"
 N GTMP7A S GTMP7A=$NA(XTMP("EHEXNEXT"))
 S GTMP7="  "_$$PNXTCNT_" Patients in <a href=""/global/XTMP(%22EHEXNEXT%22)"" target=""_blank"">Next Cycle Staging Area</a></p>"
 D ADDTO(RTN,"<p>this page"_GTMP1_GTMP2_GTMP3_GTMP4_"</p>")
 D ADDTO(RTN,GTMP5_GTMP6_GTMP7)
 D STATHTML(RTN)
 N %J S %J=0
 F  S %J=$O(@IARY@("Summary",%J)) Q:'%J  D  ; index the array by status
 . N STAT S STAT=$P($O(@IARY@("Summary",%J,"")),"@",1)
 . S @IARY@(STAT,%J)=""
 N GARY
 S GARY("WIDTH")="60%"
 S GARY("TITLE")="Summary"
 S GARY("HEADER",1)="Patients"
 S GARY("HEADER",2)="Documents"
 N TOT,PEN,COMP,PRO,ERR
 S PEN=@IARY@("COUNT","patient@PENDING")
 S COMP=@IARY@("COUNT","patient@COMPLETE")
 S PRO=@IARY@("COUNT","patient@INPROCESS")
 S ERR=@IARY@("COUNT","patient@ERROR")
 S TOT=PEN+COMP+PRO+ERR
 S GARY(1,1)="Total: "_TOT
 S GARY(2,1)="Pending: "_PEN
 S GARY(3,1)="In Process "_PRO
 S GARY(4,1)="Complete: "_COMP
 S GARY(5,1)="Error: "_ERR
 S PEN=@IARY@("COUNT","document@PENDING")
 S COMP=@IARY@("COUNT","document@COMPLETE")
 S ERR=+$G(@IARY@("COUNT","document@ERROR"))
 S PRO=@IARY@("COUNT","document@INPROCESS")
 S TOT=PEN+COMP+PRO+ERR
 S GARY(1,2)="Total: "_TOT
 S GARY(2,2)="Pending: "_PEN
 S GARY(3,2)="In Process "_PRO
 S GARY(4,2)="Complete: "_COMP
 S GARY(5,2)="Error: "_ERR
 D GENHTML(RTN,"GARY")
 N %I,FORM
 F FORM="COMPLETE","INPROCESS","PENDING","ERROR" D  ;
 . N GARY
 . S GARY("WIDTH")="60%"
 . S GARY("TITLE")="Patients with status "_FORM
 . S GARY("HEADER",1)="Patient ICN"
 . S GARY("HEADER",2)="Status"
 . S %I=""
 . N ZCNT S ZCNT=0
 . F  S %I=$O(@IARY@(FORM,%I)) Q:%I=""  D  ;
 . . S ZCNT=ZCNT+1
 . . N ZTMP,ICN,PURL
 . . S ICN=$G(@IARY@("Summary",%I,FORM_"@icn")) 
 . . ;S PURL="http://52.11.197.74:9080/queryPatient?format=html&icn="_ICN
 . . ;S PURL="http://prefetch.fiscientific.org/queryPatient?format=html&icn="_ICN
 . . S PURL="/queryPatient?format=html&icn="_ICN
 . . S ZTMP="<a href="""_PURL_""" target=_blank>"_ICN_"</a>"
 . . N ZLOG,LOGURL
 . . S LOGURL="/log?icn="_ICN
 . . S ZLOG=" <a href="""_LOGURL_""" target=_blank>(log)</a> "
 . . S ZTMP=ZTMP_ZLOG
 . . I FORM'["PENDING" D  ; show audit and pa results
 . . . N ZIEN S ZIEN=$O(^XTMP("EHEXPAT","ICN",ICN,""))
 . . . N AUD S AUD=$G(^XTMP("EHEXPAT",ZIEN,"AURESULT"))
 . . . I AUD'="" S ZTMP=ZTMP_" AUDIT: "_AUD
 . . . I AUD=1 Q  ; no PA done
 . . . N PAR S PAR=$G(^XTMP("EHEXPAT",ZIEN,"PARESULT"))
 . . . I PAR'="" S ZTMP=ZTMP_" PA: "_PAR
 . . S GARY(ZCNT,1)=ZTMP
 . . N DURL S DURL=FORM
 . . I FORM="COMPLETE" D  ;
 . . . N DQ S DQ=@IARY@("Summary",%I,"COMPLETE@id")
 . . . S DURL="<a href="""_DQ_""" target=_blank>COMPLETE</a>"
 . . . S DURL=DURL_" Document Count: "_$$DCOUNT(ICN)
 . . S GARY(ZCNT,2)=DURL
 . D GENHTML(RTN,"GARY")
 S @RTN@($O(@RTN@(""),-1)+1)=GBOT
 ;ZWR IARY
 ;ZWR RTN
 Q
STATHTML(RTN) ; generate status html and add it to RTN - passed by name
 ;
 N ARY
 S ARY("WIDTH")="60%"
 S ARY("TITLE")="Prefetch Status as of "_$$FMTE^XLFDT($$NOW^XLFDT)
 S ARY("HEADER",1)="Control"
 S ARY("HEADER",2)="Current Value"
 N CN S CN=$NA(^XTMP("EHXCTRL"))
 N START,STOP
 S START=$G(@CN@("TODAY START TIME"))
 S STOP=$G(@CN@("TODAY STOP TIME"))
 S ARY(1,1)="Prefetch Start Time"
 S ARY(1,2)=$G(@CN@("TODAY START TIME EXTERNAL"))
 S ARY(2,1)="Prefetch Stop Time"
 S ARY(2,2)=$G(@CN@("TODAY STOP TIME EXTERNAL"))
 ;N JBS S JBS=$$N^EHXPRE6()
 S ARY(3,1)="Status"
 S:$$INWINDOW^EHXPREJ ARY(3,2)="RUNNING"
 S:'$$INWINDOW^EHXPREJ ARY(3,2)="NOT RUNNING"
 ;S:+$$PARM^EHXPREA("STOP")<0 ARY(3,2)="STOPPED"
 S:+$G(^XTMP("EHXCTRL","STOP MONITOR")) ARY(3,2)="STOPPED"
 ;S ARY(3,2)=$S(JBS>0:"RUNNING",1:"NOT RUNNING")
 S ARY(4,1)="Monitor Check In"
 N MON S MON=$G(@CN@("STATUS"))
 S ARY(4,2)="Job: "_$P(MON,"^",1)_" Time: "_$$FMTE^XLFDT($P(MON,"^",2))
 I $$INWINDOW^EHXPREJ D  ;
 . N MIN
 . S MIN=$P($$FMDIFF^XLFDT(STOP,$$NOW^XLFDT,2)/60,".")
 . S ARY(5,1)="Minutes until Prefetch Stops"
 . S ARY(5,2)=MIN
 I '$$INWINDOW^EHXPREJ D  ;
 . N MIN,SEC
 . S SEC=$$FMDIFF^XLFDT(START,$$NOW^XLFDT,2)
 . S MIN=$P(SEC/60,".")
 . S ARY(5,1)="Minutes until Prefetch Starts"
 . S ARY(5,2)=MIN
 D GENHTML(RTN,"ARY")
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
