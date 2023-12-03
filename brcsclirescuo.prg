// Programa   : BRCSCLIRESCUO
// Fecha/Hora : 02/09/2022 22:36:24
// Propósito  : "Situacional de Cuotas Mensuales"
// Creado Por : Automáticamente por BRWMAKER
// Llamado por: <DPXBASE>
// Aplicación : Gerencia
// Tabla      : <TABLA>

#INCLUDE "DPXBASE.CH"

PROCE MAIN(cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle,cCodCli,lCodInv,cCodInv,dFecha,nValCam,oFrmMain,cTipDoc)
   LOCAL aData,aFechas,cFileMem:="USER\BRCSCLIRESCUO.MEM",V_nPeriodo:=1,cCodPar,nPrecio:=0
   LOCAL V_dDesde:=CTOD(""),V_dHasta:=CTOD("")
   LOCAL cServer  :=oDp:cRunServer
   LOCAL lConectar:=.F.
   LOCAL nCol     :=3,lIva:=.F.,nPorIva:=0,nPor,cTipIva:="GN",nIva:=0

   oDp:cRunServer:=NIL

   IF Type("oCSCLIRESCUO")="O" .AND. oCSCLIRESCUO:oWnd:hWnd>0
      RETURN EJECUTAR("BRRUNNEW",oCSCLIRESCUO,GetScript())
   ENDIF

   DEFAULT cCodCli:="",;
           lCodInv:=!Empty(cCodCli),;
           cCodInv:="",;
           oDp:cTipDocClb:="CUO",;
           dFecha :=oDp:dFecha,;
           nValCam:=EJECUTAR("DPGETVALCAM",oDp:cMonedaExt,dFecha)


   DEFAULT cTipDoc:=oDp:cTipDocClb

// ? cCodInv,"cCodInv"

   IF lCodInv .AND. !Empty(cCodCli)
      cCodInv:=SQLGET("DPCLIENTEPROG","DPG_CODINV","DPG_CODIGO"+GetWhere("=",cCodCli))
   ENDIF

  IF !Empty(cCodInv) .OR. !Empty(cCodCli)

     nPrecio:=SQLGET("DPCLIENTEPROG","PRE_PRECIO,PRE_CODMON"," INNER JOIN DPINV            ON DPG_CODINV=INV_CODIGO  "+;
                                                             " INNER JOIN DPCLIENTES       ON DPG_CODIGO=CLI_CODIGO AND LEFT(CLI_SITUAC,1)='A' "+;
                                                             " LEFT JOIN VIEW_UNDMEDXINV   ON INV_CODIGO=IME_CODIGO "+;
                                                             " LEFT JOIN VIEW_DPINVPRECIOS ON DPINV.INV_CODIGO=PRE_CODIGO "+;
                                                             " WHERE DPG_CODIGO"+GetWhere("=",cCodCli)+" AND INV_CODIGO "+GetWhere("=",cCodInv)+;
                                                             " GROUP BY DPG_CODINV ")
     IF Empty(cCodCli)

       nPrecio:=SQLGET("DPCLIENTEPROG","PRE_PRECIO,PRE_CODMON"," INNER JOIN DPINV            ON DPG_CODINV=INV_CODIGO  "+;
                                                               " INNER JOIN DPCLIENTES       ON DPG_CODIGO=CLI_CODIGO AND LEFT(CLI_SITUAC,1)"+GetWhere("=","A")+;
                                                               " LEFT JOIN VIEW_UNDMEDXINV   ON INV_CODIGO=IME_CODIGO "+;
                                                               " LEFT JOIN VIEW_DPINVPRECIOS ON DPINV.INV_CODIGO=PRE_CODIGO "+;
                                                               " WHERE INV_CODIGO "+GetWhere("=",cCodInv)+;
                                                               " GROUP BY DPG_CODINV ")

     ENDIF

     lIva   :=SQLGET("DPTIPDOCCLI","TDC_IVA","TDC_TIPO"+GetWhere("=",oDp:cTipDocClb))

     IF lIva
       nPorIva:=EJECUTAR("IVACAL",cTipIva,nCol,dFecha)
       nPor   :=1 // (nPorIva/100)+1
     ELSE
       nPor   :=1
     ENDIF

// ? nPrecio,"nPrecio"

     nIva   :=PORCEN(nPrecio,nPorIva)
//   nPrecio:=nPrecio+nIva
// ? nPorIva,"nPorIva",dFecha,nPrecio,nIva,"nIVA"
//     nIva   :=PORCEN(nPrecio,nPorIva)

  ENDIF

  IF !Empty(cServer)

     MsgRun("Conectando con Servidor "+cServer+" ["+ALLTRIM(SQLGET("DPSERVERBD","SBD_DOMINI","SBD_CODIGO"+GetWhere("=",cServer)))+"]",;
            "Por Favor Espere",{||lConectar:=EJECUTAR("DPSERVERDBOPEN",cServer)})

     IF !lConectar
        RETURN .F.
        ENDIF
     ENDIF


   cTitle:="Situacional de Cuotas Mensuales" +IF(Empty(cTitle),"",cTitle)+" "+cTipDoc

   oDp:oFrm:=NIL

   IF FILE(cFileMem) .AND. nPeriodo=NIL
      RESTORE FROM (cFileMem) ADDI
      nPeriodo:=V_nPeriodo
   ENDIF

   DEFAULT cCodSuc :=oDp:cSucursal,;
           nPeriodo:=4,;
           dDesde  :=CTOD(""),;
           dHasta  :=CTOD("")


   // Obtiene el Código del Parámetro

   IF !Empty(cWhere)

      cCodPar:=ATAIL(_VECTOR(cWhere,"="))

      IF TYPE(cCodPar)="C"
        cCodPar:=SUBS(cCodPar,2,LEN(cCodPar))
        cCodPar:=LEFT(cCodPar,LEN(cCodPar)-1)
      ENDIF

   ENDIF

   IF .T. .AND. (!nPeriodo=11 .AND. (Empty(dDesde) .OR. Empty(dhasta)))

       aFechas:=EJECUTAR("DPDIARIOGET",nPeriodo)
       dDesde :=aFechas[1]
       dHasta :=aFechas[2]

   ENDIF

   aData :=LEERDATA(HACERWHERE(dDesde,dHasta,cWhere),NIL,cServer,NIL,cCodCli,cCodInv,cTipDoc)

   IF Empty(aData)
      MensajeErr("no hay "+cTitle,"Información no Encontrada")
      RETURN .F.
   ENDIF

   ViewData(aData,cTitle,oDp:cWhere)

   oDp:oFrm:=oCSCLIRESCUO

RETURN .T.


FUNCTION ViewData(aData,cTitle,cWhere_)
   LOCAL oBrw,oCol,aTotal:=ATOTALES(aData)
   LOCAL oFont,oFontB
   LOCAL aPeriodos:=ACLONE(oDp:aPeriodos)
   LOCAL aCoors:=GetCoors( GetDesktopWindow() )

   DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 BOLD
   DEFINE FONT oFontB NAME "Tahoma"   SIZE 0, -12 BOLD

   DpMdi(cTitle,"oCSCLIRESCUO","BRCSCLIRESCUO.EDT")

   oCSCLIRESCUO:Windows(0,0,aCoors[3]-160,MIN(1408,aCoors[4]-10),.T.) // Maximizado

   oCSCLIRESCUO:cCodSuc   :=cCodSuc
   oCSCLIRESCUO:lMsgBar   :=.F.
   oCSCLIRESCUO:cPeriodo  :=aPeriodos[nPeriodo]
   oCSCLIRESCUO:cCodSuc   :=cCodSuc
   oCSCLIRESCUO:nPeriodo  :=nPeriodo
   oCSCLIRESCUO:cNombre   :=""
   oCSCLIRESCUO:dDesde    :=dDesde
   oCSCLIRESCUO:cServer   :=cServer
   oCSCLIRESCUO:dHasta    :=dHasta
   oCSCLIRESCUO:cWhere    :=cWhere
   oCSCLIRESCUO:cWhere_   :=cWhere_
   oCSCLIRESCUO:cWhereQry :=""
   oCSCLIRESCUO:cSql      :=oDp:cSql
   oCSCLIRESCUO:oWhere    :=TWHERE():New(oCSCLIRESCUO)
   oCSCLIRESCUO:cCodPar   :=cCodPar // Código del Parámetro
   oCSCLIRESCUO:lWhen     :=.T.
   oCSCLIRESCUO:cTextTit  :="" // Texto del Titulo Heredado
   oCSCLIRESCUO:oDb       :=oDp:oDb
   oCSCLIRESCUO:cBrwCod   :="CSCLIRESCUO"
   oCSCLIRESCUO:lTmdi     :=.T.
   oCSCLIRESCUO:aHead     :={}
   oCSCLIRESCUO:lBarDef   :=.T.     // Activar Modo Diseño.
   oCSCLIRESCUO:cCodCli   :=cCodCli // Activar Modo Diseño.
   oCSCLIRESCUO:cNombre   :=SQLGET("DPCLIENTES","CLI_NOMBRE","CLI_CODIGO"+GetWhere("=",cCodCli))
   oCSCLIRESCUO:cCodInv   :=cCodInv
   oCSCLIRESCUO:cInvDescri:=SQLGET("DPINV","INV_DESCRI","INV_CODIGO"+GetWhere("=",cCodInv))
   oCSCLIRESCUO:lFindInv  :=ISSQLFIND("DPINV","INV_CODIGO"+GetWhere("=",cCodInv))
   oCSCLIRESCUO:nPrecio   :=nPrecio
   oCSCLIRESCUO:nIva      :=nIva
   oCSCLIRESCUO:nPorIva   :=nPorIva
   oCSCLIRESCUO:nTotal    :=nPrecio+nIva
   oCSCLIRESCUO:nValCam   :=nValCam
   oCSCLIRESCUO:oFrmMain  :=oFrmMain
   oCSCLIRESCUO:nCuantos  :=0 
   oCSCLIRESCUO:cTipDoc   :=cTipDoc

   // Guarda los parámetros del Browse cuando cierra la ventana
   oCSCLIRESCUO:bValid   :={|| EJECUTAR("BRWSAVEPAR",oCSCLIRESCUO)}

   oCSCLIRESCUO:lBtnRun     :=.F.
   oCSCLIRESCUO:lBtnMenuBrw :=.F.
   oCSCLIRESCUO:lBtnSave    :=.F.
   oCSCLIRESCUO:lBtnCrystal :=.F.
   oCSCLIRESCUO:lBtnRefresh :=.F.
   oCSCLIRESCUO:lBtnHtml    :=.T.
   oCSCLIRESCUO:lBtnExcel   :=.T.
   oCSCLIRESCUO:lBtnPreview :=.T.
   oCSCLIRESCUO:lBtnQuery   :=.F.
   oCSCLIRESCUO:lBtnOptions :=.T.
   oCSCLIRESCUO:lBtnPageDown:=.T.
   oCSCLIRESCUO:lBtnPageUp  :=.T.
   oCSCLIRESCUO:lBtnFilters :=.T.
   oCSCLIRESCUO:lBtnFind    :=.T.
   oCSCLIRESCUO:lBtnColor   :=.F.

   oCSCLIRESCUO:nClrPane1:=16775408
   oCSCLIRESCUO:nClrPane2:=16771797

   oCSCLIRESCUO:nClrText :=0
   oCSCLIRESCUO:nClrText1:=CLR_HBLUE
   oCSCLIRESCUO:nClrText2:=4887808
   oCSCLIRESCUO:nClrText3:=0

   oCSCLIRESCUO:oBrw:=TXBrowse():New( IF(oCSCLIRESCUO:lTmdi,oCSCLIRESCUO:oWnd,oCSCLIRESCUO:oDlg ))
   oCSCLIRESCUO:oBrw:SetArray( aData, .F. )
   oCSCLIRESCUO:oBrw:SetFont(oFont)

   oCSCLIRESCUO:oBrw:lFooter     := .T.
   oCSCLIRESCUO:oBrw:lHScroll    := .T.
   oCSCLIRESCUO:oBrw:nHeaderLines:= 2
   oCSCLIRESCUO:oBrw:nDataLines  := 1
   oCSCLIRESCUO:oBrw:nFooterLines:= 1

   oCSCLIRESCUO:aData            :=ACLONE(aData)

   AEVAL(oCSCLIRESCUO:oBrw:aCols,{|oCol|oCol:oHeaderFont:=oFontB})
  

  // Campo: DIA_ANO
  oCol:=oCSCLIRESCUO:oBrw:aCols[1]
  oCol:cHeader      :='Año'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 32

  // Campo: DIA_MES
  oCol:=oCSCLIRESCUO:oBrw:aCols[2]
  oCol:cHeader      :='Mes'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 16
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,2],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[2],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[2],oCol:cEditPicture)


  // Campo: DIA_CMES
  oCol:=oCSCLIRESCUO:oBrw:aCols[3]
  oCol:cHeader      :='Mes'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 160

  // Campo: MTOCUO
  oCol:=oCSCLIRESCUO:oBrw:aCols[4]
  oCol:cHeader      :='Monto'+CRLF+'Cuota'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 136
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,4],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[4],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[4],oCol:cEditPicture)


  // Campo: MTODIV
  oCol:=oCSCLIRESCUO:oBrw:aCols[5]
  oCol:cHeader      :='Cuota'+CRLF+'Divisa'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 120
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,5],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[5],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[5],oCol:cEditPicture)


  // Campo: CUANTOSCUO
  oCol:=oCSCLIRESCUO:oBrw:aCols[6]
  oCol:cHeader      :='Cant.'+CRLF+'Cuotas'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 144
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,6],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[6],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[6],oCol:cEditPicture)


  // Campo: MTOFAV
  oCol:=oCSCLIRESCUO:oBrw:aCols[7]
  oCol:cHeader      :='Monto'+CRLF+'Facturado'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 136
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,7],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[7],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[7],oCol:cEditPicture)


  // Campo: MTOFAVDIV
  oCol:=oCSCLIRESCUO:oBrw:aCols[8]
  oCol:cHeader      :='Por Facturar'+CRLF+'Divisa'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 120
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,8],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[8],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[8],oCol:cEditPicture)


  // Campo: MTOPAG
  oCol:=oCSCLIRESCUO:oBrw:aCols[9]
  oCol:cHeader      :='Monto'+CRLF+'Pagado'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 136
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,9],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[9],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[9],oCol:cEditPicture)


  // Campo: MTOPAGDIV
  oCol:=oCSCLIRESCUO:oBrw:aCols[10]
  oCol:cHeader      :='Pagado'+CRLF+'Divisa'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 120
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,10],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[10],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[10],oCol:cEditPicture)


  // Campo: CUANTOSFAV
  oCol:=oCSCLIRESCUO:oBrw:aCols[11]
  oCol:cHeader      :='Cant.'+CRLF+'Facturas'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 144
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,11],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[11],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[11],oCol:cEditPicture)


  // Campo: XFACTURAR
  oCol:=oCSCLIRESCUO:oBrw:aCols[12]
  oCol:cHeader      :='Cant.'+CRLF+'X Fact.'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 144
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='999,999'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,12],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[12],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[12],oCol:cEditPicture)


  // Campo: MTODIV
  oCol:=oCSCLIRESCUO:oBrw:aCols[13]
  oCol:cHeader      :='Por Cobrar'+CRLF+'Divisa '+oDp:cMonedaExt
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
  oCol:nWidth       := 120
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,13],;
                              oCol  := oCSCLIRESCUO:oBrw:aCols[13],;
                              FDP(nMonto,oCol:cEditPicture)}
   oCol:cFooter      :=FDP(aTotal[13],oCol:cEditPicture)


   oCol:=oCSCLIRESCUO:oBrw:aCols[14]
   oCol:bLClickHeader:= {|r,c,f,o| SortArray( o, oCSCLIRESCUO:oBrw:aArrayData ) } 
   oCol:cHeader      :="Marcar"
   oCol:AddBmpFile("BITMAPS\checkverde.bmp")
   oCol:AddBmpFile("BITMAPS\checkrojo.bmp")
   oCol:bBmpData    := { |oBrw|oBrw:=oCSCLIRESCUO:oBrw,IIF(oBrw:aArrayData[oBrw:nArrayAt,14],1,2) }
   oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
   oCol:bStrData    :={||""}



   oCSCLIRESCUO:oBrw:aCols[1]:cFooter:=" #"+LSTR(LEN(aData))

   oCSCLIRESCUO:oBrw:bClrStd  := {|oBrw,nClrText,aLine|oBrw:=oCSCLIRESCUO:oBrw,aLine:=oBrw:aArrayData[oBrw:nArrayAt],;
                                                 nClrText:=oCSCLIRESCUO:nClrText,;
                                                 nClrText:=IF(aLine[5]>0,oCSCLIRESCUO:nClrText1,nClrText),;
                                                 nClrText:=IF(aLine[8]>0,oCSCLIRESCUO:nClrText2,nClrText),;
                                                 {nClrText,iif( oBrw:nArrayAt%2=0, oCSCLIRESCUO:nClrPane1, oCSCLIRESCUO:nClrPane2 ) } }

//   oCSCLIRESCUO:oBrw:bClrHeader            := {|| {0,14671839 }}
//   oCSCLIRESCUO:oBrw:bClrFooter            := {|| {0,14671839 }}

   oCSCLIRESCUO:oBrw:bClrHeader          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
   oCSCLIRESCUO:oBrw:bClrFooter          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}

   oCSCLIRESCUO:oBrw:bLDblClick:={|oBrw|oCSCLIRESCUO:RUNCLICK() }

   oCSCLIRESCUO:oBrw:bChange:={||oCSCLIRESCUO:BRWCHANGE()}
   oCSCLIRESCUO:oBrw:CreateFromCode()


   oCSCLIRESCUO:oWnd:oClient := oCSCLIRESCUO:oBrw

   oCSCLIRESCUO:Activate({||oCSCLIRESCUO:ViewDatBar()})

   oCSCLIRESCUO:BRWRESTOREPAR()

RETURN .T.

/*
// Barra de Botones
*/
FUNCTION ViewDatBar()
   LOCAL oCursor,oBar,oBtn,oFont,oCol,oFont2
   LOCAL oDlg:=IF(oCSCLIRESCUO:lTmdi,oCSCLIRESCUO:oWnd,oCSCLIRESCUO:oDlg)
   LOCAL nLin:=2,nCol:=0
   LOCAL nWidth:=oCSCLIRESCUO:oBrw:nWidth()

   oCSCLIRESCUO:oBrw:GoBottom(.T.)
   oCSCLIRESCUO:oBrw:Refresh(.T.)

//   IF !File("FORMS\BRCSCLIRESCUO.EDT")
//     oCSCLIRESCUO:oBrw:Move(44,0,1408+50,460)
//   ENDIF

   DEFINE CURSOR oCursor HAND
   IF !oDp:lBtnText 
     DEFINE BUTTONBAR oBar SIZE 52-15,60-15+IF(Empty(oCSCLIRESCUO:cCodCli),0,50) OF oDlg 3D CURSOR oCursor
   ELSE 
     DEFINE BUTTONBAR oBar SIZE oDp:nBtnWidth,oDp:nBarnHeight+6 OF oDlg 3D CURSOR oCursor 
   ENDIF 

   DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -10 BOLD

 // Emanager no Incluye consulta de Vinculos

   oCSCLIRESCUO:oFontBtn   :=oFont    
   oCSCLIRESCUO:nClrPaneBar:=oDp:nGris
   oCSCLIRESCUO:oBrw:oLbx  :=oCSCLIRESCUO

 DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\RUN.BMP",NIL,"BITMAPS\RUNG.BMP";
            TOP PROMPT "Crear"; 
            ACTION oCSCLIRESCUO:CREARCUOTAS();
            WHEN oCSCLIRESCUO:nCuantos>0

   oBtn:cToolTip:="Generar Cuotas"

   oCSCLIRESCUO:oBtnRun:=oBtn

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\notasdeconsumo.bmp";
          TOP PROMPT "Ingresos"; 
          ACTION oCSCLIRESCUO:NOTASDECONSUMO();
          WHEN .T.

   oBtn:cToolTip:="Registrar Notas de Consumo o Ingresos"

   oCSCLIRESCUO:oBtnRun:=oBtn

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\CXC.BMP";
          TOP PROMPT "CxC"; 
          ACTION oCSCLIRESCUO:VERBRCXC()

   oBtn:cToolTip:="Cuentas por Cobrar"


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\facturavta.BMP";
          TOP PROMPT "Factura"; 
          ACTION oCSCLIRESCUO:VERFACTURAS()

   oBtn:cToolTip:="Ver Facturas"



   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XBROWSE.BMP";
          TOP PROMPT "Detalles"; 
          ACTION  oCSCLIRESCUO:VERBRDOC()

   oBtn:cToolTip:="Ver Documentos"


/*
   IF Empty(oCSCLIRESCUO:cServer) .AND. !Empty(SQLGET("DPBRWLNK","EBR_CODIGO","EBR_CODIGO"+GetWhere("=","CSCLIRESCUO")))
*/

   IF ISSQLFIND("DPBRWLNKCONCAT","BRC_CODIGO"+GetWhere("=","CSCLIRESCUO"))

       DEFINE BUTTON oBtn;
       OF oBar;
       NOBORDER;
       FONT oFont;
       FILENAME "BITMAPS\XBROWSE.BMP";
       ACTION EJECUTAR("BRWRUNBRWLINK",oCSCLIRESCUO:oBrw,"CSCLIRESCUO",oCSCLIRESCUO:cSql,oCSCLIRESCUO:nPeriodo,oCSCLIRESCUO:dDesde,oCSCLIRESCUO:dHasta,oCSCLIRESCUO)

       oBtn:cToolTip:="Ejecutar Browse Vinculado(s)"
       oCSCLIRESCUO:oBtnRun:=oBtn

       oCSCLIRESCUO:oBrw:bLDblClick:={||EVAL(oCSCLIRESCUO:oBtnRun:bAction) }

   ENDIF




IF oCSCLIRESCUO:lBtnRun

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            MENU EJECUTAR("BRBTNMENU",{"Opcion 1",;
                                       "Opcion 2",;
                                       "Opcion 3"},;
                                       "oCSCLIRESCUO");
            FILENAME "BITMAPS\RUN.BMP";
            ACTION oCSCLIRESCUO:BTNRUN()

      oBtn:cToolTip:="Opciones de Ejecucion"

ENDIF

IF oCSCLIRESCUO:lBtnColor

     oCSCLIRESCUO:oBtnColor:=NIL

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            TOP PROMPT "Color"; 
            FILENAME "BITMAPS\COLORS.BMP";
            MENU EJECUTAR("BRBTNMENUCOLOR",oCSCLIRESCUO:oBrw,oCSCLIRESCUO,oCSCLIRESCUO:oBtnColor,{||EJECUTAR("BRWCAMPOSOPC",oCSCLIRESCUO,.T.)});
            ACTION EJECUTAR("BRWSELCOLORFIELD",oCSCLIRESCUO,.T.)

    oBtn:cToolTip:="Personalizar Colores en los Campos"

    oCSCLIRESCUO:oBtnColor:=oBtn

ENDIF



IF oCSCLIRESCUO:lBtnSave
/*
      DEFINE BITMAP OF OUTLOOK oBRWMENURUN:oOut ;
             BITMAP "BITMAPS\XSAVE.BMP";
             PROMPT "Guardar Consulta";
               TOP PROMPT "Grabar"; 
              ACTION  EJECUTAR("DPBRWSAVE",oCSCLIRESCUO:oBrw,oCSCLIRESCUO:oFrm)
*/
ENDIF

IF oCSCLIRESCUO:lBtnMenuBrw

 DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\BRWMENU.BMP",NIL,"BITMAPS\BRWMENUG.BMP";
          TOP PROMPT "Menú"; 
          ACTION  (EJECUTAR("BRWBUILDHEAD",oCSCLIRESCUO),;
                  EJECUTAR("DPBRWMENURUN",oCSCLIRESCUO,oCSCLIRESCUO:oBrw,oCSCLIRESCUO:cBrwCod,oCSCLIRESCUO:cTitle,oCSCLIRESCUO:aHead));
          WHEN !Empty(oCSCLIRESCUO:oBrw:aArrayData[1,1])

   oBtn:cToolTip:="Menú de Opciones"

ENDIF


IF oCSCLIRESCUO:lBtnFind

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XFIND.BMP";
          TOP PROMPT "Buscar"; 
          ACTION  EJECUTAR("BRWSETFIND",oCSCLIRESCUO:oBrw)

   oBtn:cToolTip:="Buscar"
ENDIF

IF oCSCLIRESCUO:lBtnFilters

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\FILTRAR.BMP";
          MENU EJECUTAR("BRBTNMENUFILTER",oCSCLIRESCUO:oBrw,oCSCLIRESCUO);
          TOP PROMPT "Filtrar"; 
          ACTION  EJECUTAR("BRWSETFILTER",oCSCLIRESCUO:oBrw)

   oBtn:cToolTip:="Filtrar Registros"
ENDIF

IF oCSCLIRESCUO:lBtnOptions

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\OPTIONS.BMP",NIL,"BITMAPS\OPTIONSG.BMP";
          TOP PROMPT "Opciones"; 
          ACTION  EJECUTAR("BRWSETOPTIONS",oCSCLIRESCUO:oBrw);
          WHEN LEN(oCSCLIRESCUO:oBrw:aArrayData)>1

   oBtn:cToolTip:="Filtrar según Valores Comunes"

ENDIF

IF oCSCLIRESCUO:lBtnRefresh

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\REFRESH.BMP";
          TOP PROMPT "Refrescar"; 
          ACTION  oCSCLIRESCUO:BRWREFRESCAR()

   oBtn:cToolTip:="Refrescar"

ENDIF

IF oCSCLIRESCUO:lBtnCrystal

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\CRYSTAL.BMP";
          TOP PROMPT "Crystal"; 
          ACTION  EJECUTAR("BRWTODBF",oCSCLIRESCUO)

   oBtn:cToolTip:="Visualizar Mediante Crystal Report"

ENDIF

IF oCSCLIRESCUO:lBtnExcel


     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\EXCEL.BMP";
            TOP PROMPT "Excel"; 
            ACTION  (EJECUTAR("BRWTOEXCEL",oCSCLIRESCUO:oBrw,oCSCLIRESCUO:cTitle,oCSCLIRESCUO:cNombre))

     oBtn:cToolTip:="Exportar hacia Excel"

     oCSCLIRESCUO:oBtnXls:=oBtn

ENDIF

IF oCSCLIRESCUO:lBtnHtml

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\html.BMP";
          TOP PROMPT "Html"; 
          ACTION  (oCSCLIRESCUO:HTMLHEAD(),EJECUTAR("BRWTOHTML",oCSCLIRESCUO:oBrw,NIL,oCSCLIRESCUO:cTitle,oCSCLIRESCUO:aHead))

   oBtn:cToolTip:="Generar Archivo html"

   oCSCLIRESCUO:oBtnHtml:=oBtn

ENDIF


IF oCSCLIRESCUO:lBtnPreview

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\PREVIEW.BMP";
          TOP PROMPT "Preview"; 
          ACTION  (EJECUTAR("BRWPREVIEW",oCSCLIRESCUO:oBrw))

   oBtn:cToolTip:="Previsualización"

   oCSCLIRESCUO:oBtnPreview:=oBtn

ENDIF

   IF ISSQLGET("DPREPORTES","REP_CODIGO","BRCSCLIRESCUO")

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\XPRINT.BMP";
            TOP PROMPT "Imprimir"; 
            ACTION  oCSCLIRESCUO:IMPRIMIR()

      oBtn:cToolTip:="Imprimir"

     oCSCLIRESCUO:oBtnPrint:=oBtn

   ENDIF

IF oCSCLIRESCUO:lBtnQuery


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\QUERY.BMP";
          ACTION oCSCLIRESCUO:BRWQUERY()

   oBtn:cToolTip:="Imprimir"

ENDIF


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xTOP.BMP";
          TOP PROMPT "Primero"; 
          ACTION  (oCSCLIRESCUO:oBrw:GoTop(),oCSCLIRESCUO:oBrw:Setfocus())

IF nWidth>800 .OR. nWidth=0

   IF oCSCLIRESCUO:lBtnPageDown

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\xSIG.BMP";
              TOP PROMPT "Avance"; 
              ACTION  (oCSCLIRESCUO:oBrw:PageDown(),oCSCLIRESCUO:oBrw:Setfocus())
  ENDIF

  IF  oCSCLIRESCUO:lBtnPageUp

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           FILENAME "BITMAPS\xANT.BMP";
           TOP PROMPT "Anterior"; 
           ACTION  (oCSCLIRESCUO:oBrw:PageUp(),oCSCLIRESCUO:oBrw:Setfocus())

  ENDIF

ENDIF

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xFIN.BMP";
          TOP PROMPT "Ultimo"; 
          ACTION  (oCSCLIRESCUO:oBrw:GoBottom(),oCSCLIRESCUO:oBrw:Setfocus())

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XSALIR.BMP";
          TOP PROMPT "Cerrar"; 
          ACTION  oCSCLIRESCUO:Close()

  oCSCLIRESCUO:oBrw:SetColor(0,oCSCLIRESCUO:nClrPane1)

  oCSCLIRESCUO:SETBTNBAR(40+20,40+10,oBar)


  EVAL(oCSCLIRESCUO:oBrw:bChange)

  oBar:SetColor(CLR_BLACK,oDp:nGris)

  AEVAL(oBar:aControls,{|o,n|o:SetColor(CLR_BLACK,oDp:nGris)})

  oCSCLIRESCUO:oBar:=oBar

  nCol:=1048+32
  //nLin:=<NLIN> // 08

  // Controles se Inician luego del Ultimo Boton
  nCol:=32
  AEVAL(oBar:aControls,{|o,n|nCol:=nCol+o:nWidth() })

  //
  // Campo : Periodo
  //

  @ nLin, nCol COMBOBOX oCSCLIRESCUO:oPeriodo  VAR oCSCLIRESCUO:cPeriodo ITEMS aPeriodos;
                SIZE 100,200;
                PIXEL;
                OF oBar;
                FONT oFont;
                ON CHANGE oCSCLIRESCUO:LEEFECHAS();
                WHEN oCSCLIRESCUO:lWhen


  ComboIni(oCSCLIRESCUO:oPeriodo )

  @ nLin, nCol+103 BUTTON oCSCLIRESCUO:oBtn PROMPT " < " SIZE 27,24;
                 FONT oFont;
                 PIXEL;
                 OF oBar;
                 ACTION (EJECUTAR("PERIODOMAS",oCSCLIRESCUO:oPeriodo:nAt,oCSCLIRESCUO:oDesde,oCSCLIRESCUO:oHasta,-1),;
                         EVAL(oCSCLIRESCUO:oBtn:bAction));
                WHEN oCSCLIRESCUO:lWhen


  @ nLin, nCol+130 BUTTON oCSCLIRESCUO:oBtn PROMPT " > " SIZE 27,24;
                 FONT oFont;
                 PIXEL;
                 OF oBar;
                 ACTION (EJECUTAR("PERIODOMAS",oCSCLIRESCUO:oPeriodo:nAt,oCSCLIRESCUO:oDesde,oCSCLIRESCUO:oHasta,+1),;
                         EVAL(oCSCLIRESCUO:oBtn:bAction));
                WHEN oCSCLIRESCUO:lWhen


  @ nLin, nCol+160 BMPGET oCSCLIRESCUO:oDesde  VAR oCSCLIRESCUO:dDesde;
                PICTURE "99/99/9999";
                PIXEL;
                NAME "BITMAPS\Calendar.bmp";
                ACTION LbxDate(oCSCLIRESCUO:oDesde ,oCSCLIRESCUO:dDesde);
                SIZE 76-2,24;
                OF   oBar;
                WHEN oCSCLIRESCUO:oPeriodo:nAt=LEN(oCSCLIRESCUO:oPeriodo:aItems) .AND. oCSCLIRESCUO:lWhen ;
                FONT oFont

   oCSCLIRESCUO:oDesde:cToolTip:="F6: Calendario"

  @ nLin, nCol+252 BMPGET oCSCLIRESCUO:oHasta  VAR oCSCLIRESCUO:dHasta;
                PICTURE "99/99/9999";
                PIXEL;
                NAME "BITMAPS\Calendar.bmp";
                ACTION LbxDate(oCSCLIRESCUO:oHasta,oCSCLIRESCUO:dHasta);
                SIZE 76-2,24;
                WHEN oCSCLIRESCUO:oPeriodo:nAt=LEN(oCSCLIRESCUO:oPeriodo:aItems) .AND. oCSCLIRESCUO:lWhen ;
                OF oBar;
                FONT oFont

   oCSCLIRESCUO:oHasta:cToolTip:="F6: Calendario"

   @ nLin, nCol+345 BUTTON oCSCLIRESCUO:oBtn PROMPT " > " SIZE 27,24;
               FONT oFont;
               OF oBar;
               PIXEL;
               WHEN oCSCLIRESCUO:oPeriodo:nAt=LEN(oCSCLIRESCUO:oPeriodo:aItems);
               ACTION oCSCLIRESCUO:HACERWHERE(oCSCLIRESCUO:dDesde,oCSCLIRESCUO:dHasta,oCSCLIRESCUO:cWhere,.T.);
               WHEN oCSCLIRESCUO:lWhen

  BMPGETBTN(oBar,oFont,13)

  nLin:=32
  AEVAL(oBar:aControls,{|o|o:ForWhen(.T.),nLin:=nLin+o:nWidth()})

  IF !Empty(oCSCLIRESCUO:cCodCli) .OR. !Empty(oCSCLIRESCUO:cCodInv)

    oBar:SetSize(NIL,100+12,.T.)

    nLin:=15

    DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 BOLD

    @ 1+45+20,nLin SAY " Cliente "  OF oBar BORDER SIZE 75,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont
    @22+45+20,nLin SAY " Servicio"  OF oBar BORDER SIZE 75,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont

    @22+45+20,nLin+610-40 SAY "IVA%"+LSTR(oCSCLIRESCUO:nPorIva,6,2)  OF oBar BORDER SIZE 78,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont
    @01+45+20,nLin+610-40 SAY " Precio "                             OF oBar BORDER SIZE 78,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont

    @01+45+20,nLin+610+160 SAY "Divisa "+oDp:cMonedaExt              OF oBar BORDER SIZE 78,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont
    @22+45+20,nLin+610+160 SAY " Total "                             OF oBar BORDER SIZE 78,20 PIXEL RIGHT  COLOR oDp:nClrYellowText,oDp:nClrYellow FONT oFont


    DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 UNDERLINE BOLD
    DEFINE FONT oFont2 NAME "Tahoma"   SIZE 0, -12 BOLD


    IF !Empty(oCSCLIRESCUO:cCodCli)

      @ 01+45+20,nLin+320-240 SAYREF oCSCLIRESCUO:oCodCli PROMPT " "+oCSCLIRESCUO:cCodCli+" ";
                           OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont
    ELSE

      @ 01+45+20,nLin+320-240 SAY oCSCLIRESCUO:oCodCli PROMPT " Todos ";
                           OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont2

    ENDIF

    DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 UNDERLINE BOLD
   
    @ 22+45,nLin+320-240 SAYREF oCSCLIRESCUO:oCodInv PROMPT " "+oCSCLIRESCUO:cCodInv+" ";
                  OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont

    DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 BOLD

    @ 01+45+20,nLin+210 SAY " "+oCSCLIRESCUO:cNombre;
                     OF oBar SIZE 350,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER

    @ 22+45+20,nLin+210 SAY " "+oCSCLIRESCUO:cInvDescri;
                     OF oBar SIZE 350,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER

    @ 01+45+20,nLin+690-40 SAY oCSCLIRESCUO:oPrecio PROMPT FDP(oCSCLIRESCUO:nPrecio,"999,999,999,999.99");
                        OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER RIGHT

    @ 22+45+20,nLin+690-40 SAY oCSCLIRESCUO:oIVA  PROMPT FDP(oCSCLIRESCUO:nIVA,"999,999,999,999.99");
                        OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER RIGHT

    @ 22+45+20,nLin+690+160 SAY oCSCLIRESCUO:oTotal  PROMPT FDP(oCSCLIRESCUO:nTotal,"999,999,999,999.99");
                         OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER RIGHT

    @ 01+45+20,nLin+690+160 SAY oCSCLIRESCUO:oValCam  PROMPT FDP(oCSCLIRESCUO:nValCam,oDp:cPictValCam);
                         OF oBar SIZE 95+20,20 PIXEL COLOR oDp:nClrLabelText,oDp:nClrLabelPane FONT oFont BORDER RIGHT

    IF !Empty(oCSCLIRESCUO:cCodCli)
       SayAction(oCSCLIRESCUO:oCodCli,{||EJECUTAR("DPCLIENTES",0,oCSCLIRESCUO:cCodCli)})
    ENDIF

    IF !Empty(oCSCLIRESCUO:cCodInv)
       SayAction(oCSCLIRESCUO:oCodInv,{||EJECUTAR("DPINV",0,oCSCLIRESCUO:cCodInv)})
    ENDIF

  ENDIF

RETURN .T.

/*
// Evento para presionar CLICK
*/
FUNCTION RUNCLICK()
  LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
  LOCAL nColSel:=oCSCLIRESCUO:oBrw:nColSel

  IF aLine[12]>0 .AND. !Empty(oCSCLIRESCUO:cCodCli)
    oCSCLIRESCUO:oBrw:nColSel:=1
    EJECUTAR("XSCGMSGERR",oCSCLIRESCUO:oBrw,"Cuota de "+aLine[1]+" "+aLine[3]+" ya Existe","Cuota Generada")
    oCSCLIRESCUO:oBrw:nColSel:=nColSel 
    RETURN .F.
  ENDIF
 
  IF aLine[6]=0

    oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,14]:=!oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,14]
    oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,13]:=oCSCLIRESCUO:nTotal
    oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,05]:=oCSCLIRESCUO:nTotal
    oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,04]:=ROUND(oCSCLIRESCUO:nTotal*oCSCLIRESCUO:nValCam,2)

    oCSCLIRESCUO:oBrw:DrawLine(.T.)

    EJECUTAR("BRWCALTOTALES",oCSCLIRESCUO:oBrw,.F.)

  ENDIF

  IF .T. 

    //aLine[7]=0 .AND. aLine[14]

    oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,14]:=!aLine[14]
    //oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,13]:=0
    //oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,05]:=0
    //oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt,04]:=0
    oCSCLIRESCUO:oBrw:DrawLine(.T.)

    EJECUTAR("BRWCALTOTALES",oCSCLIRESCUO:oBrw,.F.)

  ENDIF

  AEVAL(oCSCLIRESCUO:oBrw:aArrayData,{|a,n| oCSCLIRESCUO:nCuantos:=oCSCLIRESCUO:nCuantos+IF(a[14],1,0) })

  oCSCLIRESCUO:oBtnRun:ForWhen(.T.)

RETURN .T.


/*
// Imprimir
*/
FUNCTION IMPRIMIR()
  LOCAL oRep,cWhere

  oRep:=REPORTE("BRCSCLIRESCUO",cWhere)
  oRep:cSql  :=oCSCLIRESCUO:cSql
  oRep:cTitle:=oCSCLIRESCUO:cTitle

RETURN .T.

FUNCTION LEEFECHAS()
  LOCAL nPeriodo:=oCSCLIRESCUO:oPeriodo:nAt,cWhere

  oCSCLIRESCUO:nPeriodo:=nPeriodo


  IF oCSCLIRESCUO:oPeriodo:nAt=LEN(oCSCLIRESCUO:oPeriodo:aItems)

     oCSCLIRESCUO:oDesde:ForWhen(.T.)
     oCSCLIRESCUO:oHasta:ForWhen(.T.)
     oCSCLIRESCUO:oBtn  :ForWhen(.T.)

     DPFOCUS(oCSCLIRESCUO:oDesde)

  ELSE

     oCSCLIRESCUO:aFechas:=EJECUTAR("DPDIARIOGET",nPeriodo)

     oCSCLIRESCUO:oDesde:VarPut(oCSCLIRESCUO:aFechas[1] , .T. )
     oCSCLIRESCUO:oHasta:VarPut(oCSCLIRESCUO:aFechas[2] , .T. )

     oCSCLIRESCUO:dDesde:=oCSCLIRESCUO:aFechas[1]
     oCSCLIRESCUO:dHasta:=oCSCLIRESCUO:aFechas[2]

     cWhere:=oCSCLIRESCUO:HACERWHERE(oCSCLIRESCUO:dDesde,oCSCLIRESCUO:dHasta,oCSCLIRESCUO:cWhere,.T.)

     oCSCLIRESCUO:LEERDATA(cWhere,oCSCLIRESCUO:oBrw,oCSCLIRESCUO:cServer,oCSCLIRESCUO)

  ENDIF

  oCSCLIRESCUO:SAVEPERIODO()

RETURN .T.


FUNCTION HACERWHERE(dDesde,dHasta,cWhere_,lRun)
   LOCAL cWhere:=""

   DEFAULT lRun:=.F.

   // Campo fecha no puede estar en la nueva clausula
   IF "DPDIARIO.DIA_FECHA"$cWhere
     RETURN ""
   ENDIF

   IF !Empty(dDesde)
       cWhere:=GetWhereAnd('DPDIARIO.DIA_FECHA',dDesde,dHasta)
   ELSE
     IF !Empty(dHasta)
       cWhere:=GetWhereAnd('DPDIARIO.DIA_FECHA',dDesde,dHasta)
     ENDIF
   ENDIF


   IF !Empty(cWhere_)
      cWhere:=cWhere + IIF( Empty(cWhere),""," AND ") +cWhere_
   ENDIF

   IF lRun

     IF !Empty(oCSCLIRESCUO:cWhereQry)
       cWhere:=cWhere + oCSCLIRESCUO:cWhereQry
     ENDIF

     oCSCLIRESCUO:LEERDATA(cWhere,oCSCLIRESCUO:oBrw,oCSCLIRESCUO:cServer,oCSCLIRESCUO)

   ENDIF


RETURN cWhere


FUNCTION LEERDATA(cWhere,oBrw,cServer,oFrm,cCodCli,cCodInv,cTipDoc)
   LOCAL aData:={},aTotal:={},oCol,cSql,aLines:={}
   LOCAL oDb
   LOCAL nAt,nRowSel,cWhereCli:="",cInnerMov:=""
   LOCAL cDocCli,cDocFav


   DEFAULT cWhere :="",;
           cTipDoc:=oDp:cTipDocClb

   IF ValType(oFrm)="O"
      cCodCli:=oFrm:cCodCli
      cTipDoc:=oFrm:cTipDoc
   ENDIF

   IF !Empty(cCodCli) .AND. ISSQLFIND("DPCLIENTES","CLI_CODIGO"+GetWhere("=",cCodCli))
      cWhereCli:="DOC_CODIGO"+GetWhere("=",cCodCli)
   ENDIF

   IF !Empty(cCodInv)

      cInnerMov:=" LEFT JOIN DPMOVINV ON DOC_CODSUC=MOV_CODSUC AND DOC_TIPDOC=MOV_TIPDOC AND DOC_NUMERO=MOV_DOCUME "+;
                 " AND MOV_APLORG"+GetWhere("=","V")

      IF ISSQLFIND("DPINV","INV_CODIGO"+GetWhere("=",cCodInv))
         cInnerMov:=cInnerMov+" AND MOV_CODIGO"+GetWhere("=",cCodInv)
      ENDIF

   ENDIF

   IF !Empty(cServer)

     IF !EJECUTAR("DPSERVERDBOPEN",cServer)
        RETURN .F.
     ENDIF

     oDb:=oDp:oDb

   ENDIF

   cWhere:=IIF(Empty(cWhere),"",ALLTRIM(cWhere))

   IF !Empty(cWhere) .AND. LEFT(cWhere,5)="WHERE"
      cWhere:=SUBS(cWhere,6,LEN(cWhere))
   ENDIF

   cDocCli:=" FROM DPDOCCLI  WHERE DOC_CODSUC"+GetWhere("=",oDp:cSucursal)+" AND DOC_TIPDOC"+GetWhere("=",cTipDoc)+" AND DOC_TIPTRA='D' AND YEAR(DIA_FECHA)=YEAR(DOC_FECHA) AND MONTH(DIA_FECHA)=MONTH(DOC_FECHA) AND DOC_ACT=1)"
   cDocFav:=" FROM DPDOCCLI  WHERE DOC_CODSUC"+GetWhere("=",oDp:cSucursal)+" AND DOC_TIPAFE"+GetWhere("=",cTipDoc)+" AND DOC_TIPTRA='D' AND YEAR(DIA_FECHA)=YEAR(DOC_FECHA) AND MONTH(DIA_FECHA)=MONTH(DOC_FECHA) AND DOC_ACT=1)"


// "  LEFT JOIN DPDOCCLI               ON DOC_TIPDOC"+GetWhere("=",cTipDoc)+" AND DIA_FECHA=DOC_FECHA AND DOC_TIPTRA='D' "+IF(Empty(cWhereCli),""," AND "+cWhereCli)+;


   cSql:=" SELECT "+;
          "  DIA_ANO, "+;
          "  DIA_MES, "+;
          "  DIA_CMES,"+;
          "  (SELECT SUM(DOC_NETO)            "+cDocCli+" AS MTOCUO, "+CRLF+;
          "  (SELECT SUM(DOC_NETO/DOC_VALCAM) "+cDocCli+" AS MTODIV, "+CRLF+;
          "  (SELECT COUNT(*)        "+cDocCli+" AS  CUANTOSCUO     ,"+CRLF+;
          "  (SELECT SUM(ROUND(DOC_NETO/DOC_VALCAM,2)) "+cDocFav+" AS MTOFAVDIV,"+CRLF+;
          "  0 AS XFACTURAS,"+CRLF+;
          "  SUM(PAG_NETO) AS MTOPAG, "+;
          "  SUM(PAG_NETO/PAG_VALCAM) AS MTOPAGDIV, "+;
          "  (SELECT COUNT(*) "+cDocFav+" AS CUANTOSFAV, "+;
          "  SUM(IF(DOC_NUMERO IS NULL,0,1))-SUM(IF(DES_NUMERO IS NULL,0,1)) AS XFACTURAR,"+CRLF+;
          "  0 AS XCOBRAR,"+;
          "  0 AS LOGICO "+;
          "  FROM DPDIARIO "+;
          "  LEFT JOIN DPDOCCLI               ON DOC_CODSUC"+GetWhere("=",oDp:cSucursal)+" AND DOC_TIPDOC"+GetWhere("=",cTipDoc)+" AND DOC_TIPTRA='D' AND YEAR(DIA_FECHA)=YEAR(DOC_FECHA) AND MONTH(DIA_FECHA)=MONTH(DOC_FECHA)  "+;
          IF(Empty(cWhereCli),""," AND "+cWhereCli)+;
          cInnerMov+;
          "  LEFT JOIN VIEW_DPDOCCLITIPAFEDES ON DOC_CODSUC=DES_CODSUC AND DOC_TIPAFE=DES_TIPDOC AND DOC_NUMERO=DES_NUMERO "+CRLF+;
          "  LEFT JOIN VIEW_DPDOCCLIPAGDET    ON DES_CODSUC=PAG_CODSUC AND DES_TIPORG=PAG_TIPDOC AND DES_NUMERO=PAG_NUMERO "+CRLF+;
          "  WHERE 1=1 "+;
          "  GROUP BY DIA_ANO,DIA_MES "+;
          "  ORDER BY DIA_ANO,DIA_MES "


/*
   IF Empty(cWhere)
     cSql:=STRTRAN(cSql,"<WHERE>","")
   ELSE
     cSql:=STRTRAN(cSql,"<WHERE>"," WHERE "+cWhere)
   ENDIF
*/
   IF !Empty(cWhere)
      cSql:=EJECUTAR("SQLINSERTWHERE",cSql,cWhere)
   ENDIF

   cSql:=EJECUTAR("WHERE_VAR",cSql)

//? CLPCOPY(cSql)
//RETURN {}

   oDp:lExcluye:=.T.

   DPWRITE("TEMP\BRCSCLIRESCUO.SQL",cSql)

   aData:=ASQL(cSql,oDb)

   oDp:cWhere:=cWhere

   IF EMPTY(aData)
      aData:=EJECUTAR("SQLARRAYEMPTY",cSql,oDb)
   ENDIF

   AEVAL(aData,{|a,n|aData[n,12]:=a[6]-a[11],;
                     aData[n,08]:=a[5]-a[07],;
                     aData[n,13]:=IF(a[5]>0,a[5],0)})

   IF ValType(oBrw)="O"

      oCSCLIRESCUO:cSql   :=cSql
      oCSCLIRESCUO:cWhere_:=cWhere

      aTotal:=ATOTALES(aData)

      oBrw:aArrayData:=ACLONE(aData)
      // oBrw:nArrayAt  :=1
      // oBrw:nRowSel   :=1

      // JN 15/03/2020 Sustituido por BRWCALTOTALES
      EJECUTAR("BRWCALTOTALES",oBrw,.F.)

      nAt    :=oBrw:nArrayAt
      nRowSel:=oBrw:nRowSel

      oBrw:Refresh(.F.)
      oBrw:nArrayAt  :=MIN(nAt,LEN(aData))
      oBrw:nRowSel   :=MIN(nRowSel,oBrw:nRowSel)
      AEVAL(oCSCLIRESCUO:oBar:aControls,{|o,n| o:ForWhen(.T.)})

      oCSCLIRESCUO:SAVEPERIODO()

   ENDIF

RETURN aData


FUNCTION SAVEPERIODO()
  LOCAL cFileMem:="USER\BRCSCLIRESCUO.MEM",V_nPeriodo:=oCSCLIRESCUO:nPeriodo
  LOCAL V_dDesde:=oCSCLIRESCUO:dDesde
  LOCAL V_dHasta:=oCSCLIRESCUO:dHasta

  SAVE TO (cFileMem) ALL LIKE "V_*"

RETURN .T.

/*
// Permite Crear Filtros para las Búquedas
*/
FUNCTION BRWQUERY()
     EJECUTAR("BRWQUERY",oCSCLIRESCUO)
RETURN .T.

/*
// Ejecución Cambio de Linea
*/
FUNCTION BRWCHANGE()
RETURN NIL

/*
// Refrescar Browse
*/
FUNCTION BRWREFRESCAR()
    LOCAL cWhere


    IF Type("oCSCLIRESCUO")="O" .AND. oCSCLIRESCUO:oWnd:hWnd>0

      cWhere:=" "+IIF(!Empty(oCSCLIRESCUO:cWhere_),oCSCLIRESCUO:cWhere_,oCSCLIRESCUO:cWhere)
      cWhere:=STRTRAN(cWhere," WHERE ","")

      oCSCLIRESCUO:LEERDATA(oCSCLIRESCUO:cWhere_,oCSCLIRESCUO:oBrw,oCSCLIRESCUO:cServer)
      oCSCLIRESCUO:oWnd:Show()
      oCSCLIRESCUO:oWnd:Restore()

    ENDIF

RETURN NIL

FUNCTION BTNRUN()
    ? "PERSONALIZA FUNCTION DE BTNRUN"
RETURN .T.

FUNCTION BTNMENU(nOption,cOption)

   ? nOption,cOption,"PESONALIZA LAS SUB-OPCIONES"

   IF nOption=1
   ENDIF

   IF nOption=2
   ENDIF

   IF nOption=3
   ENDIF

RETURN .T.

FUNCTION HTMLHEAD()

   oCSCLIRESCUO:aHead:=EJECUTAR("HTMLHEAD",oCSCLIRESCUO)

// Ejemplo para Agregar mas Parámetros
//   AADD(oDOCPROISLR:aHead,{"Consulta",oDOCPROISLR:oWnd:cTitle})

RETURN

// Restaurar Parametros
FUNCTION BRWRESTOREPAR()
  EJECUTAR("BRWRESTOREPAR",oCSCLIRESCUO)
RETURN .T.

FUNCTION CREARCUOTAS()
   LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
   LOCAL dDesde :=CTOD("01/"+CTOO(aLine[2],"C")+"/"+CTOO(aLine[1],"C"))
   LOCAL dHasta :=FCHFINMES(dDesde)
   LOCAL cWhere :=NIL,cCodSuc:=oDp:cSucursal,nPeriodo:=4,cTitle:=""
   LOCAL aData  :=ACLONE(oCSCLIRESCUO:oBrw:aArrayData),I,cWhere:="",dFecha,aFechas:={}
   LOCAL lOk    :=.F.
   LOCAL lDelete:=.T.
   LOCAL lAsk   :=.T.
   LOCAL cCodInv:=""

   ADEPURA(aData,{|a,n| !a[14]})

   oCSCLIRESCUO:nCuantos:=LEN(aData)

   IF oCSCLIRESCUO:nCuantos=0
      
   ENDIF

   IF LEN(aData)>0

     FOR I=1 TO LEN(aData)
       dFecha:=CTOD("01/"+CTOO(aData[I,2],"C")+"/"+CTOO(aData[I,1],"C"))
       AADD(aFechas,dFecha)
     NEXT I

     cWhere:=" 1=1 "

     IF oCSCLIRESCUO:lFindInv

        cCodInv:=oCSCLIRESCUO:cCodInv
        cWhere :="DPG_CODINV"+GetWhere("=",oCSCLIRESCUO:cCodInv)

     ELSE

        cCodInv:=SQLGET("DPCLIENTEPROG","DPG_CODINV","DPG_TIPDES"+GetWhere("=",oCSCLIRESCUO:cTipDoc)+" AND "+;
                                                     "DPG_CODIGO"+GetWhere("=",oCSCLIRESCUO:cCodCli))
     ENDIF

     IF !Empty(oCSCLIRESCUO:cCodCli)

        cWhere:=cWhere+IF(Empty(cWhere),""," AND ")+;
                "DPG_CODIGO"+GetWhere("=",oCSCLIRESCUO:cCodCli)

     ENDIF

     lOk   := EJECUTAR("CLBGENCUOTAS",cWhere,FCHINIMES(dFecha),FCHFINMES(dFecha),cCodInv,oCSCLIRESCUO:cCodCli,lDelete,lAsk,aFechas,oCSCLIRESCUO:nValCam,NIL,oCSCLIRESCUO:cTipDoc)

     IF "TMDI"$oCSCLIRESCUO:oFrmMain:ClassName() .AND. oCSCLIRESCUO:oFrmMain:oWnd:hWnd>0
        oCSCLIRESCUO:Close()
        oCSCLIRESCUO:oFrmMain:RELOADDOCS()
        RETURN .T.
     ENDIF

     oCSCLIRESCUO:BRWREFRESCAR()

     // EJECUTAR("BRCXC")

     RETURN .T.

   ENDIF


RETURN EJECUTAR("BRCLBAFILIACION",cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle,oCSCLIRESCUO:cCodCli)

FUNCTION VERBRCXC()
   LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
   LOCAL dDesde :=CTOD("01/"+CTOO(aLine[2],"C")+"/"+CTOO(aLine[1],"C"))
   LOCAL dHasta :=FCHFINMES(dDesde)
   LOCAL cWhere :=NIL,cCodSuc:=oDp:cSucursal,nPeriodo:=4,cTitle:="",cCodigo:=NIL

RETURN EJECUTAR("BRDOCCXCDET",cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle)

FUNCTION VERBRDOC()
   LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
   LOCAL dDesde :=CTOD("01/"+CTOO(aLine[2],"C")+"/"+CTOO(aLine[1],"C"))
   LOCAL dHasta :=FCHFINMES(dDesde)
   LOCAL cWhere :=NIL,cCodSuc:=oDp:cSucursal,nPeriodo:=4,cTitle:="",cCodigo:=NIL

   cWhere:="DOC_CODSUC"+GetWhere("=",oCSCLIRESCUO:cCodSuc)+" AND "+;
           "DOC_TIPDOC"+GetWhere("=",oCSCLIRESCUO:cTipDoc)
// cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle,cTipDoc,cCodMot
RETURN EJECUTAR("BRDPDOCCLICXC",cWhere,NIL,oDp:nMensual,dDesde,dHasta,NIL,oCSCLIRESCUO:cTipDoc)

FUNCTION VERFACTURAS()
   LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
   LOCAL dDesde :=CTOD("01/"+CTOO(aLine[2],"C")+"/"+CTOO(aLine[1],"C"))
   LOCAL dHasta :=FCHFINMES(dDesde)
   LOCAL cWhere :=NIL,cCodSuc:=oDp:cSucursal,nPeriodo:=4,cTitle:="",cCodigo:=NIL

   cWhere:="DOC_CODSUC"+GetWhere("=",oCSCLIRESCUO:cCodSuc)+" AND "+;
           "DOC_TIPDOC"+GetWhere("=","FAV")

RETURN EJECUTAR("BRDPDOCCLICXC",cWhere,NIL,oDp:nMensual,dDesde,dHasta,NIL,"FAV")



FUNCTION NOTASDECONSUMO()
  LOCAL aLine  :=oCSCLIRESCUO:oBrw:aArrayData[oCSCLIRESCUO:oBrw:nArrayAt]
  LOCAL dDesde :=CTOD("01/"+CTOO(aLine[2],"C")+"/"+CTOO(aLine[1],"C"))
  LOCAL dHasta :=FCHFINMES(dDesde)
  LOCAL cWhere :=NIL,cCodSuc:=oDp:cSucursal,nPeriodo:=4,cTitle:="",cCodCli:=NIL,lDirecto:=.T.

EJECUTAR("BRCLBCONSUMOS",cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle,cCodCli,lDirecto)

// EOF

