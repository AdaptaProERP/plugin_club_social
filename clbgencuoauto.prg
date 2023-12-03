// Programa   : CLBGENCUOAUTO
// Fecha/Hora : 03/12/2023 05:58:59
// Propósito  : Generación automática de Cuotas
// Creado Por : Juan Navas
// Llamado por: Proceso Automático
// Aplicación :
// Tabla      :

#INCLUDE "DPXBASE.CH"

PROCE MAIN(cTipDoc,dFecha,cCodCli,nValCam)
 LOCAL cSql,oTable,cWhere,nCant,cCodInv,cCodInv,lDelete:=.F.,lAsk:=.F.,aFechas:={},dViernes

 DEFAULT cTipDoc:="CUO",;
         dFecha :=oDp:dFecha,;
         cCodCli:=""

 dViernes:=dFecha

 IF DOW(dViernes)=1 // Domingo
    dViernes-- // Sabado
 ENDIF

 IF DOW(dViernes)=7 // Sábado
    dViernes-- // Viernes
 ENDIF

 DEFAULT nValCam:=SQLGET("DPHISMON","HMN_VALOR","HMN_CODIGO"+GetWhere("=",oDp:cMonedaExt)+" AND HMN_FECHA"+GetWhere("=",dViernes))

 IF nValCam=0

    EJECUTAR("GETURLDIV_BCV")

    nValCam:=SQLGET("DPHISMON","HMN_VALOR","HMN_CODIGO"+GetWhere("=",oDp:cMonedaExt)+" AND HMN_FECHA"+GetWhere("=",dViernes))

 ENDIF

 cSql:=[ SELECT    DPG_CODINV,PRE_UNDMED,PRE_CODMON,PRE_PRECIO,   COUNT(*) AS CUANTOS   FROM DPCLIENTEPROG ]+;
       [ INNER JOIN DPINV             ON DPG_CODINV=INV_CODIGO ]+;
       [ INNER JOIN DPCLIENTES        ON DPG_CODIGO=CLI_CODIGO AND LEFT(CLI_SITUAC,1)='A'  ]+;
       [ LEFT  JOIN VIEW_UNDMEDXINV   ON INV_CODIGO=IME_CODIGO  ]+;
       [ LEFT  JOIN VIEW_DPINVPRECIOS ON DPINV.INV_CODIGO=PRE_CODIGO ]+;
       [ WHERE DPG_TIPDES]+GetWhere("=",cTipDoc)+[ AND INV_UTILIZ='Afiliación' ]+;
       [ GROUP BY DPG_CODINV ]

  oTable:=OpenTable(cSql,.t.)

  WHILE !oTable:Eof()
 
     cWhere :=""
     cCodInv:=oTable:DPG_CODINV

     IF UPPER(oTable:PRE_UNDMED)="ANUAL"
        cWhere:="MOV_CODSUC"+GetWhere("=",oDp:cSucursal)+" AND MOV_TIPDOC"+GetWhere("=",cTipDoc)+" AND YEAR(MOV_FECHA)"+GetWhere("=",YEAR(dFecha))+" AND MOV_INVACT=1"
     ENDIF

     IF UPPER(oTable:PRE_UNDMED)="MENSUAL"
        cWhere:="MOV_CODSUC"+GetWhere("=",oDp:cSucursal)+" AND MOV_TIPDOC"+GetWhere("=",cTipDoc)+" AND YEAR(MOV_FECHA)"+GetWhere("=",YEAR(dFecha))+" AND MONTH(MOV_FECHA)"+GetWhere("=",MONTH(dFecha))+" AND MOV_INVACT=1"
     ENDIF

     IF !Empty(cWhere)
       cWhere:=cWhere+" AND MOV_CODIGO"+GetWhere("=",cCodInv)
       nCant :=COUNT("DPMOVINV",cWhere)
     ENDIF

     IF !Empty(cWhere) .AND. nCant=0
        cWhere :=" 1=1 "
        aFechas:={dFecha}
        EJECUTAR("CLBGENCUOTAS",cWhere,FCHINIMES(dFecha),FCHFINMES(dFecha),cCodInv,cCodCli,lDelete,lAsk,aFechas,nValCam,NIL,cTipDoc)
     ENDIF

     oTable:DbSkip()

  ENDDO

  oTable:End()

  // valida las penalizaciones
   EJECUTAR("CLBAPLPENALIZA")


RETURN .T.
// EOF
