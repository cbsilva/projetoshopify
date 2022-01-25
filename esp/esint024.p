
/*----------------------------------------------------------------------------------------------
    File        : esint024.p
    Purpose     : Interface de Integra‡Æo TOTVS x Shopify
    Description : Envio de informacoes da nota para shopify
    Author(s)   : 4Make Consultoria
    Created     : 18.10.2021
    Notes       :
-----------------------------------------------------------------------------------------------*/

/******************************* Definitions **************************************************/


/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

/* --------------------------------------------------------------------------------------------
    Define input param
----------------------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.


/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE oJsonArrayMain   AS JsonArray   NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject  NO-UNDO.
DEFINE VARIABLE oJsonObjMain     AS JsonObject  NO-UNDO.
DEFINE VARIABLE oJsonObjIni      AS jsonObject  NO-UNDO.
DEFINE VARIABLE ojsonArrayIni    AS JsonArray   NO-UNDO.
DEFINE VARIABLE hAPI             AS HANDLE      NO-UNDO.
DEFINE VARIABLE hTempNota        AS HANDLE      NO-UNDO.
DEFINE VARIABLE cJsonBody        AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE cRetorno         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lResp            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cArqLog          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAmbiente        AS CHARACTER 
    EXTENT 2 INITIAL ["PROD", "TEST"] NO-UNDO.

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{METHOD/dbotterr.i} //RowErrors

DEFINE TEMP-TABLE ttNota NO-UNDO
    FIELD orderId       AS CHARACTER SERIALIZE-NAME "orderid"
    FIELD storeName     AS CHARACTER SERIALIZE-NAME "store_name"
    FIELD totvsInvoice  AS CHARACTER SERIALIZE-NAME "totvs_invoice_id".

/* DEFINE TEMP-TABLE tt-erro NO-UNDO  */
/*       FIELD cd-erro AS INTEGER     */
/*       FIELD mensagem AS CHARACTER. */

/* --------------------------------------------------------------------------------------------
    Functions
----------------------------------------------------------------------------------------------*/


/******************************* Main Block **************************************************/

LOG-MANAGER:WRITE-MESSAGE("esint024 - PROGRAMA RETORNO INFO FATURAMENTO") NO-ERROR.

/*-- instancia hApi de comunicao --*/
IF NOT VALID-HANDLE(hAPI) THEN
DO :
    RUN esp/esintapi200.p PERSISTENT SET hAPI.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        RUN pi-deleta-objetos.
        RETURN "NOK":U.
    END.
END.

/*-- cria objeto principal oJsonObjMain --*/
RUN piGeraObjJson IN hAPI (OUTPUT oJsonObjMain).
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RUN pi-deleta-objetos.
    RETURN "NOK":U.
END.

FOR FIRST es-api-export WHERE ROWID(es-api-export) = r-table NO-LOCK: END.
IF NOT AVAIL es-api-export THEN
DO:
    ASSIGN c-erro = "Registro Tabela de Nota Fiscal nÆo localizado. ".
    RUN pi-deleta-objetos.
    RETURN "NOK":U.
END.

FOR FIRST es-api-param NO-LOCK
    WHERE es-api-param.ind-tipo-trans = 2 /*exportacao*/
      AND es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr: 
END.
IF NOT AVAIL es-api-param THEN
DO:
    ASSIGN c-erro = SUBSTITUTE("Tipo de Integra‡Æo &1 nÆo Encontrada", es-api-export.cd-tipo-integr).
    RETURN "NOK":U.
END.

FIND FIRST es-api-token-param NO-LOCK WHERE es-api-token-param.cod-sistema = es-api-param.cd-sistema NO-ERROR.
IF NOT AVAIL es-api-token-param THEN
DO:
    ASSIGN c-erro = "TI - Informa‡äes de TOKEN nÆo parametrizadas no programa esint300. ".
    RETURN "NOK":U.
END.

RUN piGravaTTNotas (OUTPUT hTempNota, OUTPUT c-erro).
IF VALID-HANDLE(hTempNota) THEN 
DO:

    /* ------ Adiciona Array -----*/
    RUN piCriaObj IN hAPI (INPUT hTempNota,
                           INPUT NO, /*RETORNA ARRAY DE OBJETOS*/ 
                           OUTPUT oJsonObjMain,
                           OUTPUT ojsonArrayIni) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        RUN pi-deleta-objetos.
        RETURN "NOK":U.
    END.

    DELETE OBJECT hTempNota.
END.


/* ----- Cria Json Principal ------- */
oJsonArrayMain = NEW JsonArray().
oJsonArrayMain:ADD(ojsonObjIni). 

/* ------ Grava conteudo do Json em variavel -----*/
RUN piGeraVarJson IN hAPI (INPUT oJsonObjMain,OUTPUT cJsonBody) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RUN pi-deleta-objetos.
    RETURN "NOK":U.
END.

/* ------ Grava conteudo do Json no monitor de integracao -----*/
FIND CURRENT es-api-export EXCLUSIVE-LOCK NO-ERROR.
ASSIGN es-api-export.c-json = cJsonBody.
FIND CURRENT es-api-export NO-LOCK NO-ERROR.


/* ------------ Envia Objeto Json --------- */
RUN piPostJsonObj IN hAPI (INPUT oJsonObjMain, INPUT ROWID(es-api-param),
                           OUTPUT TABLE RowErrors). 


IF TEMP-TABLE RowErrors:HAS-RECORDS THEN 
DO:
    FOR EACH RowErrors NO-LOCK:
        IF c-erro = "" THEN
            ASSIGN c-erro = string(RowErrors.ErrorNumber)  + " - " + RowErrors.ErrorDescription.
        ELSE 
            ASSIGN c-erro = c-erro + " | " + string(RowErrors.ErrorNumber)  + " - " + RowErrors.ErrorDescription.
    END.
    RUN pi-deleta-objetos.    
    RETURN "NOK":U.
END.




/* --------------------------------------------------------------------------------------------
    Procedures
----------------------------------------------------------------------------------------------*/

PROCEDURE piGravaTTNotas:
    DEFINE OUTPUT PARAM pTempNota AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAM pErro     AS CHARACTER NO-UNDO.

    FOR FIRST es-ped-venda NO-LOCK
        WHERE es-ped-venda.nr-shopify = es-api-export.chave ,
        FIRST ped-venda NO-LOCK
        WHERE ped-venda.nome-abrev = es-ped-venda.nome-abrev
          AND ped-venda.nr-pedcli  = es-ped-venda.nr-pedcli:


        FIND FIRST nota-fiscal
             WHERE nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
               AND nota-fiscal.nome-ab-cli = ped-venda.nome-abrev NO-LOCK NO-ERROR.
        IF NOT AVAIL nota-fiscal THEN
        DO:
            ASSIGN pErro = SUBSTITUTE("NÆo Encontado Nota Fiscal, para Pedido de Venda &1/&2",ped-venda.nome-abrev,ped-venda.nr-pedcli).
            RETURN "NOK":U.
        END.

        CREATE ttNota.
        ASSIGN ttNota.orderid      = es-ped-venda.nr-shopify
               ttNota.storeName    = cAmbiente[es-api-token-param.ind-tip-ambiente]
               ttNota.totvsInvoice = nota-fiscal.nr-nota-fis.

    END.

    IF TEMP-TABLE ttNota:HAS-RECORDS THEN
        pTempNota = BUFFER ttNota:HANDLE.

END PROCEDURE.


PROCEDURE pi-deleta-objetos:

    IF VALID-HANDLE(hAPI) THEN DO:
        DELETE OBJECT hAPI.
        ASSIGN hAPI = ?.
    END.  
    IF VALID-OBJECT(oJsonArrayMain) THEN 
        DELETE OBJECT oJsonArrayMain  NO-ERROR.


    LOG-MANAGER:WRITE-MESSAGE(">>>Imprimindo variavel de erro"  + c-erro) NO-ERROR.
    
END PROCEDURE.

