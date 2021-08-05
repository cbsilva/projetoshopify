/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        LIB/ComunicacaoAPI.p
 *  OBJETIVO        
 *  AUTOR           TOTVS - LASF
 *  DATA            04/2021
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINI€åES
 *
 *------------------------------------------------------------------------------
 */

using Progress.Json.ObjectModel.JsonObject.
using Progress.Json.ObjectModel.JsonArray.
DEF VAR lc-json-enviado AS LONGCHAR  NO-UNDO.
DEF VAR JsonString   AS LONGCHAR                      NO-UNDO.
DEF VAR cResposta AS c NO-UNDO.
DEF VAR iErro     AS i NO-UNDO.

/* ------ Vari˜veis ------ */
DEF VAR httpUrl as character no-undo.
DEF VAR cToken  AS CHARACTER NO-UNDO.
DEF VAR client  AS COM-HANDLE.

{lib/rowErrors.i}
{lib/utilidades.i}
{lib/mensagens2.i}


/*DEFINE DATASET dsItem FOR ttItem.
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.*/

DEFINE VARIABLE perro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pchave AS CHARACTER   NO-UNDO.

/*
 *------------------------------------------------------------------------------
 *
 *                                BLOCO PRINCIPAL
 *
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                PROCEDURES
 *
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *------------------------------------------------------------------------------
 */
PROCEDURE RealizarPost:
DEFINE INPUT  PARAMETER pRowTipoIntegr          AS ROWID       NO-UNDO.
//DEFINE INPUT  PARAMETER pJSON                   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pJSON                   AS JsonObject  NO-UNDO.
DEFINE OUTPUT PARAMETER pErro                   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pResposta               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cJSON   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-porta AS CHARACTER   NO-UNDO.

    FOR FIRST es-api-param NO-LOCK WHERE ROWID(es-api-param)        = pRowTipoIntegr :
    END.
    IF NOT AVAIL es-api-param THEN DO:
       pErro =  "Token nÆo encontrado!".
       RETURN "NOK".
    END.

    cToken  = es-api-param.val-token.

    IF cToken = "" OR cToken = ? THEN DO:
        MESSAGE "Token nÆo encontrado!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        pErro =  "Token nÆo encontrado!".
        RETURN "NOK".
    END.
    
    IF es-api-param.porta-integr = 0 THEN
        ASSIGN c-porta = "".
    ELSE
        ASSIGN c-porta = ":" + string(es-api-param.porta-integr).

    ASSIGN httpUrl = es-api-param.host-integr + c-porta + es-api-param.path-integr.
    
    CREATE "MSXML2.XMLHTTP" client.
    
    client:OPEN("post", httpUrl, FALSE).
    client:SetRequestHeader ("Content-Type", "application/json").
    client:SetRequestHeader ("Authorization","Bearer " + cToken).
    pJSON:WRITE(cJSON, TRUE  ).

    

    client:Send(cJSON).

    INDEX(JsonString,'"Status":false') > 0 NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND index(JsonString,'"Status":false') > 0 THEN 
    DO:
        IF LOOKUP("Description",client:ResponseText,'"') > 0 THEN 
             ASSIGN  pErro = SUBSTRING(ENTRY(LOOKUP("Description",client:ResponseText,'"') + 2,
                             client:ResponseText,'"'),1,5000).
    END.
    ASSIGN cResposta = client:ResponseText.
     
    RELEASE OBJECT client.
    
    ASSIGN pResposta    = cResposta.    

    RETURN "OK".
    
END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *
 *------------------------------------------------------------------------------
 */
PROCEDURE criarObjetoJSON:
    
    /* ------- Defini‡Æo de Parƒmetros ----- */
    DEFINE INPUT  PARAM pTable         AS HANDLE     NO-UNDO.  
    DEFINE OUTPUT PARAM pJsonObjAux    AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAM pJsonArrayAux  AS JsonArray  NO-UNDO.
    DEFINE INPUT  PARAM lArray         AS LOGICAL    NO-UNDO.
    
    /* ------- Defini‡Æo de Vari veis ----- */
    DEFINE VARIABLE i_reg         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i_fld         AS INTEGER    NO-UNDO.   
    DEFINE VARIABLE qh            AS HANDLE     NO-UNDO.

    CREATE query qh.
    qh:SET-BUFFERS(pTable).
    qh:QUERY-PREPARE("for each " + pTable:NAME).
    qh:QUERY-OPEN(). 

    repeat:

        qh:get-next().  
        IF qh:QUERY-OFF-END THEN LEAVE.

        i_reg = i_reg + 1.

        pJsonObjAux = NEW JsonObject().

        DO i_fld = 1 TO pTable:NUM-FIELDS:

            IF pTable:BUFFER-FIELD(i_fld):NAME = "fld-rel" THEN NEXT.

            IF pTable:BUFFER-FIELD(i_fld):EXTENT > 0 THEN
                pJsonObjAux:ADD(pTable:BUFFER-FIELD(i_fld):SERIALIZE-NAME,pTable:BUFFER-FIELD(i_fld):BUFFER-VALUE(1)).                
            ELSE
                pJsonObjAux:ADD(pTable:BUFFER-FIELD(i_fld):SERIALIZE-NAME,pTable:BUFFER-FIELD(i_fld):BUFFER-VALUE()).                
        END.

        IF lArray THEN DO:

            IF i_reg = 1 THEN
                pJsonArrayAux = NEW JsonArray().

            pJsonArrayAux:ADD(pJsonObjAux).
        END.

    END.
    qh:QUERY-CLOSE().

    delete object pTable.
    delete object qh.     


END PROCEDURE.
