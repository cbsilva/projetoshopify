
/*----------------------------------------------------------------------------------------------
    File        : ESSPFAPI200.p
    Purpose     : API de Comunica‡Æo para as Integra‡Æo TOTVS x Shopify
    Description : Envio de informacoes da nota para shopify
    Author(s)   : 4Make Consultoria
    Created     : 24.10.2021
    Notes       :
-----------------------------------------------------------------------------------------------*/
/******************************* Definitions **************************************************/

/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
using OpenEdge.Core.String.
using OpenEdge.Net.HTTP.IHttpClientLibrary.
using OpenEdge.Net.HTTP.ConfigBuilder.
using OpenEdge.Net.HTTP.ClientBuilder.
using OpenEdge.Net.HTTP.Credentials.
using OpenEdge.Net.HTTP.IHttpClient.
using OpenEdge.Net.HTTP.IHttpRequest.
using OpenEdge.Net.HTTP.RequestBuilder.
using OpenEdge.Net.HTTP.ResponseBuilder.
USING OpenEdge.Net.HTTP.lib.ClientLibraryBuilder.
using OpenEdge.Net.HTTP.IHttpResponse.
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.
USING Progress.Lang.*. 
USING Progress.Json.ObjectModel.*. 

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{METHOD/dbotterr.i} //RowErrors


/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cResposta AS CHARACTER NO-UNDO.
DEFINE VARIABLE iErro     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cHttpUrl  AS CHARACTER NO-UNDO.



PROCEDURE piGeraObjJson:
/*-----------------------------------------------------------
  Prop¢sito: Instancia Objeto JsonOBJect
------------------------------------------------------------*/

    DEFINE OUTPUT PARAM pJsonObjMaster AS JsonObject NO-UNDO.

    /* --------- Cria Objeto Principal ------ */
    pJsonObjMaster = NEW JsonObject().

END PROCEDURE.



PROCEDURE piCriaObj:
/*-----------------------------------------------------------
  Prop¢sito: Adiciona elementos ao objeto JsonOBJect
------------------------------------------------------------*/
    
    /* ------- Defini‡Æo de Parƒmetros ----- */
    DEFINE INPUT  PARAM pTable         AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAM lArray         AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAM pJsonObjAux    AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAM pJsonArrayAux  AS JsonArray  NO-UNDO.
   
    
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


PROCEDURE piGeraArqJson:
/*-----------------------------------------------------------
  Prop¢sito: Gera um arquivo .json do Objeto criado
 ------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pJsonObjMaster AS JsonObject NO-UNDO.
    DEFINE INPUT PARAMETER pCaminho       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER pTipoIntegr    AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER pJsonFile     AS CHARACTER  NO-UNDO.

    ASSIGN pJsonFile = pCaminho + "spf-" + pTipoIntegr + "-" + STRING(TIME) + STRING(RANDOM(100000,200000)) + ".json".

    pJsonObjMaster:WriteFile(pJsonFile, TRUE).

END.

PROCEDURE piGeraVarJson:
/*-----------------------------------------------------------
  Prop¢sito: Gera um arquivo .json do Objeto criado
 ------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pJsonObjMaster AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER pJson         AS LONGCHAR   NO-UNDO.

    pJsonObjMaster:WRITE(INPUT-OUTPUT pJson, TRUE).

END.
   
PROCEDURE piErro:
/*-----------------------------------------------------------
  Prop¢sito: Gera temp-table de erro
 ------------------------------------------------------------*/
    DEF INPUT PARAM cErrorDescription AS CHARACTER NO-UNDO.
    DEF INPUT PARAM cErrorHelp        AS CHARACTER NO-UNDO.

    CREATE RowErrors.
    ASSIGN iErro            = iErro + 1
           ErrorSequence    = iErro
           ErrorNumber      = 17006
           ErrorType        = "Error"
           ErrorDescription = cErrorDescription
           ErrorHelp        = cErrorHelp       .
END.

PROCEDURE piConvLongObj:
/*-----------------------------------------------------------
  Prop¢sito: Converte LongChar para Object
 ------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pJsonString AS LONGCHAR          NO-UNDO. 
    DEFINE OUTPUT PARAMETER ojson       AS JsonObject        NO-UNDO. 
    
    DEFINE VARIABLE myParser AS ObjectModelParser NO-UNDO. 
    
    myParser = NEW ObjectModelParser(). 
    ojson = CAST(myParser:Parse(pJsonString ),JsonObject).

END PROCEDURE.


PROCEDURE piPostJsonObj:
/*-----------------------------------------------------------
  Prop¢sito: M‚todo para envio de conteudo json 
 ------------------------------------------------------------*/
    DEFINE INPUT  PARAM oInputData     AS JsonObject NO-UNDO.
    DEFINE INPUT PARAM pRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAM TABLE FOR RowErrors.
    
    DEFINE VARIABLE oRequest         as IHttpRequest                         NO-UNDO.
    DEFINE VARIABLE oResponse        as IHttpResponse                        NO-UNDO.
    DEFINE VARIABLE oHttpClient      AS OpenEdge.Net.HTTP.IHttpClient        NO-UNDO.
    DEFINE VARIABLE oLib             AS OpenEdge.Net.HTTP.IHttpClientLibrary NO-UNDO.
    DEFINE VARIABLE oCredentials     AS Credentials                          NO-UNDO.
    DEFINE VARIABLE oRequestBody     as String                               no-undo.
    DEFINE VARIABLE oJsonObject      AS JsonObject                           NO-UNDO.
    DEFINE VARIABLE oJsonEntity      AS JsonArray                            NO-UNDO.
    DEFINE VARIABLE iIndexErro       AS INTEGER                              NO-UNDO.    
    DEFINE VARIABLE icount           AS INTEGER                              NO-UNDO.
    DEFINE VARIABLE itoken           AS i                                    NO-UNDO.
    DEFINE VARIABLE oClient          AS IHttpClient                          NO-UNDO.    
    DEFINE VARIABLE cToken           AS CHARACTER                            NO-UNDO.
    DEFINE VARIABLE client           AS COM-HANDLE                           NO-UNDO.
    DEFINE VARIABLE cServerName      AS CHARACTER                            NO-UNDO.


    FIND FIRST es-api-param-spf WHERE ROWID(es-api-param-spf) = pRowid NO-LOCK NO-ERROR.
    IF NOT AVAIL es-api-param-spf THEN
    DO:
        RUN piErro("Tipo de Integra‡Æo Inexistente.").
        RETURN "NOK":U.
    END.

    MESSAGE "##API" es-api-param-spf.cd-tipo-integr
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    FIND FIRST es-api-token-param-spf  
         WHERE es-api-token-param-spf.cod-sistema = es-api-param-spf.cd-sistema 
    NO-LOCK NO-ERROR.
    IF NOT AVAIL es-api-token-param-spf THEN
    DO:
        RUN piErro("Login de integra‡Æo nÆo informado.").
        RETURN "NOK":U.
    END.

    ASSIGN cHttpUrl    = es-api-param-spf.host-integr
           cServerName = cHttpUrl.


    /*-- Remove caracteres desnecess rios --*/
    ASSIGN cServerName =  REPLACE(cServerName, "https","")  
           cServerName =  REPLACE(cServerName, "http","")           
           cServerName =  REPLACE(cServerName, ":","")              
           cServerName =  REPLACE(cServerName, "//","").            


    /*-- Quando informado porta no esspf004, ‚ acrescentado no parametro --*/
    IF es-api-param-spf.porta-integr > 0 THEN ASSIGN cHttpUrl = cHttpUrl + ":" +  STRING(es-api-param-spf.porta-integr).

    ASSIGN cHttpUrl = cHttpUrl + es-api-param-spf.path-integr.

    oCredentials = new Credentials('Authorization', es-api-token-param-spf.cod-usuario, es-api-token-param-spf.cod-senha ).   
    oLib         = ClientLibraryBuilder:Build()
                                       :sslVerifyHost(NO)
                                       :ServerNameIndicator(cServerName)
                                       :library.
    oRequest = RequestBuilder:Post(cHttpUrl, oInputData)
                             :UsingBasicAuthentication(oCredentials)
                             :ContentType('application/json')
                             :AcceptJson()
                             :Request.

    oResponse = ClientBuilder:Build():Client:Execute(oRequest).
    
    IF ERROR-STATUS:ERROR THEN DO:
        RUN piErro ("Ocorreram erros no envio do Json - " + STRING(ERROR-STATUS:GET-MESSAGE(1)), "" ).
        RETURN "NOK":U.            
    END.

    IF oResponse:StatusCode < 200 OR oResponse:StatusCode > 299 THEN DO:
        RUN piErro ("Ocorreram erros no envio do Json - " + 
                    STRING(oResponse:statusCode)          + 
                    " - " + 
                    STRING(oResponse:StatusReason),"").
        RETURN "NOK":U.
    END.

    


END PROCEDURE.
