/*------------------------------------------------------------------------
    File        : apiPayments.p
    Purpose     : API REST para Implantação de Títulos
    Syntax      :
    Description : Clientes
    Author(s)   : 4Make Consultoria
    Created     : 08.09.2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i customer 2.00.00.000} /*** 010000 ***/

{utp/ut-api-action.i pi-create POST /~*}
{utp/ut-api-action.i pi-update PUT /~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}


{acr/api/v1/apipayments.i}
{acr/api/v1/apipaymentsVar.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*----------------------------------------------------------------------
 Purpose: Efetua a cria‡Æo de um novo emitente
------------------------------------------------------------------------*/
PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    OUTPUT TO VALUE ("\\192.168.0.131\datasul\Teste\ERP\quarentena\spf\logIntegracao\Payments.txt") APPEND.
        PUT UNFORMATTED "INICIO DA INTEGRACAO DE TITULOSS" SKIP.

    FIX-CODEPAGE(jsonRecebido) = "UTF-8".
    ASSIGN oRequestParser  = NEW JsonAPIRequestParser(jsonInput)
           jsonRecebido    = oRequestParser:getPayloadLongChar()
           i-prox-numero   = NEXT-VALUE(seq_import)
           cCnpjCpf        = ""
           cNrPedido       = "".

    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("payments":U).


    PUT UNFORMATTED "OK1" SKIP.

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        PUT UNFORMATTED "OK2" SKIP.

        IF oJsonObjectMain:Has("customerCNPJ")   then do:
            cCnpjCpf = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("customerCNPJ"),".",""),"/",""),"-","")  NO-ERROR             .
            
        END.
		
		IF oJsonObjectMain:Has("orderNumber")   then do:
            cNrPedido = "DG" + REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("orderNumber"),".",""),"/",""),"-","")  NO-ERROR             .
            
        END.
    END.

    PUT UNFORMATTED "OK3" SKIP.

    MESSAGE SUBSTITUTE("PROXIMO NUMERO &1", i-prox-numero).
    
    CREATE  es-api-import-spf.                                            
    ASSIGN  es-api-import-spf.id-movto          = i-prox-numero 
            es-api-import-spf.cd-tipo-integr    = 23 /*-- Importacao de Titulos --*/   
            es-api-import-spf.chave             = cCnpjCpf + '-' + cNrPedido   
            es-api-import-spf.data-movto        = NOW                     
            es-api-import-spf.data-inicio       = NOW                     
            es-api-import-spf.data-fim          = ?                       
            es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import-spf.cod-status        = 0 /*--- sem status ---*/
            es-api-import-spf.c-json            = jsonRecebido.   


    RUN pi-gera-status (cCnpjCpf + '-' + cNrPedido,                
                        "Registro em processamento",                          
                        "").                                
                                                            
    /* -------- Grava retorno ------*/                      
    ASSIGN  jsonRetorno = NEW JsonArray().                  
            jsonRetorno:Read(TEMP-TABLE ttRetorno:HANDLE).  
                                                            
    RUN createJsonResponse(INPUT jsonRetorno,               
                           INPUT TABLE RowErrors,           
                           INPUT FALSE,                     
                           OUTPUT jsonOutput).

    OUTPUT CLOSE.

END PROCEDURE.

/*----------------------------------------------------------------------
 Purpose: Efetua a cria‡Æo dos status de erro
------------------------------------------------------------------------*/
PROCEDURE pi-gera-status:
    DEFINE INPUT PARAMETER pChave       AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pSituacao    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pMensagem    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    CREATE ttRetorno.
    ASSIGN ttRetorno.chave          = pChave
           ttRetorno.situacao       = pSituacao
           ttRetorno.descricao      = pMensagem.
    
END PROCEDURE.
