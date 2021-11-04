/*------------------------------------------------------------------------
    File        : customer.p
    Purpose     : API REST para Manuten‡Æo de Clientes
    Syntax      :
    Description : Clientes
    Author(s)   : 4Make Consultoria
    Created     : 15.07.2021
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


{cdp/api/v1/apicustomer.i}
{cdp/api/v1/apicustomerVar.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*----------------------------------------------------------------------
 Purpose: Efetua a cria‡Æo de um novo emitente
------------------------------------------------------------------------*/
PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    OUTPUT TO VALUE ("\\192.168.0.131\datasul\Teste\ERP\quarentena\spf\logIntegracao\CriaCliente.txt") APPEND.
        PUT UNFORMATTED "INICIO DA INTEGRACAO DE CLIENTES" SKIP.

    FIX-CODEPAGE(jsonRecebido) = "UTF-8".

    ASSIGN oRequestParser  = NEW JsonAPIRequestParser(jsonInput)
           jsonRecebido    = oRequestParser:getPayloadLongChar()
           i-prox-numero   = NEXT-VALUE(seq_import)
           cCnpjCpf        = "".


    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("customer":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("CNPJ")   then do:
            cCnpjCpf = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("CNPJ"),".",""),"/",""),"-","")  NO-ERROR             .
            LEAVE.
        END.
    END.

    MESSAGE SUBSTITUTE("PROXIMO NUMERO &1", i-prox-numero).
    
    CREATE  es-api-import-spf.                                            
    ASSIGN  es-api-import-spf.id-movto          = i-prox-numero 
            es-api-import-spf.cd-tipo-integr    = 21 /*-- Importacao de Clientes --*/   
            es-api-import-spf.chave             = cCnpjCpf     
            es-api-import-spf.data-movto        = NOW                     
            es-api-import-spf.data-inicio       = NOW                     
            es-api-import-spf.data-fim          = ?                       
            es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import-spf.cod-status        = 0 /*--- sem status ---*/
            es-api-import-spf.c-json            = jsonRecebido.   


    RUN pi-gera-status (cCnpjCpf,                
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
 Purpose: Efetua a atualiza‡Æo de um emitente existente
------------------------------------------------------------------------*/
PROCEDURE pi-update:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    OUTPUT TO VALUE ("\\192.168.0.131\datasul\Teste\ERP\quarentena\spf\logIntegracao\alteraCliente.txt").

     FIX-CODEPAGE(jsonRecebido) = "UTF-8".

    ASSIGN oRequestParser  = NEW JsonAPIRequestParser(jsonInput)
           jsonRecebido    = oRequestParser:getPayloadLongChar()
           i-prox-numero   = NEXT-VALUE(seq_import)
           cCnpjCpf        = "".

    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("customer":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("CNPJ")                   then do:
            cCnpjCpf = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("CNPJ"),".",""),"/",""),"-","")  NO-ERROR             .
            LEAVE.
        END.
    END.

    MESSAGE SUBSTITUTE("PROXIMO NUMERO &1", i-prox-numero).

    
    CREATE  es-api-import-spf.                                            
    ASSIGN  es-api-import-spf.id-movto          = i-prox-numero  
            es-api-import-spf.cd-tipo-integr    = 21 /*-- Importacao de Clientes --*/ 
            es-api-import-spf.chave             = cCnpjCpf     
            es-api-import-spf.data-movto        = NOW                     
            es-api-import-spf.data-inicio       = NOW                     
            es-api-import-spf.data-fim          = ?                       
            es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import-spf.cod-status        = 0 /*--- sem status ---*/
            es-api-import-spf.c-json            = jsonRecebido.   


    RUN pi-gera-status (cCnpjCpf,                
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
 Purpose: Retorna lista de clientes
------------------------------------------------------------------------*/
PROCEDURE pi-getAll:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    /*-- define jsonArray de emitente --*/
    DEFINE VARIABLE jsonEmitente AS JsonArray NO-UNDO.

    EMPTY TEMP-TABLE ttEmitente.
    EMPTY TEMP-TABLE RowErrors.


    FOR EACH emitente NO-LOCK
        WHERE emitente.cod-emitente > 0 
          AND emitente.identific    <> 2 /*fornecedor*/ :

        CREATE ttEmitente.
        ASSIGN ttEmitente.cod-emitente = emitente.cod-emitente
               ttEmitente.nome-abrev   = emitente.nome-abrev.

    END.


    jsonEmitente = NEW JsonArray().
    jsonEmitente:READ(TEMP-TABLE ttEmitente:HANDLE).


    RUN createJsonResponse(INPUT jsonEmitente, 
                           INPUT TABLE RowErrors, 
                           INPUT FALSE, OUTPUT jsonOutput).


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
