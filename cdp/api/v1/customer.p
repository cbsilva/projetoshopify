/*------------------------------------------------------------------------
    File        : customer.p
    Purpose     : API REST para Manuten‡Æo de Clientes
    Syntax      :
    Description : Clientes
    Author(s)   : Cleberson Silva
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


{cdp/api/v1/customer.i}
{cdp/api/v1/customerVar.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*----------------------------------------------------------------------
 Purpose: Efetua a cria‡Æo de um novo emitente
------------------------------------------------------------------------*/
PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    FIX-CODEPAGE(jsonRecebido) = "UTF-8".

    ASSIGN oRequestParser  = NEW JsonAPIRequestParser(jsonInput)
           jsonRecebido    = oRequestParser:getPayloadLongChar()
           cCnpjCpf        = "".


    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("customer":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("CNPJ_CPF")                   then do:
            cCnpjCpf = oJsonObjectMain:GetCharacter("CNPJ_CPF")  NO-ERROR             .
            LEAVE.
        END.
    END.

    /*-- precisa verificar no ambiente qual ser  o numero da integra‡Æo*/
    CREATE  es-api-import.                                            
    ASSIGN  es-api-import.id-movto          = NEXT-VALUE(seq_import)  
            es-api-import.cd-tipo-integr    = 999 // Customer     
            es-api-import.chave             = cCnpjCpf     
            es-api-import.data-movto        = NOW                     
            es-api-import.data-inicio       = NOW                     
            es-api-import.data-fim          = ?                       
            es-api-import.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import.cod-status        = 0 /*--- sem status ---*/
            es-api-import.c-json            = jsonRecebido.   


    RUN pi-gera-status (cCnpjCpf,                
                        "Sucesso",                          
                        "").                                
                                                            
    /* -------- Grava retorno ------*/                      
    ASSIGN  jsonRetorno = NEW JsonArray().                  
            jsonRetorno:Read(TEMP-TABLE ttRetorno:HANDLE).  
                                                            
    RUN createJsonResponse(INPUT jsonRetorno,               
                           INPUT TABLE RowErrors,           
                           INPUT FALSE,                     
                           OUTPUT jsonOutput).              




    

END PROCEDURE.




/*----------------------------------------------------------------------
 Purpose: Efetua a atualiza‡Æo de um emitente existente
------------------------------------------------------------------------*/
PROCEDURE pi-update:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonRetorno             AS JsonArray            NO-UNDO.
    DEFINE VARIABLE json_recebido           AS LONGCHAR             NO-UNDO.
    DEFINE VARIABLE oRequestParser          AS JsonAPIRequestParser NO-UNDO.
    DEFINE VARIABLE CodigoCliente           AS CHARACTER INITIAL ?  NO-UNDO.


    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("customer":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("CNPJ_CPF")                   then do:
            cCnpjCpf = oJsonObjectMain:GetCharacter("CNPJ_CPF")  NO-ERROR             .
            LEAVE.
        END.
    END.

    /*-- precisa verificar no ambiente qual ser  o numero da integra‡Æo*/
    CREATE  es-api-import.                                            
    ASSIGN  es-api-import.id-movto          = NEXT-VALUE(seq_import)  
            es-api-import.cd-tipo-integr    = 999 // Customer     
            es-api-import.chave             = cCnpjCpf     
            es-api-import.data-movto        = NOW                     
            es-api-import.data-inicio       = NOW                     
            es-api-import.data-fim          = ?                       
            es-api-import.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import.cod-status        = 0 /*--- sem status ---*/
            es-api-import.c-json            = jsonRecebido.   


    RUN pi-gera-status (cCnpjCpf,                
                        "Sucesso",                          
                        "").                                
                                                            
    /* -------- Grava retorno ------*/                      
    ASSIGN  jsonRetorno = NEW JsonArray().                  
            jsonRetorno:Read(TEMP-TABLE ttRetorno:HANDLE).  
                                                            
    RUN createJsonResponse(INPUT jsonRetorno,               
                           INPUT TABLE RowErrors,           
                           INPUT FALSE,                     
                           OUTPUT jsonOutput).
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
