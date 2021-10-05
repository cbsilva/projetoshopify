/*------------------------------------------------------------------------
    File        : Order.p
    Purpose     : API REST para Manuten‡Æo de Pedidos de Venda
    Syntax      :
    Description : Pedido de Venda
    Author(s)   : 4Make Consultoria
    Created     : 08.08.2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i Order 2.00.00.000} /*** 010000 ***/

{utp/ut-api-action.i pi-create POST /~*}

{utp/ut-api-notfound.i}


{pdp/api/v1/apiOrder.i}
{pdp/api/v1/apiOrderVar.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*----------------------------------------------------------------------
 Purpose: Efetua a criaï¿½ï¿½o de um novo emitente
------------------------------------------------------------------------*/
PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.    

    FIX-CODEPAGE(jsonRecebido) = "UTF-8".

    ASSIGN oRequestParser  = NEW JsonAPIRequestParser(jsonInput)
           jsonRecebido    = oRequestParser:getPayloadLongChar()
           i-prox-numero   = NEXT-VALUE(seq_import)
           cNrPedido       = "".


    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("order":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("OrderNumber")   then do:
            cNrPedido = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("OrderNumber"),".",""),"/",""),"-","")  NO-ERROR             .
            LEAVE.
        END.
    END.

    MESSAGE SUBSTITUTE("PROXIMO NUMERO &1", i-prox-numero).
    
    CREATE  es-api-import-spf.                                            
    ASSIGN  es-api-import-spf.id-movto          = i-prox-numero 
            es-api-import-spf.cd-tipo-integr    = 22 /*-- Importacao de Pedidos --*/   
            es-api-import-spf.chave             = cNrPedido     
            es-api-import-spf.data-movto        = NOW                     
            es-api-import-spf.data-inicio       = NOW                     
            es-api-import-spf.data-fim          = ?                       
            es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/  
            es-api-import-spf.cod-status        = 0 /*--- sem status ---*/
            es-api-import-spf.c-json            = jsonRecebido.   


    RUN pi-gera-status (cNrPedido,                
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
