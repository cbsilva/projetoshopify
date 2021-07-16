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


/* {cdp/api/vi/customer.i}     */
/* {cdp/api/vi/customerVar.i}  */

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*----------------------------------------------------------------------
 Purpose: Efetua a cria‡Æo de um novo emitente
------------------------------------------------------------------------*/
PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.



    RUN createJsonResponse(NEW JsonObject(), INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

END PROCEDURE.




/*----------------------------------------------------------------------
 Purpose: Efetua a atualiza‡Æo de um emitente existente
------------------------------------------------------------------------*/
PROCEDURE pi-update:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.



    RUN createJsonResponse(NEW JsonObject(), INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).
END PROCEDURE.
