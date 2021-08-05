/* DEFINE TEMP-TABLE RowErrors NO-UNDO */
/*     FIELD ErrorSequence    AS INTEGER */
/*     FIELD ErrorNumber      AS INTEGER */
/*     FIELD ErrorDescription AS CHARACTER */
/*     FIELD ErrorParameters  AS CHARACTER */
/*     FIELD ErrorType        AS CHARACTER */
/*     FIELD ErrorHelp        AS CHARACTER */
/*     FIELD ErrorSubType     AS CHARACTER. */
/*    */
DEFINE TEMP-TABLE rowErrorsAux LIKE rowErrors.


/*
*------------------------------------------------------------------------------
*      Gera mensagens de erro
* ------------------------------------------------------------------------------
*/
PROCEDURE gerarRowError:
DEFINE INPUT  PARAMETER pcMensagem AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iSeq AS INTEGER     NO-UNDO.

    FIND LAST rowErrors NO-ERROR.
    ASSIGN iSeq = IF AVAIL rowErrors THEN rowErrors.ErrorSequence + 1 ELSE 1.

    CREATE  rowErrors.
    ASSIGN  rowErrors.ErrorSequence     = iSeq 
            rowErrors.ErrorNumber       = 17600
            rowErrors.ErrorDescription  = pcMensagem
            rowErrors.ErrorParameters   = ""
            rowErrors.ErrorType         = "EMS"
            rowErrors.ErrorHelp         = ""
            rowErrors.ErrorSubType      = "ERROR".

/*                                                                */
/*     {method/svc/errors/inserr.i &ErrorNumber="17600"           */
/*                                 &ErrorType="EMS"               */
/*                                 &ErrorSubType="ERROR"          */
/*                                 &ErrorParameters="pcMensagem"} */


END PROCEDURE.

/*
*------------------------------------------------------------------------------
*      Gera mensagens 
* ------------------------------------------------------------------------------
*/
PROCEDURE gerarMensagem:
DEFINE INPUT  PARAMETER pcMensagem  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER piCodigo    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pcType      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iSeq AS INTEGER     NO-UNDO.

    FIND LAST rowErrors NO-ERROR.
    ASSIGN iSeq = IF AVAIL rowErrors THEN rowErrors.ErrorSequence + 1 ELSE 1.


    CREATE  rowErrors.
    ASSIGN  rowErrors.ErrorSequence     = iSeq 
            rowErrors.ErrorNumber       = piCodigo
            rowErrors.ErrorDescription  = pcMensagem
            rowErrors.ErrorParameters   = ""
            rowErrors.ErrorType         = "EMS"
            rowErrors.ErrorHelp         = ""
            rowErrors.ErrorSubType      = pcType.

/*                                                                */
/*     {method/svc/errors/inserr.i &ErrorNumber="17600"           */
/*                                 &ErrorType="EMS"               */
/*                                 &ErrorSubType="ERROR"          */
/*                                 &ErrorParameters="pcMensagem"} */


END PROCEDURE.



/*
*------------------------------------------------------------------------------
*      Copia Mensagens de Erro de BO (AUX) erros da rotina de retorn veis
* ------------------------------------------------------------------------------
*/
PROCEDURE ObterErrosAux:


    FOR EACH rowErrorsAux:
        CREATE rowErrors.
        BUFFER-COPY rowErrorsAux TO rowErrors.

        ASSIGN rowErrors.errordescription = rowErrors.errordescription + " (" + tratarString(STRING(rowErrorsAux.errorParameters)) + ").".
    END.

    EMPTY TEMP-TABLE rowErrorsAux.

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      Retorna Mensagens
 * ------------------------------------------------------------------------------
 */
PROCEDURE RetornarErros:
DEFINE OUTPUT PARAMETER TABLE FOR rowErrors. 

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      Envia mensagens para LOG
 * ------------------------------------------------------------------------------
 */
PROCEDURE FazerDumpErros:

    IF CAN-FIND (FIRST rowErrors) THEN
    DO:
        RUN gerarLog("Saindo com Erros:").
        FOR EACH rowErrors:
            RUN gerarLog("-- " + tratarString(rowErrors.errorDescription) + "(" + tratarString(STRING(rowErrors.errorNumber))  + ")").

        END.

    END.

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      Envia mensagens para LOG
 * ------------------------------------------------------------------------------
 */
PROCEDURE limparErros:

    FOR EACH rowErrors:
        DELETE rowErrors.
    END.

END PROCEDURE.
