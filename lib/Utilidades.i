FUNCTION tratarString RETURNS CHARACTER
    (INPUT pcString AS CHAR):

    IF pcString <> ? THEN RETURN pcString. ELSE RETURN "".

END FUNCTION.

FUNCTION Stamp RETURNS CHARACTER:

    RETURN STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "").

END FUNCTION.

FUNCTION timeStamp RETURNS CHARACTER:

    RETURN SUBSTITUTE("[&1-&2]", STRING(TODAY, "99/99/9999"), STRING(TIME, "HH:MM:SS")).

END FUNCTION.

FUNCTION tratarArquivo RETURNS CHARACTER
    (INPUT cArquivo AS CHAR):

DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.

    ASSIGN cRetorno = REPLACE (REPLACE(cArquivo, "\", "/"), "//", "/").

/*     MESSAGE cRetorno SKIP                  */
/*         cretorno BEGINS "/"                */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    IF cRetorno BEGINS "/" THEN
        ASSIGN cRetorno = "/" + cRetorno.

    RETURN cRetorno.

END FUNCTION.


FUNCTION tratarDiretorio RETURNS CHARACTER 
    (INPUT cDiretorio AS CHAR):

    cDiretorio = tratarArquivo(cDiretorio).


    IF NOT SUBSTRING(cDiretorio, LENGTH(cDiretorio), 1) = "/" THEN
        ASSIGN cDiretorio = cDiretorio + "/".

    RETURN cDiretorio.

END FUNCTION.


