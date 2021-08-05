DEFINE VARIABLE cArquivo    AS CHARACTER   NO-UNDO.             
DEFINE VARIABLE cCaminho    AS CHARACTER   NO-UNDO FORM "x(70)".
DEFINE VARIABLE cTipo       AS CHARACTER   NO-UNDO.             
DEFINE VARIABLE cExtensao   AS CHARACTER   NO-UNDO.             
DEFINE VARIABLE cArquivoCam AS CHARACTER   NO-UNDO FORM "x(70)".

DEFINE TEMP-TABLE ttArquivo 
    FIELD cArquivo    AS CHARACTER   
    FIELD cCaminho    AS CHARACTER   FORM "x(70)"                                
    FIELD cTipo       AS CHARACTER                                                
    FIELD cExtensao   AS CHARACTER                                                
    FIELD cArquivoCam AS CHARACTER   FORM "x(70)"
    
    FIELD cProgramaPai  AS CHARACTER 
    FIELD lContemplar   AS LOGICAL
    FIELD cQuarentena   AS CHARACTER
    .


PROCEDURE lerDiretorio:
DEFINE INPUT  PARAMETER cDir            AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER lRecursivo      AS LOGICAL     NO-UNDO.
    
    INPUT FROM OS-DIR(cDir).
    REPEAT:

        IMPORT  cArquivo cCaminho cTipo.

        IF carquivo = "." OR cArquivo = ".." THEN NEXT.

        IF cTipo = "F" THEN
        DO:
            ASSIGN  cExtensao        = ENTRY(NUM-ENTRIES(cArquivo, "."), cArquivo, ".")
                    cArquivoCam      = SUBSTRING(cCaminho, 1, LENGTH(cCaminho) - (LENGTH(cExtensao) + 1 ) ).

            CREATE  ttArquivo.
            ASSIGN  ttArquivo.cArquivo      = cArquivo   
                    ttArquivo.cCaminho      = cCaminho   
                    ttArquivo.cTipo         = cTipo      
                    ttArquivo.cExtensao     = cExtensao  
                    ttArquivo.cArquivoCam   = cArquivoCam.

        END.
        IF lRecursivo AND cTipo = "D" THEN
        DO:
            
            RUN lerDiretorio(cCaminho).

        END.



    END.
    INPUT CLOSE.




END PROCEDURE .


