
{lib/utilidades.i}
{lib/log2.i}
{lib/lerDiretorio.i}

DEFINE INPUT  PARAMETER pParam AS ROWID       NO-UNDO.


    FOR FIRST es-api-param NO-LOCK WHERE
              ROWID(es-api-param) = pParam:
    END.
    IF NOT AVAIL es-api-param THEN
        RETURN "NOK".


    
    RUN lerDiretorio (es-api-param.path-integr, NO) .

    FOR EACH ttArquivo NO-LOCK WHERE
             ttArquivo.cTipo        = "F":
         
        RUN esp/esint111a.p
            (
             ttArquivo.cCaminho,
             es-api-param.dir-export,
             es-api-param.dir-env

             ).




    END.
