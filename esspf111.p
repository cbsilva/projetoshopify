
{lib/utilidades.i}
{lib/log2.i}
{lib/lerDiretorio.i}

DEFINE INPUT  PARAMETER pParam AS ROWID       NO-UNDO.


    FOR FIRST es-api-param-spf NO-LOCK WHERE
              ROWID(es-api-param-spf) = pParam:
    END.
    IF NOT AVAIL es-api-param-spf THEN
        RETURN "NOK".


    
    RUN lerDiretorio (es-api-param-spf.path-integr, NO) .

    FOR EACH ttArquivo NO-LOCK WHERE
             ttArquivo.cTipo        = "F":
         
        RUN esp/esspf111a.p
            (
             ttArquivo.cCaminho,
             es-api-param-spf.dir-export,
             es-api-param-spf.dir-env

             ).




    END.
