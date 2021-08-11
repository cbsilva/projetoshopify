DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)":U
    FIELD desc-arq  AS CHARACTER.

DEFINE VARIABLE cMensagem AS CHARACTER   NO-UNDO.

SESSION:DEBUG-ALERT = YES.

    RUN btb/btapi910ze.p (INPUT "user_rpw",
                          INPUT "", 
                          INPUT "1", 
                          OUTPUT TABLE tt-erros). 
    IF RETURN-VALUE <> "OK" THEN 
    DO:
        FOR EACH tt-erros:
            ASSIGN cMensagem = cMensagem + tt-erros.desc-erro + CHR(10).
        END.
        RUN utp/ut-msgs.p (INPUT "show":U, 
                            INPUT  17006,
                            INPUT "Houve um erro que impediu o login. O rob“ ser  finalizado.~~" +
                                  cMensagem).   
        QUIT.
    END.
        


    RUN esp/esspf100.w
        .



    QUIT.
