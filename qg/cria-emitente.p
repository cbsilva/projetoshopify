DEFINE VARIABLE i-cod-emitente LIKE emitente.cod-emitente NO-UNDO.
DEFINE VARIABLE rw-emitente    AS ROWID                   NO-UNDO.
DEFINE VARIABLE h-boad098      AS HANDLE                  NO-UNDO.


{include/boini.i} /*tt-bo-erro*/
DEFINE TEMP-TABLE tt-emitente NO-UNDO LIKE emitente
    FIELD r-rowid AS ROWID.



IF NOT VALID-HANDLE(h-boad098) THEN
    RUN adbo/boad098.p PERSISTENT SET h-boad098.



EMPTY TEMP-TABLE tt-emitente.


/*-- cria novo numero de emitente --*/
run cdp/cd9960.p (output i-cod-emitente).




RUN newRecord IN h-boad098.
RUN getRecord IN h-boad098(OUTPUT TABLE tt-emitente).


FOR EACH tt-emitente. 
    DISP tt-emitente EXCEPT r-rowid char-1 endereco-cob-text 
         endereco_text cod-email-nfe

         WITH WIDTH 333 1 COL.
END.








