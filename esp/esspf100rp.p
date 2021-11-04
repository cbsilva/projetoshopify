/*----------------------------------------------------------------------------------------------/
 Programa..: esspf100rp.p
 Objetivo..: API Integraá‰es JSON - baseado no esspf001 (hist¢rico abaixo)
 Data......: 30/11/2019
 Autor.....: LASF
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------
 Programa..: esspf001rp.p
 Objetivo..: API Integraá‰es JSON
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{include/i-prgvrs.i esspf100rp 2.09.00.002}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esspf100rp mpd}
&ENDIF

{lib/utilidades.i}
{lib/log2.i} 

/* ------- Definiá∆o de Vari†veis ------*/
DEFINE VARIABLE l-tipo                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i-cod-sist                AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-proc                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i-time                    AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE hnd-apps-server           AS HANDLE  EXTENT 20  NO-UNDO. /* handle do servidor app */
DEFINE VARIABLE hnd-apps-server-async     AS HANDLE  EXTENT 20  NO-UNDO. /* handle da conex∆o app  */  
DEFINE VARIABLE i-count                   AS INTEGER   NO-UNDO.

DEFINE VARIABLE hFiStatus AS HANDLE      NO-UNDO.


/* /* ------ Definiá∆o de Temp -----*/    */
/* DEFINE TEMP-TABLE tt-apps NO-UNDO      */
/*     LIKE es-api-app-spfserver              */
/*     FIELD i-agent-conect AS INTEGER    */
/*     FIELD ind-server     AS INTEGER    */
/*     FIELD l-padrao       AS LOGICAL    */
/*     FIELD cd-agente      AS INTEGER    */
/*     FIELD ativo          AS LOGICAL    */
/*     FIELD erro           AS INTEGER    */
/*     INDEX i1 IS PRIMARY ind-tipo-trans */
/*                         cd-sistema     */
/*                         host-appserver */
/*                         erro.          */


PROCEDURE atualizarStatus:
DEFINE INPUT  PARAMETER pStatus AS CHARACTER   NO-UNDO.

    ASSIGN hFiStatus:SCREEN-VALUE       = timeStamp() + " " + pStatus.

END PROCEDURE.

/*--------------------------------------Procedures ------------------------------------*/

PROCEDURE pi-processa:
/* ------- Definiá∆o de ParÉmetros -----*/
DEFINE INPUT  PARAMETER pSistema        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pEntrada        AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER pSaida          AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER phFiStatus      AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pAS             AS LOGICAL     NO-UNDO.

    IF LOG-MANAGER:LOGFILE-NAME     = "" OR LOG-MANAGER:LOGFILE-NAME     = ? THEN
    RUN ativarClientLOg.

    RUN abrirLog(SESSION:TEMP-DIR + "esspf100rp_log.txt").
    RUN gerarLog("Iniciado processo").

    hFiStatus   =  phFiStatus.

    PROCESS EVENTS.

    //RUN gerarLog("Chamando execuá∆o entrada").
    //RUN atualizarStatus ("Chamando execuá∆o entrada").

    IF pEntrada >  0  THEN
    DO:
        IF pEntrada = 999 THEN
        DO:
            FOR EACH es-api-param-spf NO-LOCK WHERE
                     es-api-param-spf.cd-sistema        = pSistema
                AND  es-api-param-spf.ind-tipo-trans    = 1:

                RUN pi-proc-hdl (es-api-param-spf.cd-tipo-integr) NO-ERROR. /* import */        

            END. // EACH es-api-param-spf
        END.
        ELSE
        DO:
            FOR EACH es-api-param-spf NO-LOCK WHERE
                     es-api-param-spf.cd-sistema        = pSistema
                AND  es-api-param-spf.ind-tipo-trans    = 1
                AND  es-api-param-spf.cd-tipo-integr    = pEntrada :

                RUN pi-proc-hdl (es-api-param-spf.cd-tipo-integr) NO-ERROR. 

            END. // EACH es-api-param-spf
        END.
    END.

    IF pSaida >  0  THEN
    DO:

        IF pSaida = 999 THEN
        DO:
            FOR EACH es-api-param-spf NO-LOCK WHERE
                     es-api-param-spf.cd-sistema        = pSistema
                AND  es-api-param-spf.ind-tipo-trans    = 2:

                RUN pi-proc-hdl (es-api-param-spf.cd-tipo-integr) NO-ERROR. 

            END. // EACH es-api-param-spf
        END.
        ELSE
        DO:
            FOR EACH es-api-param-spf NO-LOCK WHERE
                     es-api-param-spf.cd-sistema        = pSistema
                AND  es-api-param-spf.ind-tipo-trans    = 2
                AND  es-api-param-spf.cd-tipo-integr    = pSaida :

                RUN pi-proc-hdl (es-api-param-spf.cd-tipo-integr) NO-ERROR. 

            END. // EACH es-api-param-spf
        END.
    END.


   
/*     IF p-AS THEN                                        */
/*     DO:                                                 */
/*                                                         */
/*         RUN atualizarStatus ("Finalizando servidores"). */
/*                                                         */
/*                                                         */
/*     END.                                                */
    

    RUN gerarLog("Processo conclu°do").
    RUN atualizarStatus ("Processamento Conclu°do").

END PROCEDURE.


PROCEDURE pi-proc-hdl:

DEFINE INPUT PARAMETER pEndPoint AS INTEGER NO-UNDO.

    DEF VAR c-string-conexao AS CHARACTER NO-UNDO.
    DEF VAR c-erro           AS CHARACTER NO-UNDO.
    DEF VAR i-erro           AS INTEGER   NO-UNDO.



    DEFINE VARIABLE cHora AS CHARACTER   NO-UNDO.

/*     ASSIGN LOG-MANAGER:LOGFILE-NAME     =             */
/*         session:TEMP-DIR + "clientlog_"  +            */
/*         STRING(YEAR(TODAY) , "9999") +                */
/*         STRING(MONTH(TODAY) , "9999") +               */
/*         STRING(DAY(TODAY) , "9999") +                 */
/*         SUBSTRING(chora, 1, 2) +                      */
/*         SUBSTRING(chora, 4, 2) +                      */
/*         SUBSTRING(chora, 7, 2) + ".txt"               */
/*         .                                             */
/*     ASSIGN LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTrace".  */
/*     ASSIGN LOG-MANAGER:LOGGING-LEVEL = 2            . */


    FOR EACH es-api-param-spf NO-LOCK WHERE
             es-api-param-spf.cd-tipo-integr = pEndPoint
         AND es-api-param-spf.ativo
                :

            IF es-api-param-spf.ind-tipo-trans = 1 THEN
            DO:
/*                 IF CAN-FIND(FIRST es-api-import-spf NO-LOCK                                                        */
/*                             WHERE es-api-import-spf.cd-tipo-integr      = es-api-param-spf.cd-tipo-integr              */
/*                               AND ( es-api-import-spf.ind-situacao = 0  // Pendente                                */
/*                                  OR es-api-import-spf.ind-situacao = 3) ) // Erro Progress                         */
/*                     OR                                                                                         */
/*                                                                                                                */
/*                     (es-api-param-spf.path-integr BEGINS "\\" OR SUBSTRING(es-api-param-spf.path-integr, 2, 1) = ":" ) */
/*                                                                                                                */
/*                 THEN                                                                                           */
                    RUN esp/esspf100irp.p (INPUT es-api-param-spf.cd-tipo-integr).



            END.
            ELSE
            DO:
/*                 IF CAN-FIND(FIRST es-api-export-spf NO-LOCK */
/*                             WHERE es-api-export-spf.cd-tipo-integr      = es-api-param-spf.cd-tipo-integr */
/*                               AND ( es-api-export-spf.ind-situacao = 0  // Pendente */
/*                                  OR es-api-export-spf.ind-situacao = 3) ) // Erro Progress */
/*                 THEN */
/*                 DO: */
                    RUN esp/esspf100erp.p (INPUT es-api-param-spf.cd-tipo-integr).
/*                 END. */
                    
            END.


    END. /* EACH es-api-param-spf */

    RETURN "OK".



END PROCEDURE.


PROCEDURE pi-finaliza:



    DEFINE INPUT PARAMETER p-ind-trans  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p-cd-sist    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p-agente     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p-nm-appserv AS CHARACTER NO-UNDO.

    /* ------- Deleta objeto de conex∆o -----*/

    IF VALID-HANDLE(hnd-apps-server[p-agente]) THEN DO:

        hnd-apps-server[p-agente]:DISCONNECT() NO-ERROR.  

        DELETE OBJECT hnd-apps-server[p-agente].
 
        ASSIGN hnd-apps-server[p-agente] = ?.
    END.

    
    
END PROCEDURE.


PROCEDURE ativarClientlog:
DEFINE VARIABLE cHora AS CHARACTER   NO-UNDO.

cHora = STRING(TIME, "HH:MM:SS").

SESSION:DEBUG-ALERT = YES.

ASSIGN LOG-MANAGER:LOGFILE-NAME     = 
    "c:\temp\clientlog_"  + 
    STRING(YEAR(today) , "9999") + 
    STRING(MONTH(today) , "9999") + 
    STRING(DAY(today) , "9999") + 
    SUBSTRING(chora, 1, 2) +
    SUBSTRING(chora, 4, 2) +
    SUBSTRING(chora, 7, 2) + ".txt"
    .

ASSIGN LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTrace".
ASSIGN LOG-MANAGER:LOGGING-LEVEL = 2            .


/*                                                     */
/* -clientlog  mylog.lg -logginglevel 2 -logentrytypes */
/* 4GLTrace                                            */


END PROCEDURE.

RETURN "OK".
