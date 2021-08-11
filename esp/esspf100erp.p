/*----------------------------------------------------------------------------------------------/
 Programa..: esspf100erp.p
 Objetivo..: API Integra��es JSON - baseado no esspf001 (hist�rico abaixo)
 Data......: 30/11/2019
 Autor.....: TOTVS - LASF
 Vers�o....: 1.000.000
-----------------------------------------------------------------------------------------------
 Programa..: esspf001erp.p
 Objetivo..: API Integra��es JSON
 Data......: 26/02/2019
 Autor.....: Rog�rio Dias
 Vers�o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{include/i-prgvrs.i esspf100erp 2.09.00.002}

{lib/utilidades.i}
/* {lib/logsfa.i} */

/* ------ Deini��o de Par�metros ------*/
DEFINE INPUT PARAMETER pEndPoint AS INTEGER   NO-UNDO.

/* ------- Defini��o de vari�veis ----- */
DEF VAR c-erro AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-chave AS CHARACTER   NO-UNDO.

DEFINE VARIABLE i-count      AS INTEGER   NO-UNDO.

DEFINE BUFFER bf-es-api-export-spf FOR es-api-export-spf.
DEFINE VARIABLE iErro AS INTEGER     NO-UNDO.

/* ----- Defini��o de fun��es -----*/
FUNCTION fncAgendaAtiva RETURN LOGICAL
    ( p-es-api-param-spf AS INTEGER ) FORWARD.


/* RUN abrirLog("//fenix/ERP/camil/teste-verde/especificos/lasf/roboSFA/esspf001irp_log_" + stamp() + ".txt"). */
LOG-MANAGER:WRITE-MESSAGE("In�cio Processo - esspf100EIRP - V.2.5 - MERGE LOGISTICA - Exporta��o").

/* RUN piNewCarregaPend. */

DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
DO iLoop = 1 TO 15:

    LOG-MANAGER:WRITE-MESSAGE("LOOP = " + STRING(iLoop)).
    RUN piNewCarregaPend.

END.

/********************************************************************************************/
/********************************************************************************************/
/********************************************************************************************/
PROCEDURE piNewCarregaPend:
    verif-tipo:
    FOR  EACH es-api-param-spf NO-LOCK
        WHERE es-api-param-spf.cd-tipo-integr        = pEndPoint
          AND es-api-param-spf.ativo:
        
        LOG-MANAGER:WRITE-MESSAGE("Verificando pendencias - End Point: " +  tratarString(STRING(pendpoint))).
        IF SEARCH(es-api-param-spf.programa-integr ) = ? THEN NEXT.

        LOG-MANAGER:WRITE-MESSAGE("INI - FOR EACH es-api-param-spf").
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.ind-tipo-trans   " + tratarString(STRING(es-api-param-spf.ind-tipo-trans  )) ).
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.cd-sistema       " + tratarString(STRING(es-api-param-spf.cd-sistema      )) ).
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.cd-tipo-integr   " + tratarString(STRING(es-api-param-spf.cd-tipo-integr  )) ).
         
        IF es-api-param-spf.tip-integracao = 2 
            THEN
        DO:

/*             MESSAGE '1'                                   */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
             //RUN VALUE( es-api-param-spf.programa-integr ) (INPUT ROWID(es-api-param-spf)) NO-ERROR.
             RUN VALUE( es-api-param-spf.programa-integr ) (OUTPUT c-Erro) NO-ERROR.
/*              MESSAGE '2'                                   */
/*                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

        END.

        ELSE
        DO:

            proc-pend_0:
            FOR EACH  bf-es-api-export-spf 
                WHERE bf-es-api-export-spf.cd-tipo-integr   = es-api-param-spf.cd-tipo-integr
                  AND bf-es-api-export-spf.ind-situacao     = 0 //  Pendente

                NO-LOCK:

                LOG-MANAGER:WRITE-MESSAGE("INI - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-export-spf.cd-tipo-integr) 
                                + " Situa��o:" + STRING(bf-es-api-export-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-export-spf.chave)
                                + " Transa��o ativa:" + STRING(TRANSACTION)).

                RUN pi-processa-transacao.
                IF RETURN-VALUE = "_LOCKED" THEN
                    NEXT proc-pend_0.

                LOG-MANAGER:WRITE-MESSAGE("FIM - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-export-spf.cd-tipo-integr) 
                                + " Situa��o:" + STRING(bf-es-api-export-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-export-spf.chave)
                                + " Transa��o ativa:" + STRING(TRANSACTION)).

                LEAVE proc-pend_0.

            END.
            //Processa somente os registros que ocorreram erro progress
            proc-pend_1:
            FOR EACH bf-es-api-export-spf 
                WHERE bf-es-api-export-spf.cd-tipo-integr   = es-api-param-spf.cd-tipo-integr
                  AND bf-es-api-export-spf.ind-situacao     = 3 //  Erro Progress
                NO-LOCK:

                LOG-MANAGER:WRITE-MESSAGE("INI - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-export-spf.cd-tipo-integr) 
                                + " Situa��o:" + STRING(bf-es-api-export-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-export-spf.chave)
                                + " Transa��o ativa:" + STRING(TRANSACTION)).

                RUN pi-processa-transacao.
                IF RETURN-VALUE = "_LOCKED" THEN
                    NEXT proc-pend_1.

                LOG-MANAGER:WRITE-MESSAGE("FIM - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-export-spf.cd-tipo-integr) 
                                + " Situa��o:" + STRING(bf-es-api-export-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-export-spf.chave)
                                + " Transa��o ativa:" + STRING(TRANSACTION)).

                LEAVE proc-pend_1.

            END.

        END.


        LOG-MANAGER:WRITE-MESSAGE("FIM - FOR EACH es-api-param-spf").

    END.



END PROCEDURE.

/********************************************************************************************/
/********************************************************************************************/
/********************************************************************************************/
PROCEDURE pi-processa-transacao:
    DEFINE VARIABLE c-chave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vLogEfetivacaoNOK AS LOGICAL     NO-UNDO.

    //Sen�o tiver registro e n�o estiver locado sai do bloco

    LOG-MANAGER:WRITE-MESSAGE("INI - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).

    FIND FIRST es-api-export-spf
         WHERE ROWID(es-api-export-spf) = ROWID(bf-es-api-export-spf)
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    LOG-MANAGER:WRITE-MESSAGE("Antes Teste LOCK es-api-export-spf").

    IF  LOCKED es-api-export-spf THEN DO:
        LOG-MANAGER:WRITE-MESSAGE("Teste positivo para LOCKED").
        RETURN "_LOCKED".
    END.

    LOG-MANAGER:WRITE-MESSAGE("DEpois Teste LOCK es-api-export-spf").

    DO TRANSACTION:
        ASSIGN  es-api-export-spf.ind-situacao  = 1 //Processando
                es-api-export-spf.data-inicio   = NOW.
                //es-api-export-spf.cd-agente     = 999
                //es-api-export-spf.nm-appserv    = "Leon"
    END.

    ASSIGN c-erro = "".

    LOG-MANAGER:WRITE-MESSAGE("ID-MOVTO ANTES PROCESSAMENTO " + STRING(es-api-export-spf.id-movto)   ).
    LOG-MANAGER:WRITE-MESSAGE("vLogEfetivacaoNOK      "  +   STRING(vLogEfetivacaoNOK)          ).
    LOG-MANAGER:WRITE-MESSAGE("RETURN-VALUE           "  +   RETURN-VALUE                       ).
    LOG-MANAGER:WRITE-MESSAGE("ERROR-STATUS:ERROR     "  +   STRING(ERROR-STATUS:ERROR )        ).
    //LOG-MANAGER:WRITE-MESSAGE("c-chave                "  +   c-chave                            ).
    IF ERROR-STATUS:ERROR THEN
    DO:
        DO iErro = 1 TO ERROR-STATUS:NUM-MESSAGES:
            LOG-MANAGER:WRITE-MESSAGE("ERRO PROGRESS:  " + ERROR-STATUS:GET-MESSAGE(iErro) + "(" + STRING(ERROR-STATUS:GET-NUMBER(iErro)) + ")" ).
        END.
    END.

    ASSIGN  vLogEfetivacaoNOK = YES.
    blk_pedidos:
    DO  ON ERROR  UNDO blk_pedidos, LEAVE blk_pedidos
        ON QUIT   UNDO blk_pedidos, LEAVE blk_pedidos
        ON STOP   UNDO blk_pedidos, LEAVE blk_pedidos
        /*ON ENDKEY UNDO blk_pedidos, LEAVE blk_pedidos*/ : 

        /* ------ Executa progama espec�fico para o tipo de integra��o ------ */
        LOG-MANAGER:WRITE-MESSAGE("Estou na pi-processa-transacao - Antes de executar :" + STRING(es-api-param-spf.programa-integr)).

        RUN VALUE( es-api-param-spf.programa-integr ) (INPUT ROWID(es-api-export-spf),
                                                   OUTPUT c-erro /*,
                                                   OUTPUT c-chave  */
                                                   ) NO-ERROR.

        LOG-MANAGER:WRITE-MESSAGE("Estou na pi-processa-transacao - Depois de executar :" + STRING(es-api-param-spf.programa-integr)).

        ASSIGN  vLogEfetivacaoNOK = NO.
        /* ------ Gerencia retorno do processo -----*/

    END.

    LOG-MANAGER:WRITE-MESSAGE("ID-MOVTO DEPOIS PROCESSAMENTO " + STRING(es-api-export-spf.id-movto)   ).
    LOG-MANAGER:WRITE-MESSAGE("vLogEfetivacaoNOK      "  +   STRING(vLogEfetivacaoNOK)          ).
    LOG-MANAGER:WRITE-MESSAGE("c-erro                 "  +   c-erro                             ).
    LOG-MANAGER:WRITE-MESSAGE("RETURN-VALUE           "  +   RETURN-VALUE                       ).
    LOG-MANAGER:WRITE-MESSAGE("ERROR-STATIS:ERROR     "  +   STRING(ERROR-STATUS:ERROR )        ).
    //LOG-MANAGER:WRITE-MESSAGE("c-chave                "  +   c-chave                            ).
    IF ERROR-STATUS:ERROR THEN
    DO:
        DO iErro = 1 TO ERROR-STATUS:NUM-MESSAGES:
            LOG-MANAGER:WRITE-MESSAGE("ERRO PROGRESS:  " + ERROR-STATUS:GET-MESSAGE(iErro) + "(" + STRING(ERROR-STATUS:GET-NUMBER(iErro)) + ")" ).
        END.
    END.

    
    IF  vLogEfetivacaoNOK /* Erro Travamento de Registro */
    OR  (RETURN-VALUE = "NOK")
    THEN DO:
        DO TRANSACTION:
            //Tem Erro de negocio
            IF RETURN-VALUE = "NOK"
            THEN DO:
                ASSIGN  es-api-export-spf.data-fim      = NOW
                        es-api-export-spf.ind-situacao  = 2 // Processado
                        es-api-export-spf.cod-status    = 2 // Com erro
                        //es-api-export-spf.chave         = IF c-chave = "" THEN es-api-export-spf.chave ELSE c-chave
                        c-erro                      = "Houve erro ao processar transa��o. " + c-erro.
                    .
            END.
            ELSE IF vLogEfetivacaoNOK /* Erro Travamento de Registro */
            THEN DO:
                ASSIGN  es-api-export-spf.data-fim      = NOW
                        es-api-export-spf.ind-situacao  = 3 // Processado
                        //es-api-export-spf.chave         = IF c-chave = "" THEN es-api-export-spf.chave ELSE c-chave
                        c-erro                      = "Ocorreu erro progress que impediu a conclus�o da transa��o. " + c-erro.
                IF ERROR-STATUS:ERROR THEN
                DO:
                    DO iErro = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        c-erro                      = c-erro + CHR(10) + ERROR-STATUS:GET-MESSAGE(iErro) + "(" + STRING(ERROR-STATUS:GET-NUMBER(iErro)) + ")" .
                    END.
                END.
            END.
        END.
    END.
    ELSE DO:
        DO TRANSACTION:
            ASSIGN  es-api-export-spf.data-fim      = NOW
                    es-api-export-spf.ind-situacao  = 2
                    es-api-export-spf.cod-status    = 1
                    //es-api-export-spf.chave         = IF c-chave = "" THEN es-api-export-spf.chave ELSE c-chave
                    c-erro                      = "".

        END.
    END.
    RUN pi-gera-status (INPUT c-erro).
    LOG-MANAGER:WRITE-MESSAGE("PONTO-001 - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).


    RELEASE es-api-export-spf.
    LOG-MANAGER:WRITE-MESSAGE("FIM - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).

    RETURN "OK".
END PROCEDURE.


/********************************************************************************************/
/********************************************************************************************/
/********************************************************************************************/
PROCEDURE pi-gera-status:

    DEFINE INPUT PARAMETER c-erro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    FIND LAST es-api-export-log-spf NO-LOCK OF es-api-export-spf NO-ERROR.
    IF AVAIL es-api-export-log-spf THEN
        ASSIGN i-nr-seq = es-api-export-log-spf.nr-seq + 1.
    ELSE i-nr-seq = 1.

    CREATE es-api-export-log-spf.
    ASSIGN es-api-export-log-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr
           es-api-export-log-spf.id-movto       = es-api-export-spf.id-movto      
           es-api-export-log-spf.data-log       = NOW
           es-api-export-log-spf.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro
           es-api-export-log-spf.nr-seq         = i-nr-seq.

    RELEASE es-api-export-log-spf.

END PROCEDURE.


/********************************************************************************************/
/********************************************************************************************/
/********************************************************************************************/
PROCEDURE gerarLog:
DEFINE INPUT  PARAMETER cLog AS CHARACTER   NO-UNDO.

    LOG-MANAGER:WRITE-MESSAGE(cLog).

/*     MESSAGE clog                           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*     RUN gerarLogSFA(cLog). */

END PROCEDURE.



/* RUN gerarLog("In�cio Processo - esspf100ERP - V.1.0 - Exporta��o").                                                                */
/*                                                                                                                                    */
/* IF NOT CONNECTED("mgcad") THEN                                                                                                     */
/*     RUN esp/esspf100conn.p.                                                                                                        */
/*                                                                                                                                    */
/*                                                                                                                                    */
/* IF NOT CONNECTED("mgadm") THEN                                                                                                     */
/*     RUN esp/esspf100alias.p.                                                                                                       */
/*                                                                                                                                    */
/*                                                                                                                                    */
/* RUN pi-carrega-pend.                                                                                                               */
/* /* RUN pi-processa. */                                                                                                             */
/* RUN gerarLog("Processo conclu�do").                                                                                                */
/*                                                                                                                                    */
/*                                                                                                                                    */
/* PROCEDURE pi-processa:                                                                                                             */
/*                                                                                                                                    */
/*     RUN gerarLog("Vai processar registro").                                                                                        */
/*                                                                                                                                    */
/* /*     ASSIGN i-count = 0.                                                            */                                           */
/* /*     FOR EACH es-api-export-spf WHERE es-api-export-spf.ind-situacao = 1                    */                                           */
/* /*                              AND es-api-export-spf.cd-agente    = p-cd-agente          */                                           */
/* /*                              AND es-api-export-spf.nm-appserv   = p-nome-app  NO-LOCK, */                                           */
/* /*         FIRST es-api-param-spf NO-LOCK OF es-api-export-spf WHERE es-api-param-spf.ativo:      */                                           */
/*                                                                                                                                    */
/* /*         ASSIGN i-count = i-count + 1. */                                                                                        */
/*                                                                                                                                    */
/*         IF NOT AVAIL es-api-export-spf THEN                                                                                            */
/*         DO:                                                                                                                        */
/*             RUN gerarLog("ERRO: Registro n�o dispon�vel para processamento"  ).                                                    */
/*             RETURN "NOK".                                                                                                          */
/*         END.                                                                                                                       */
/*                                                                                                                                    */
/*         IF NOT AVAIL es-api-param-spf THEN                                                                                             */
/*         DO:                                                                                                                        */
/*             RUN gerarLog("ERRO: Parametro de execu��o n�o dispon�vel para processamento"  ).                                       */
/*             RETURN "NOK".                                                                                                          */
/*         END.                                                                                                                       */
/*                                                                                                                                    */
/*         RUN gerarLog("Programa de integra��o: " + SEARCH(es-api-param-spf.programa-integr ) ).                                         */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*         IF SEARCH(es-api-param-spf.programa-integr ) <> ? THEN                                                                         */
/*         DO:                                                                                                                        */
/*                                                                                                                                    */
/*             /* Comando FIND CURRENT aparentemente trava no AS */                                                                   */
/* /*             FIND CURRENT es-api-export-spf EXCLUSIVE-LOCK NO-ERROR. */                                                              */
/*             FOR FIRST B-es-api-export-spf EXCLUSIVE-LOCK WHERE                                                                         */
/*                       ROWID(b-es-api-export)        = ROWID(es-api-export):                                                        */
/*                                                                                                                                    */
/*                 ASSIGN b-es-api-export-spf.data-inicio  = NOW.                                                                         */
/*                                                                                                                                    */
/*                 RUN gerarLog("Chamando programa de integra��o").                                                                   */
/*                                                                                                                                    */
/*                 /* ------ Executa progama espec�fico para o tipo de integra��o ------ */                                           */
/*                 RUN VALUE( es-api-param-spf.programa-integr ) (INPUT ROWID(b-es-api-export),                                           */
/*                                                            OUTPUT c-erro,                                                          */
/*                                                            OUTPUT c-chave) NO-ERROR.                                               */
/*                 ASSIGN b-es-api-export-spf.data-fim     = NOW                                                                          */
/*                        b-es-api-export-spf.ind-situacao = 2 .                                                                          */
/*                                                                                                                                    */
/*                 RUN gerarLog("Depois do programa de integra��o").                                                                  */
/*                                                                                                                                    */
/*                 /* ------ Gerencia retorno do processo -----*/                                                                     */
/*                 IF c-erro = "" THEN                                                                                                */
/*                     ASSIGN b-es-api-export-spf.cod-status = 1.                                                                         */
/*                 ELSE ASSIGN b-es-api-export-spf.cod-status = 2.                                                                        */
/*                                                                                                                                    */
/*                 RUN pi-gera-status (INPUT c-erro).                                                                                 */
/*                                                                                                                                    */
/*             END.                                                                                                                   */
/*                                                                                                                                    */
/*         END.                                                                                                                       */
/*                                                                                                                                    */
/*                                                                                                                                    */
/* /*         IF /*i-count = 10 */ i-count > 1 THEN LEAVE . */                                                                        */
/* /*                                                       */                                                                        */
/* /*     END.                                              */                                                                        */
/* END PROCEDURE.                                                                                                                     */
/*                                                                                                                                    */
/* PROCEDURE pi-carrega-pend:                                                                                                         */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*     verif-tipo:                                                                                                                    */
/*     FOR EACH es-api-param-spf NO-LOCK WHERE es-api-param-spf.ind-tipo-trans = 2                                                            */
/*                                     AND es-api-param-spf.cd-sistema     = p-sistema                                                    */
/*                                     AND es-api-param-spf.ativo:                                                                        */
/*                                                                                                                                    */
/*         /* ------ Verifica se existe agenda de integra��o v�lida ----*/                                                            */
/* /*         IF fncAgendaAtiva(es-api-param-spf.cd-tipo-integr) THEN NEXT. */                                                            */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*         i-count = 0.                                                                                                               */
/*                                                                                                                                    */
/*         /* ---- Localiza itens novos ----- */                                                                                      */
/*         proc-pend:                                                                                                                 */
/*         FOR EACH es-api-export-spf OF es-api-param-spf WHERE es-api-export-spf.ind-situacao = 0                                                */
/*                                                  AND es-api-export-spf.cd-agente    = 0                                                */
/*                                                  AND es-api-export-spf.nm-appserv   = "" NO-LOCK:                                      */
/*             i-count = i-count + 1.                                                                                                 */
/*                                                                                                                                    */
/*             RUN marcarPendenciaParaExecucao.                                                                                       */
/*             RUN pi-processa.                                                                                                       */
/*                                                                                                                                    */
/*             IF /*i-count = 10 */ i-count >= 1 THEN LEAVE proc-pend.                                                                */
/*         END.                                                                                                                       */
/*                                                                                                                                    */
/* /*         RUN gerarLog("EXPORTA��O - Transa��o aberta entre registros pendentes e presos? " + STRING(TRANSACTION, "Sim/N�o")). */ */
/*                                                                                                                                    */
/*         i-count = 0.                                                                                                               */
/*                                                                                                                                    */
/*         /* ------ Verifica registros presos ----------*/                                                                           */
/*         proc-perd:                                                                                                                 */
/*         FOR EACH es-api-export-spf OF es-api-param-spf WHERE es-api-export-spf.ind-situacao = 1                                                */
/*                                                  /* AND es-api-export-spf.cd-agente    = 0  */                                         */
/*                                                  /* AND es-api-export-spf.nm-appserv   = "" */ NO-LOCK:                                */
/*             i-count = i-count + 1.                                                                                                 */
/*                                                                                                                                    */
/*              RUN gerarLog("Encontrou registro preso").                                                                             */
/*                                                                                                                                    */
/*             RUN marcarPendenciaParaExecucao.                                                                                       */
/*             RUN pi-processa.                                                                                                       */
/*                                                                                                                                    */
/*             IF /*i-count = 10 */ i-count >= 1 THEN LEAVE proc-perd.                                                                */
/*         END.                                                                                                                       */
/*     END.                                                                                                                           */
/*                                                                                                                                    */
/* END PROCEDURE.                                                                                                                     */
/*                                                                                                                                    */
/* PROCEDURE marcarPendenciaParaExecucao:                                                                                             */
/*                                                                                                                                    */
/*     DO TRANSACTION:                                                                                                                */
/*         FIND CURRENT es-api-export-spf EXCLUSIVE-LOCK NO-ERROR NO-WAIT.                                                                */
/*         IF LOCKED(es-api-export) THEN                                                                                              */
/*             NEXT.                                                                                                                  */
/*         ELSE                                                                                                                       */
/*         DO:                                                                                                                        */
/*             IF AVAIL es-api-export-spf THEN                                                                                            */
/*                 ASSIGN es-api-export-spf.ind-situacao = 1                                                                              */
/*                        es-api-export-spf.cd-agente    = p-cd-agente                                                                    */
/*                        es-api-export-spf.nm-appserv   = p-nome-app.                                                                    */
/*                                                                                                                                    */
/*             IF AVAIL es-api-export-spf THEN                                                                                            */
/*                 FIND CURRENT es-api-export-spf NO-LOCK NO-ERROR.                                                                       */
/*                                                                                                                                    */
/*         END.                                                                                                                       */
/*     END.                                                                                                                           */
/*                                                                                                                                    */
/* END PROCEDURE.                                                                                                                     */
/*                                                                                                                                    */
/* PROCEDURE pi-gera-status:                                                                                                          */
/*                                                                                                                                    */
/*     DEFINE INPUT PARAMETER c-erro AS CHARACTER NO-UNDO.                                                                            */
/*                                                                                                                                    */
/*     DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.                                                                                   */
/*                                                                                                                                    */
/*     FIND LAST es-api-export-log-spf NO-LOCK OF es-api-export-spf NO-ERROR.                                                                 */
/*     IF AVAIL es-api-export-log-spf THEN                                                                                                */
/*         ASSIGN i-nr-seq = es-api-export-log-spf.nr-seq + 1.                                                                            */
/*     ELSE i-nr-seq = 1.                                                                                                             */
/*                                                                                                                                    */
/*     CREATE es-api-export-log-spf.                                                                                                      */
/*     ASSIGN es-api-export-log-spf.ind-tipo-trans = es-api-export-spf.ind-tipo-trans                                                         */
/*            es-api-export-log-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr                                                         */
/*            es-api-export-log-spf.id-movto       = es-api-export-spf.id-movto                                                               */
/*            es-api-export-log-spf.data-log       = NOW                                                                                  */
/*            es-api-export-log-spf.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro                     */
/*            es-api-export-log-spf.nr-seq         = i-nr-seq.                                                                            */
/*                                                                                                                                    */
/*     RELEASE es-api-export-log-spf.                                                                                                     */
/*                                                                                                                                    */
/* END PROCEDURE.                                                                                                                     */
/*                                                                                                                                    */
/* FUNCTION fncAgendaAtiva RETURN LOGICAL                                                                                             */
/*     ( p-es-api-param-spf AS INTEGER ) :                                                                                                */
/*                                                                                                                                    */
/*     FIND FIRST es-api-schedule WHERE es-api-schedule.cd-tipo-integr = p-es-api-param-spf                                               */
/*                                  AND es-api-schedule.ativo                                                                         */
/*                                  AND es-api-schedule.data-inicio <= TODAY                                                          */
/*                                  AND es-api-schedule.data-fim    >= TODAY NO-LOCK NO-ERROR.                                        */
/*     IF AVAIL es-api-schedule THEN DO:                                                                                              */
/*                                                                                                                                    */
/*         IF es-api-schedule.hora-inicio <= TIME AND                                                                                 */
/*             es-api-schedule.hora-fim >= TIME THEN RETURN NO.                                                                       */
/*         ELSE RETURN YES.                                                                                                           */
/*     END.                                                                                                                           */
/*     ELSE RETURN NO.                                                                                                                */
/*                                                                                                                                    */
/* END FUNCTION.                                                                                                                      */
/*                                                                                                                                    */
/* PROCEDURE gerarLog:                                                                                                                */
/* DEFINE INPUT  PARAMETER cLog AS CHARACTER   NO-UNDO.                                                                               */
/*                                                                                                                                    */
/*     LOG-MANAGER:WRITE-MESSAGE(cLog).                                                                                               */
/*                                                                                                                                    */
/* /*     RUN gerarLogSFA(cLog). */                                                                                                   */
/*                                                                                                                                    */
/* END PROCEDURE.                                                                                                                     */