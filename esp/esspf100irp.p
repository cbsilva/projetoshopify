/*
 *-----------------------------------------------------------------------------`
 *  PROGRAMA        esspf100irp
 *  OBJETIVO        API Integraá‰es JSON  - baseado no esspf001 (hist¢rico abaixo)
 *  AUTOR           TOTVS - LASF
 *  DATA            12/2020
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINIÄÂES
 *
 *------------------------------------------------------------------------------
 */
{include/i-prgvrs.i esspf100irp 2.09.00.002}

{lib/utilidades.i}
/* {lib/logsfa.i} */


/* ------ Deiniá∆o de ParÉmetros ------*/
DEFINE INPUT PARAMETER pEndPoint     AS INTEGER   NO-UNDO.

/* ------- Definiá∆o de vari†veis ----- */
DEF VAR c-erro AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-es-api-import-spf FOR es-api-import-spf.

DEFINE VARIABLE iErro AS INTEGER     NO-UNDO.


/*
 *------------------------------------------------------------------------------
 *
 *                                BLOCO PRINCIPAL
 *
 *------------------------------------------------------------------------------
 */

/* RUN abrirLog("//fenix/ERP/camil/teste-verde/especificos/lasf/roboSFA/esspf001irp_log_" + stamp() + ".txt"). */
LOG-MANAGER:WRITE-MESSAGE("In°cio Processo - esspf100IRP - V.1.0 - MACMILLAN - Importaá∆o").


/* MESSAGE "LASF MAC - PAUSE"                    */
/*     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
//DO iLoop = 1 TO 5:

    LOG-MANAGER:WRITE-MESSAGE("LOOP = " + STRING(iLoop)).
    RUN piNewCarregaPend.

//END.

/* // TRIM agentes                                                                                                                                                                                            */
/* LOG-MANAGER:WRITE-MESSAGE("Vai preparar o TRIM").                                                                                                                                                          */
/* DEFINE VARIABLE cComando        AS CHARACTER   NO-UNDO.                                                                                                                                                    */
/* FOR EACH es-api-app-spfserver WHERE es-api-app-spfserver.cd-sistema = p-sistema NO-LOCK :                                                                                                                          */
/*     ASSIGN  cComando = SUBSTITUTE("asbman -i &1 -trim &2 >> &3", es-api-app-spfserver.nome-appserv, STRING(es-api-app-spfserver.agentes-appserver), "//fenix/erp/camil/teste-amarelo/log_appserver/AgentLog.txt"). */
/*     LOG-MANAGER:WRITE-MESSAGE("esspf100IRP - TRIM agentes - commando " + cComando).                                                                                                                        */
/*     OS-COMMAND SILENT VALUE(cComando).                                                                                                                                                                     */
/* END.                                                                                                                                                                                                       */

RUN checarPersistentes.


/*
 *------------------------------------------------------------------------------
 *
 *                                PROCEDURES
 *
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE piNewCarregaPend:
    verif-tipo:
    FOR  EACH es-api-param-spf NO-LOCK
        WHERE 
              //es-api-param-spf.ind-tipo-trans = p-ind-trans 
          //AND es-api-param-spf.cd-sistema     = p-sistema   // SalesForce
              es-api-param-spf.cd-tipo-integr       = pEndPoint
          AND es-api-param-spf.ativo:
        
        LOG-MANAGER:WRITE-MESSAGE("Verificando pendencias - end Point : " +  tratarString(STRING(pEndPoint))).
        IF SEARCH(es-api-param-spf.programa-integr ) = ? THEN NEXT.

        LOG-MANAGER:WRITE-MESSAGE("INI - FOR EACH es-api-param-spf").
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.ind-tipo-trans   " + tratarString(STRING(es-api-param-spf.ind-tipo-trans  )) ).
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.cd-sistema       " + tratarString(STRING(es-api-param-spf.cd-sistema      )) ).
        LOG-MANAGER:WRITE-MESSAGE("es-api-param-spf.cd-tipo-integr   " + tratarString(STRING(es-api-param-spf.cd-tipo-integr  )) ).
         
        IF es-api-param-spf.tip-integracao = 2 // es-api-param-spf.path-integr BEGINS "\\" OR SUBSTRING(es-api-param-spf.path-integr, 2, 1) = ":"  
            THEN
        DO:

            RUN VALUE( es-api-param-spf.programa-integr ) (INPUT ROWID(es-api-param-spf)) NO-ERROR.

        END.

        //ELSE
        IF es-api-param-spf.tip-integracao = 1 THEN 
        DO:


            proc-pend_0:
            FOR EACH  bf-es-api-import-spf 
                WHERE bf-es-api-import-spf.cd-tipo-integr   = es-api-param-spf.cd-tipo-integr
                  AND bf-es-api-import-spf.ind-situacao     = 0 //  Pendente
            
                NO-LOCK:
            
                LOG-MANAGER:WRITE-MESSAGE("INI - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                                + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
                RUN pi-processa-transacao.
                IF RETURN-VALUE = "_LOCKED" THEN
                    NEXT proc-pend_0.
            
                LOG-MANAGER:WRITE-MESSAGE("FIM - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                                + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
                LEAVE proc-pend_0.
            
            END.
            //Processa somente os registros que ocorreram erro progress
            proc-pend_1:
            FOR EACH bf-es-api-import-spf 
                WHERE bf-es-api-import-spf.cd-tipo-integr   = es-api-param-spf.cd-tipo-integr
                  AND bf-es-api-import-spf.ind-situacao     = 3 //  Erro Progress
                NO-LOCK:
            
                LOG-MANAGER:WRITE-MESSAGE("INI - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                                + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
                RUN pi-processa-transacao.
                IF RETURN-VALUE = "_LOCKED" THEN
                    NEXT proc-pend_1.
            
                LOG-MANAGER:WRITE-MESSAGE("FIM - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                                + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
                LEAVE proc-pend_1.
            
            END.

            //Reprocessamento autom†tico
            proc-pend_3:
            FOR EACH bf-es-api-import-spf 
                WHERE bf-es-api-import-spf.cd-tipo-integr   = es-api-param-spf.cd-tipo-integr
                  AND bf-es-api-import-spf.cod-status          = 2
                  AND bf-es-api-import-spf.data-movto        >= DATETIME(TODAY - 30) 

                NO-LOCK
                
                BY bf-es-api-import-spf.data-fim

                :


                LOG-MANAGER:WRITE-MESSAGE("INI REPROCESS. - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                              + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                              + " Status:" + STRING(bf-es-api-import-spf.cod-status)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
            
                RUN pi-processa-transacao.
                IF RETURN-VALUE = "_LOCKED" THEN
                    NEXT proc-pend_3.
            
                LOG-MANAGER:WRITE-MESSAGE("FIM REPROCESS. - Processando CD-TIPO-TRANS:" + STRING(bf-es-api-import-spf.cd-tipo-integr) 
                                + " Situaá∆o:" + STRING(bf-es-api-import-spf.ind-situacao)
                                + " Chave:" + STRING(bf-es-api-import-spf.chave)
                                + " Transaá∆o ativa:" + STRING(TRANSACTION)).
            
                LEAVE proc-pend_3.
            
            END.


        END.


        LOG-MANAGER:WRITE-MESSAGE("FIM - FOR EACH es-api-param-spf").

    END.



END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE pi-processa-transacao:
    DEFINE VARIABLE c-chave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vLogEfetivacaoNOK AS LOGICAL     NO-UNDO.

    //Sen∆o tiver registro e n∆o estiver locado sai do bloco

    LOG-MANAGER:WRITE-MESSAGE("INI - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).

    FIND FIRST es-api-import-spf
         WHERE ROWID(es-api-import-spf) = ROWID(bf-es-api-import-spf)
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    LOG-MANAGER:WRITE-MESSAGE("Antes Teste LOCK es-api-import").

    IF  LOCKED es-api-import-spf THEN DO:
        LOG-MANAGER:WRITE-MESSAGE("Teste positivo para LOCKED").
        RETURN "_LOCKED".
    END.

    LOG-MANAGER:WRITE-MESSAGE("DEpois Teste LOCK es-api-import").

    DO TRANSACTION:
        ASSIGN  es-api-import-spf.ind-situacao  = 1 //Processando
                es-api-import-spf.data-inicio   = NOW.
                //es-api-import-spf.cd-agente     = 999
                //es-api-import-spf.nm-appserv    = "Leon"
    END.

    ASSIGN c-erro = "".

    LOG-MANAGER:WRITE-MESSAGE("ID-MOVTO ANTES PROCESSAMENTO " + STRING(es-api-import-spf.id-movto)   ).
    LOG-MANAGER:WRITE-MESSAGE("vLogEfetivacaoNOK      "  +   STRING(vLogEfetivacaoNOK)          ).
    LOG-MANAGER:WRITE-MESSAGE("RETURN-VALUE           "  +   RETURN-VALUE                       ).
    LOG-MANAGER:WRITE-MESSAGE("ERROR-STATUS:ERROR     "  +   STRING(ERROR-STATUS:ERROR )        ).
    LOG-MANAGER:WRITE-MESSAGE("c-chave                "  +   c-chave                            ).
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

        /* ------ Executa progama espec°fico para o tipo de integraá∆o ------ */
        LOG-MANAGER:WRITE-MESSAGE("Estou na pi-processa-transacao - Antes de executar :" + STRING(es-api-param-spf.programa-integr)).

        RUN VALUE( es-api-param-spf.programa-integr ) (INPUT ROWID(es-api-import-spf),
                                                   OUTPUT c-erro,
                                                   OUTPUT c-chave) NO-ERROR.

        LOG-MANAGER:WRITE-MESSAGE("Estou na pi-processa-transacao - Depois de executar :" + STRING(es-api-param-spf.programa-integr)).

        ASSIGN  vLogEfetivacaoNOK = NO.
        /* ------ Gerencia retorno do processo -----*/

    END.

    LOG-MANAGER:WRITE-MESSAGE("ID-MOVTO DEPOIS PROCESSAMENTO " + STRING(es-api-import-spf.id-movto)   ).
    LOG-MANAGER:WRITE-MESSAGE("vLogEfetivacaoNOK      "  +   STRING(vLogEfetivacaoNOK)          ).
    LOG-MANAGER:WRITE-MESSAGE("c-erro                 "  +   c-erro                             ).
    LOG-MANAGER:WRITE-MESSAGE("RETURN-VALUE           "  +   RETURN-VALUE                       ).
    LOG-MANAGER:WRITE-MESSAGE("ERROR-STATIS:ERROR     "  +   STRING(ERROR-STATUS:ERROR )        ).
    LOG-MANAGER:WRITE-MESSAGE("c-chave                "  +   c-chave                            ).
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
                ASSIGN  es-api-import-spf.data-fim      = NOW
                        es-api-import-spf.ind-situacao  = 2 // Processado
                        es-api-import-spf.cod-status    = 2 // Com erro
                        es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
                        c-erro                      = "Houve erro ao processar transaá∆o. " + c-erro.
                    .
            END.
            ELSE IF vLogEfetivacaoNOK /* Erro Travamento de Registro */
            THEN DO:
                ASSIGN  es-api-import-spf.data-fim      = NOW
                        es-api-import-spf.ind-situacao  = 3 // Houve erro de execuá∆o / reprocesssar
                        es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
                        c-erro                      = "Ocorreu erro progress que impediu a conclus∆o da transaá∆o. " + c-erro.
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
            ASSIGN  es-api-import-spf.data-fim      = NOW
                    es-api-import-spf.ind-situacao  = 2
                    es-api-import-spf.cod-status    = 1
                    es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
                    c-erro                      = "".

        END.
    END.

    // Tratametno erro tÇcnico/LOCKS
    IF INDEX(c-erro, "#ERROTEC") > 0
    THEN DO:
        ASSIGN  es-api-import-spf.data-fim      = NOW
                es-api-import-spf.ind-situacao  = 3 // Erro TÇcnico - Reprocessar
                es-api-import-spf.cod-status    = 0 // N∆o Processado / Na Fila
                es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
                c-erro                      = "Houve erro tÇcnico ao processar transaá∆o. " + c-erro.
            .
    END.

    RUN pi-gera-status (INPUT c-erro).
    LOG-MANAGER:WRITE-MESSAGE("PONTO-001 - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).

/*     IF  vLogEfetivacaoNOK /* Erro Travamento de Registro */                                                                                                                               */
/*         OR  (RETURN-VALUE = "NOK")                                                                                                                                                        */
/*         /*OR  (RETURN-VALUE = "ERROTEC")*/                                                                                                                                                */
/*     THEN DO:                                                                                                                                                                              */
/*         DO TRANSACTION:                                                                                                                                                                   */
/*             //Tem Erro de negocio                                                                                                                                                         */
/*             IF RETURN-VALUE = "NOK" AND INDEX(c-erro, "#ERROTEC") = 0                                                                                                                      */
/*             THEN DO:                                                                                                                                                                      */
/*                 ASSIGN  es-api-import-spf.data-fim      = NOW                                                                                                                                 */
/*                         es-api-import-spf.ind-situacao  = 2 // Processado                                                                                                                     */
/*                         es-api-import-spf.cod-status    = 2 // Com erro                                                                                                                       */
/*                         es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave                                                                               */
/*                         c-erro                      = "Houve erro ao processar transaá∆o. " + c-erro.                                                                                       */
/*                     .                                                                                                                                                                     */
/*             END.                                                                                                                                                                          */
/*             ELSE IF vLogEfetivacaoNOK OR  INDEX(c-erro, "#ERROTEC") > 0 /* Erro Travamento de Registro */                                                                                  */
/*             THEN DO:                                                                                                                                                                      */
/*                 ASSIGN  es-api-import-spf.data-fim      = NOW                                                                                                                                 */
/*                         es-api-import-spf.ind-situacao  = 3 // Processado                                                                                                                     */
/*                         es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave                                                                               */
/*                         c-erro                      = "Ocorreu erro progress que impediu a conclus∆o da transaá∆o. " + c-erro + CHR(10) + "Valor Retornado: " + tratarString(RETURN-VALUE). */
/*                 IF ERROR-STATUS:ERROR THEN                                                                                                                                                */
/*                 DO:                                                                                                                                                                       */
/*                     DO iErro = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                                                                            */
/*                         c-erro                      = c-erro + CHR(10) + ERROR-STATUS:GET-MESSAGE(iErro) + "(" + STRING(ERROR-STATUS:GET-NUMBER(iErro)) + ")" .                             */
/*                     END.                                                                                                                                                                  */
/*                 END.                                                                                                                                                                      */
/*             END.                                                                                                                                                                          */
/*         END.                                                                                                                                                                              */
/*     END.                                                                                                                                                                                  */
/*     ELSE DO:                                                                                                                                                                              */
/*         DO TRANSACTION:                                                                                                                                                                   */
/*             ASSIGN  es-api-import-spf.data-fim      = NOW                                                                                                                                     */
/*                     es-api-import-spf.ind-situacao  = 2                                                                                                                                       */
/*                     es-api-import-spf.cod-status    = 1                                                                                                                                       */
/*                     es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave                                                                                   */
/*                     c-erro                      =  "Registro integrado com sucesso".                                                                                                       */
/*             /*IF RETURN-VALUE <> "" AND RETURN-VALUE <> "OK" THEN*/                                                                                                                       */
/*                 ASSIGN c-erro = c-erro + "Valor Retornado: " + tratarString(RETURN-VALUE) .                                                                                                 */
/*                                                                                                                                                                                           */
/*         END.                                                                                                                                                                              */
/*     END.                                                                                                                                                                                  */
/*     RUN pi-gera-status (INPUT c-erro).                                                                                                                                                     */
/*     LOG-MANAGER:WRITE-MESSAGE("PONTO-001 - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).                                                                                         */

    RELEASE es-api-import-spf.
    LOG-MANAGER:WRITE-MESSAGE("FIM - METODO:pi-processa-transacao:" + STRING(TRANSACTION)).

    RETURN "OK".
END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE pi-gera-status:
DEFINE VARIABLE daAux AS DATETIME        NO-UNDO.

    DEFINE INPUT PARAMETER c-erro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    FIND LAST es-api-import-log-spf NO-LOCK OF es-api-import-spf NO-ERROR.
    IF AVAIL es-api-import-log-spf THEN
        ASSIGN i-nr-seq = es-api-import-log-spf.nr-seq + 1.
    ELSE i-nr-seq = 1.

    daAux  = NOW.
    IF NOT AVAIL es-api-import-log-spf OR DATE(es-api-import-log-spf.data-log) <> DATE(daAux) THEN
    DO:
        CREATE es-api-import-log-spf.
        ASSIGN es-api-import-log-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr
               es-api-import-log-spf.id-movto       = es-api-import-spf.id-movto      
               es-api-import-log-spf.data-log       = NOW
               es-api-import-log-spf.nr-seq         = i-nr-seq. 
    END.
    ELSE
        FIND CURRENT es-api-import-log-spf EXCLUSIVE-LOCK.
        

    ASSIGN es-api-import-log-spf.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro 
           es-api-import-log-spf.data-log       = daAux.

/*     CREATE es-api-import-log-spf.                                                                                  */
/*     ASSIGN es-api-import-log-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr                                     */
/*            es-api-import-log-spf.id-movto       = es-api-import-spf.id-movto                                           */
/*            es-api-import-log-spf.data-log       = NOW                                                              */
/*            es-api-import-log-spf.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro */
/*            es-api-import-log-spf.nr-seq         = i-nr-seq.                                                        */

    RELEASE es-api-import-log-spf.

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE gerarLog:
DEFINE INPUT  PARAMETER cLog AS CHARACTER   NO-UNDO.

    LOG-MANAGER:WRITE-MESSAGE(cLog).

/*     MESSAGE clog                           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*     RUN gerarLogSFA(cLog). */

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */

PROCEDURE checarPersistentes:

DEFINE VARIABLE hProcedureHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE hAux AS HANDLE      NO-UNDO.
DEFINE VARIABLE cProgramName AS CHARACTER NO-UNDO.
    

    LOG-MANAGER:WRITE-MESSAGE("INICIO - Tratando procedures persistentes").
    LOG-MANAGER:WRITE-MESSAGE("LISTANDO...").
    
    ASSIGN
        hProcedureHandle = SESSION:FIRST-PROCEDURE NO-ERROR.
    
    DO WHILE VALID-HANDLE(hProcedureHandle):
        LOG-MANAGER:WRITE-MESSAGE("Procedure: " + STRING(hProcedureHandle:FILE-NAME)).
        hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.
    END.

    /*
    LOG-MANAGER:WRITE-MESSAGE("EXCLUINDO...").
    ASSIGN
        hProcedureHandle = SESSION:FIRST-PROCEDURE NO-ERROR.
    
    DO WHILE VALID-HANDLE(hProcedureHandle):
        IF VALID-HANDLE(hProcedureHandle) AND NOT hProcedureHandle:FILE-NAME MATCHES "*esspf100*" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE("Excluindo procedure: " + STRING(hProcedureHandle:FILE-NAME)).
            hAux = hProcedureHandle.
            hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.
            DELETE PROCEDURE hAux.            
        END.
        ELSE
            hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.

    END.
      */
      
    LOG-MANAGER:WRITE-MESSAGE("FIM - Tratando procedures persistentes").

END PROCEDURE.
