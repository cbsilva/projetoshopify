/************************************************************************
** Programa: twp/tw-nota-fiscal.p                                      **
** Data....: 08/11/2011                                                **
** Autor...: Hernandez Aquino                                          **
** Objetivo: Trigger de write para atualizar o faturamento             **
*************************************************************************
** Alterac.: 03/07/2013 - Controle para PNLD                           **
**           03/11/2014 - Controle WebSales                            **
**            26/11/2015 - Criar controle na Trigger para empresa Springer   **
**                       onde dever† ser digitado os dados de projeto        **
*************************************************************************/
DEF PARAMETER BUFFER p-table FOR nota-fiscal.
DEF PARAMETER BUFFER p-old-table FOR nota-fiscal.
{utp/ut-glob.i}


/* 26/11/2015 - */
DEFINE NEW GLOBAL SHARED VARIABLE gv_sn_cod_projeto AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE gv_sn_cod_wbs     AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE gv_sn_cod_icp     AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE gv_sn_controle    AS LOGICAL.

DEF BUFFER b-web-ped-venda FOR web-ped-venda.
/* Trigger da Springer */
IF i-ep-codigo-usuario = "2" THEN DO: /* Trocar para o codigo da Springer */

/* 19/01/2016 feito direto na it-nota-fisc
    /* Ir† solicitar a digitaá∆o caso seja novo registro */
    IF NEW(p-table) THEN DO:

        ASSIGN gv_sn_controle = FALSE. /* Controle para n∆o solicitar a digitaá∆o no contas a pagar */

        FIND sn_nota_fiscal 
            WHERE sn_nota_fiscal.cod_empresa  = i-ep-codigo-usuario
              AND sn_nota_fiscal.cod-estabel  = p-table.cod-estabel
              AND sn_nota_fiscal.serie        = p-table.serie
              AND sn_nota_fiscal.nr-nota-fis  = p-table.nr-nota-fis
            NO-ERROR NO-WAIT.

        IF NOT AVAIL sn_nota_fiscal THEN DO:
            CREATE sn_nota_fiscal.
            ASSIGN sn_nota_fiscal.cod_empresa  = i-ep-codigo-usuario 
                   sn_nota_fiscal.cod-estabel  = p-table.cod-estabel 
                   sn_nota_fiscal.serie        = p-table.serie       
                   sn_nota_fiscal.nr-nota-fis  = p-table.nr-nota-fis.
        END.
        ASSIGN sn_nota_fiscal.cod_projeto = gv_sn_cod_projeto 
               sn_nota_fiscal.cod_wbs     = gv_sn_cod_wbs     
               sn_nota_fiscal.cod_icp     = gv_sn_cod_icp    
               sn_nota_fiscal.cod-usuario = c-seg-usuario
               sn_nota_fiscal.datahora    = NOW.

        IF AVAIL sn_nota_fiscal THEN RELEASE sn_nota_fiscal.
    END.

    */
END.

/* Trigger da Macmillan */
ELSE DO:
    /*********************************************/
    /* Alterado em 16/06/2015 - Migraá∆o TOTVS12 */
    /*********************************************/
    {include/pnld_movimento.i}

    DEF VAR c-msg          AS CHAR.
    DEF VAR l-erro         AS LOG.
    DEF VAR c-docto        AS CHAR.
    DEF VAR i-cod-emitente AS INT.
    DEF VAR c-nome-arquivo AS CHAR.

    DEF BUFFER bf_it-nota-fisc FOR it-nota-fisc.
    IF NEW(p-table) AND p-table.dt-cancela = ? THEN DO:

       IF CAN-FIND ( FIRST es-natur-oper WHERE es-natur-oper.nat-operacao = p-table.nat-operacao 
                                           AND ( es-natur-oper.u-dec-1    = 1 
                                            OR   es-natur-oper.u-dec-1    = 2  
                                            OR   es-natur-oper.u-dec-1    = 3 ) ) THEN DO:

           ASSIGN
           c-docto        = p-table.nr-nota-fis
           i-cod-emitente = p-table.cod-emitente.

           RUN ftp/esft002a.p ( INPUT  ROWID ( p-table ),
                                INPUT  NO,
                                OUTPUT c-msg,
                                OUTPUT l-erro ).

           /* log */
            FIND FIRST param-nf-estab NO-LOCK NO-ERROR.
            IF AVAIL param-nf-estab THEN DO:
                IF param-nf-estab.idi-tip-emis-amb-sefaz = 1 THEN
                   /* Cadastro no programa MAC0270 */
                   RUN include/param_prog.p (INPUT "TW-NOTA-FISCAL",
                                             INPUT "c-nome-arquivo",
                                             OUTPUT c-nome-arquivo).
                ELSE
                   RUN include/param_prog.p (INPUT "TW-NOTA-FISCAL-TST",
                                             INPUT "c-nome-arquivo",
                                             OUTPUT c-nome-arquivo).
            END.

           OUTPUT TO VALUE(c-nome-arquivo) NO-CONVERT APPEND.

           PUT UNFORMAT
           string(TODAY,"99/99/9999") ";" 
           string(TIME, "hh:mm:ss")   ";"
           p-table.nat-operacao       ";"
           c-docto                    ";"
           i-cod-emitente             ";"
           l-erro                     ";"
           c-msg                      ";"
           SKIP.

           OUTPUT CLOSE.

       END. /* IF CAN-FIND ( FIRST */

    END.

    /* Ajsute de volume na nf-e 20/11/2012 */
    IF AVAIL p-table AND 
        (p-table.nat-operacao = "1903NF" OR
         p-table.nat-operacao = "3102II" OR
         p-table.nat-operacao = "3102CC" OR
         p-table.nat-operacao = "3949PS" OR
         p-table.nat-operacao = "1949IN"
         ) THEN DO:
        FIND FIRST nota-embal NO-LOCK
            WHERE nota-embal.cod-estabel = p-table.cod-estabel
              AND nota-embal.serie = p-table.serie
              AND nota-embal.nr-nota-fis = p-table.nr-nota-fis NO-ERROR.
        IF NOT AVAIL nota-embal THEN DO:
            CREATE nota-embal.
            ASSIGN 
                nota-embal.cod-estabel = p-table.cod-estabel
                nota-embal.serie       = p-table.serie            
                nota-embal.nr-nota-fis = p-table.nr-nota-fis
                nota-embal.sigla-emb   = "PAD"
                nota-embal.qt-volumes  = 1.
        END.
    END.

    /* 03/07/2013 - Controle para PNLD */
    /* Nova nota fiscal, registrar dados no PNLD */
    IF NEW(p-table) THEN DO:
        FOR EACH pnld_palete EXCLUSIVE-LOCK
           WHERE pnld_palete.nome-abrev = p-table.nome-ab-cli
             AND pnld_palete.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST pnld_isbn_objeto NO-LOCK
                 WHERE pnld_isbn_objeto.cod_objeto   = pnld_palete.cod_objeto 
                   AND pnld_isbn_objeto.cod_programa = pnld_palete.cod_programa NO-ERROR.

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = pnld_palete.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = pnld_palete.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = pnld_palete.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = pnld_isbn_objeto.it-codigo NO-ERROR.

            IF NOT AVAIL pnld_isbn_objeto THEN NEXT.
            IF NOT AVAIL bf_it-nota-fisc THEN NEXT.

            FOR EACH pnld_encomenda
                WHERE pnld_encomenda.num_palete = pnld_palete.num_palete:
                /* inclui o registro de movimento das Encomendas */
                RUN pi_pnld_movimento (INPUT "ENCOMENDA",
                                       INPUT TODAY,
                                       INPUT "FAT",
                                       INPUT pnld_encomenda.num_palete,
                                       INPUT pnld_encomenda.num_encomenda,
                                       INPUT pnld_encomenda.num_encomenda,
                                       INPUT pnld_encomenda.qtde_objetos,
                                       INPUT 1,
                                       INPUT pnld_encomenda.peso_liquido,
                                       INPUT 0,
                                       INPUT pnld_encomenda.vlr_encomenda).

                ASSIGN pnld_encomenda.pnld_status  = 6
                       pnld_encomenda.cod-estabel  = p-table.cod-estabel
                       pnld_encomenda.serie        = p-table.serie
                       pnld_encomenda.nr-nota-fisc = p-table.nr-nota-fis
                       pnld_encomenda.nr-seq-fat   = bf_it-nota-fisc.nr-seq-fat.
            END.

            /* inclui o registro de movimento do palete */
            RUN pi_pnld_movimento (INPUT "PALETE",
                                   INPUT TODAY,
                                   INPUT "FAT",
                                   INPUT pnld_palete.num_palete,
                                   INPUT pnld_palete.encomenda_ini,
                                   INPUT pnld_palete.encomenda_fim,
                                   INPUT pnld_palete.qtde_objetos,
                                   INPUT pnld_palete.qtde_encomendas,
                                   INPUT pnld_palete.peso_liquido,
                                   INPUT pnld_palete.vlr_declarado,
                                   INPUT 0).

            ASSIGN pnld_palete.pnld_status = 6
                   pnld_palete.cod-estabel  = p-table.cod-estabel
                   pnld_palete.serie        = p-table.serie
                   pnld_palete.nr-nota-fisc = p-table.nr-nota-fis
                   pnld_palete.nr-seq-fat   = bf_it-nota-fisc.nr-seq-fat.
        END.
    END.

    /* Nota Fiscal cancelada, registrar dados no PNLD */
    IF p-table.dt-cancela <> p-old-table.dt-cancela THEN DO:
        FOR EACH pnld_palete EXCLUSIVE-LOCK
           WHERE pnld_palete.nome-abrev = p-table.nome-ab-cli
             AND pnld_palete.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST pnld_isbn_objeto NO-LOCK
                 WHERE pnld_isbn_objeto.cod_objeto   = pnld_palete.cod_objeto 
                   AND pnld_isbn_objeto.cod_programa = pnld_palete.cod_programa NO-ERROR.

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = pnld_palete.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = pnld_palete.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = pnld_palete.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = pnld_isbn_objeto.it-codigo NO-ERROR.

            IF NOT AVAIL pnld_isbn_objeto THEN NEXT.
            IF NOT AVAIL bf_it-nota-fisc THEN NEXT.

            FOR EACH pnld_encomenda
                WHERE pnld_encomenda.num_palete = pnld_palete.num_palete:
                /* inclui o registro de movimento das Encomendas */
                RUN pi_pnld_movimento (INPUT "ENCOMENDA",
                                       INPUT TODAY,
                                       INPUT "CFA",
                                       INPUT pnld_encomenda.num_palete,
                                       INPUT pnld_encomenda.num_encomenda,
                                       INPUT pnld_encomenda.num_encomenda,
                                       INPUT pnld_encomenda.qtde_objetos,
                                       INPUT 1,
                                       INPUT pnld_encomenda.peso_liquido,
                                       INPUT 0,
                                       INPUT pnld_encomenda.vlr_encomenda).

                ASSIGN pnld_encomenda.pnld_status  = 5
                       pnld_encomenda.cod-estabel  = ""
                       pnld_encomenda.serie        = ""
                       pnld_encomenda.nr-nota-fisc = ""
                       pnld_encomenda.nr-seq-fat   = 0.
            END.

            /* inclui o registro de movimento do palete */
            RUN pi_pnld_movimento (INPUT "PALETE",
                                   INPUT TODAY,
                                   INPUT "CFA",
                                   INPUT pnld_palete.num_palete,
                                   INPUT pnld_palete.encomenda_ini,
                                   INPUT pnld_palete.encomenda_fim,
                                   INPUT pnld_palete.qtde_objetos,
                                   INPUT pnld_palete.qtde_encomendas,
                                   INPUT pnld_palete.peso_liquido,
                                   INPUT pnld_palete.vlr_declarado,
                                   INPUT 0).

            ASSIGN pnld_palete.pnld_status = 5
                   pnld_palete.cod-estabel  = ""
                   pnld_palete.serie        = ""
                   pnld_palete.nr-nota-fisc = ""
                   pnld_palete.nr-seq-fat   = 0.
        END.
    END.

    /********************************************************************/
    /* 03/11/2014 - Controle Websales                                   */
    /* Nova nota fiscal, registrar dados no Websales                    */
    /* 09/02/2015 - gera pedido BackOrder                               */
    /********************************************************************/
    IF NEW(p-table) THEN DO: 
    
        DEFINE TEMP-TABLE tt-web-ped-venda LIKE web-ped-venda.
        DEFINE VARIABLE c-pedido  AS CHAR NO-UNDO.
        DEFINE VARIABLE i-pedido  AS INTEGER NO-UNDO.


        /***** Pedidos de Doacao *****/
        FOR EACH web-ped-doacao EXCLUSIVE-LOCK
           WHERE web-ped-doacao.nome-abrev = p-table.nome-ab-cli
             AND web-ped-doacao.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = web-ped-doacao.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = web-ped-doacao.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = web-ped-doacao.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = web-ped-doacao.it-codigo  
                   AND bf_it-nota-fisc.serie       = p-table.serie 
                   AND bf_it-nota-fisc.nr-nota-fis = p-table.nr-nota-fis NO-ERROR.
            /*** IF NOT AVAIL bf_it-nota-fisc THEN NEXT. ****/

            ASSIGN web-ped-doacao.situacao     = "FATURADO"                   WHEN AVAIL bf_it-nota-fisc /* 18/07/2016 */  
                   web-ped-doacao.serie        = bf_it-nota-fisc.serie        WHEN AVAIL bf_it-nota-fisc   
                   web-ped-doacao.nr-nota-fisc = bf_it-nota-fisc.nr-nota-fis  WHEN AVAIL bf_it-nota-fisc
                   web-ped-doacao.nr-seq-fat   = bf_it-nota-fisc.nr-seq-fat   WHEN AVAIL bf_it-nota-fisc.
            IF NOT AVAIL bf_it-nota-fisc THEN
               ASSIGN web-ped-doacao.situacao = "CANCELADO".

            OUTPUT TO "\\192.168.0.131\edi\TEMP\log-tw-mod-web-ped-doacao.csv" NO-CONVERT APPEND.
            EXPORT DELIMITER ";"
                   NOW
                   c-seg-usuario
                   web-ped-doacao.nome-abrev
                   web-ped-doacao.nr-pedcli
                   web-ped-doacao.it-codigo
                   web-ped-doacao.situacao.
                  OUTPUT CLOSE.

      END.

        /***** Pedidos de Venda *****/
        FOR EACH web-ped-venda EXCLUSIVE-LOCK
           WHERE web-ped-venda.nome-abrev = p-table.nome-ab-cli
             AND web-ped-venda.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = web-ped-venda.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = web-ped-venda.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = web-ped-venda.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = web-ped-venda.it-codigo 
                   AND bf_it-nota-fisc.serie       = p-table.serie 
                   AND bf_it-nota-fisc.nr-nota-fis = p-table.nr-nota-fis NO-ERROR.
            /*** IF NOT AVAIL bf_it-nota-fisc THEN NEXT.***/

            ASSIGN web-ped-venda.situacao     = "FATURADO"                  WHEN AVAIL bf_it-nota-fisc   /* 18/07/2016 */
                   web-ped-venda.serie        = bf_it-nota-fisc.serie       WHEN AVAIL bf_it-nota-fisc
                   web-ped-venda.nr-nota-fisc = bf_it-nota-fisc.nr-nota-fis WHEN AVAIL bf_it-nota-fisc
                   web-ped-venda.nr-seq-fat   = bf_it-nota-fisc.nr-seq-fat  WHEN AVAIL bf_it-nota-fisc.
            IF NOT AVAIL bf_it-nota-fisc THEN
                IF web-ped-venda.situacao <> "FATURADO" THEN /* bhfs.2020-11-18 */
                    ASSIGN web-ped-venda.situacao = "CANCELADO".

        END.

        /* 09/02/2015 Criaá∆o de pedidos Back Order */
        EMPTY TEMP-TABLE tt-web-ped-venda.

        /* Busca ultimo pedido Back Order relacionado */
        ASSIGN i-pedido = 1
               c-pedido = substR(p-table.nr-pedcli,1,8) + "BO" + STRING(i-pedido,"99").
        DO WHILE CAN-FIND(FIRST web-ped-venda WHERE web-ped-venda.nr-pedcli = c-pedido):
            ASSIGN i-pedido = i-pedido + 1
                   c-pedido = substR(p-table.nr-pedcli,1,8) + "BO" + STRING(i-pedido,"99").
        END.

        /*******************   Pesquisa se bo gerou pedido *******************/
        IF i-pedido > 1  THEN ASSIGN i-pedido = (i-pedido - 1) 
                                     c-pedido = substR(p-table.nr-pedcli,1,8) + "BO" + STRING(i-pedido,"99").

        FIND FIRST ped-venda
           WHERE ped-venda.nr-pedcli = c-pedido 
            AND ped-venda.nome-abrev = p-table.nome-ab-cli NO-ERROR.

        IF AVAIL ped-venda  THEN
           ASSIGN i-pedido = (i-pedido + 1)
                  c-pedido = substR(p-table.nr-pedcli,1,8) + "BO" + STRING(i-pedido,"99").

        FOR EACH  web-ped-venda NO-LOCK
            WHERE web-ped-venda.nome-abrev = p-table.nome-ab-cli
              AND web-ped-venda.nr-pedcli  = p-table.nr-pedcli
            break BY web-ped-venda.nr-pedcli:
            
            IF web-ped-venda.situacao = "FATURADO"
                AND web-ped-venda.quantidade = web-ped-venda.qtde-saldo-fisico THEN NEXT.
            /* 09/12/2016 - Foi ajustado a quantidade para zero */
            IF web-ped-venda.quantidade = 0 THEN NEXT.

                         
            IF AVAIL p-table AND (p-table.nat-operacao = "5102CP" OR
                                  p-table.nat-operacao = "6102CP") THEN NEXT.
            
            // PIVOTAL
            //IF tt-web-ped-venda.flg_Restricao THEN NEXT.

            CREATE tt-web-ped-venda.
            BUFFER-COPY web-ped-venda TO tt-web-ped-venda.
            ASSIGN 
                tt-web-ped-venda.nr-pedcli            = c-pedido
                tt-web-ped-venda.nr-sequencia         = 0
                tt-web-ped-venda.nr-nota-fisc         = ""
                tt-web-ped-venda.serie                = "" 
                tt-web-ped-venda.nr-seq-fat           = 0 
                tt-web-ped-venda.situacao             = "BACK ORDER" 
                tt-web-ped-venda.dt-emis-ped-venda    = web-ped-venda.dt-gera-pedido  /*? *//* BHFS - ALTERADO DE ? PARA web-ped-venda.dt-gera-pedido. Projeto Pivotal */
                tt-web-ped-venda.dt-emis-nota-fiscal  = ?
                tt-web-ped-venda.log-saldo-fisico     = FALSE 
                tt-web-ped-venda.dat-saldo-fisico     = ?
                tt-web-ped-venda.aprov-saldo-fis      = "" 
                tt-web-ped-venda.quantidade           = IF web-ped-venda.situacao = "CANCELADO" THEN web-ped-venda.quantidade ELSE (web-ped-venda.quantidade - web-ped-venda.qtde-saldo-fisico)
                tt-web-ped-venda.qtde-saldo-fisico    = 0
                tt-web-ped-venda.qtde-confirmada-fat  = 0
                tt-web-ped-venda.dt-gera-pedido       = web-ped-venda.dt-gera-pedido /*TODAY */
                tt-web-ped-venda.hr-pedido-web        = web-ped-venda.hr-pedido-web /*STRING(TIME,"HH:MM:SS")*/
                tt-web-ped-venda.nr-conhecimento      = ""
                tt-web-ped-venda.dt-embarque          = ?
                tt-web-ped-venda.qtde-volume          = 0
                tt-web-ped-venda.peso-liquido         = 0
                tt-web-ped-venda.peso-bruto           = 0.
        END.
        IF CAN-FIND(FIRST tt-web-ped-venda) THEN DO:
            FOR EACH tt-web-ped-venda:
                CREATE web-ped-venda.
                BUFFER-COPY tt-web-ped-venda TO web-ped-venda.
            END.

            /*DRM - 22/01/2019 - Corrige quantidade 0 das Back Order's*/

            FOR EACH tt-web-ped-venda WHERE 
                     tt-web-ped-venda.quantidade = 0:

                FIND web-ped-venda OF tt-web-ped-venda NO-LOCK NO-ERROR.
                IF AVAIL web-ped-venda THEN
                DO:
                    FIND FIRST b-web-ped-venda WHERE 
                               b-web-ped-venda.nome-abrev = web-ped-venda.nome-abrev               AND 
                               b-web-ped-venda.nr-pedcli  = SUBSTRING(web-ped-venda.nr-pedcli,1,8) AND 
                               b-web-ped-venda.it-codigo  = web-ped-venda.it-codigo                NO-LOCK NO-ERROR.
                    IF AVAIL b-web-ped-venda THEN
                    DO:
                        FIND CURRENT web-ped-venda EXCLUSIVE-LOCK NO-ERROR.
                        ASSIGN web-ped-venda.quantidade = b-web-ped-venda.quantidade.
                        FIND CURRENT web-ped-venda NO-LOCK NO-ERROR.
                    END.
                END.
            END.
        END.
        
    END.

    /* Nota Fiscal cancelada, registrar dados no WebSales */
    IF p-table.dt-cancela <> p-old-table.dt-cancela THEN DO:

/*         MESSAGE "Rotina de Cancelamento em tw-nota-fiscal" VIEW-AS ALERT-BOX. */

        /***** Pedidos de Venda *****/
        FOR EACH web-ped-venda EXCLUSIVE-LOCK
           WHERE web-ped-venda.nome-abrev = p-table.nome-ab-cli
             AND web-ped-venda.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = web-ped-venda.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = web-ped-venda.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = web-ped-venda.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = web-ped-venda.it-codigo NO-ERROR.
            IF NOT AVAIL bf_it-nota-fisc THEN NEXT.

            ASSIGN web-ped-venda.situacao     = "PEND FAT"
                   web-ped-venda.serie        = ""
                   web-ped-venda.nr-nota-fisc = ""
                   web-ped-venda.nr-seq-fat   = 0.
        END.

        /***** Pedidos de Doaá∆o *****/ 
        FOR EACH web-ped-doacao EXCLUSIVE-LOCK
           WHERE web-ped-doacao.nome-abrev = p-table.nome-ab-cli
             AND web-ped-doacao.nr-pedcli  = p-table.nr-pedcli:

            FIND FIRST bf_it-nota-fisc NO-LOCK USE-INDEX ch-pedcli
                 WHERE bf_it-nota-fisc.nome-ab-cli = web-ped-doacao.nome-abrev
                   AND bf_it-nota-fisc.nr-pedcli   = web-ped-doacao.nr-pedcli
                   AND bf_it-nota-fisc.nr-seq-ped  = web-ped-doacao.nr-sequencia
                   AND bf_it-nota-fisc.it-codigo   = web-ped-doacao.it-codigo NO-ERROR.
            IF NOT AVAIL bf_it-nota-fisc THEN NEXT.

            ASSIGN web-ped-doacao.situacao     = "PEND FAT"
                   web-ped-doacao.serie        = ""
                   web-ped-doacao.nr-nota-fisc = ""
                   web-ped-doacao.nr-seq-fat   = 0.

/*            MESSAGE "Data"   NOW SKIP(1)                         */
/*                    "User " c-seg-usuario   SKIP(1)              */
/*                    "Cliente " web-ped-doacao.nome-abrev SKIP(1) */
/*                    "Pedido" web-ped-doacao.nr-pedcli SKIP(1)    */
/*                     "Item " web-ped-doacao.it-codigo SKIP(1)    */
/*                     "Situaá∆o " web-ped-doacao.situacao         */
/*                VIEW-AS ALERT-BOX.                               */
/*                                                                 */

        END.
    END.

    /********************************************************************/
    /* 01/02/2017 - Controle de Doacoes                                 */
    /* Quando cliente Suspenso, retornar na tabela emitente             */
    /********************************************************************/
    IF NEW(p-table) THEN DO: 
        IF p-table.nr-pedcli BEGINS "WD" THEN DO:
            DEFINE BUFFER bf-ped-venda FOR ped-venda.
            DEFINE BUFFER bf-emitente  FOR emitente.
            FIND FIRST bf-ped-venda 
                WHERE bf-ped-venda.nome-abrev = p-table.nome-ab-cli
                  AND bf-ped-venda.nr-pedcli  = p-table.nr-pedcli  NO-LOCK NO-ERROR.
            IF AVAIL bf-ped-venda AND bf-ped-venda.log-1 THEN DO:
                FIND FIRST bf-emitente EXCLUSIVE-LOCK
                     WHERE bf-emitente.cod-emitente = bf-ped-venda.cod-emitente NO-ERROR.
                IF AVAIL bf-emitente THEN ASSIGN bf-emitente.ind-cre-cli = 4. /* suspenso */
                RELEASE bf-emitente.
            END.
        END.
    END.
    /********************************************************************/

END.
/******* ESPECIFICO DOAÄ«O TOTVS DRG-SP ********/
RUN twp\tw-nota-fiscal-u01.p(BUFFER p-table,
                             BUFFER p-old-table).


                             
/******* ESPECIFICO SHOPIFY 4MAKE ********/
RUN twp\tw-nota-fiscal-u02.p(BUFFER p-table,
                             BUFFER p-old-table).
                             
                             
RETURN "OK"

