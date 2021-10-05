/*
 *-----------------------------------------------------------------------------`
 *  PROGRAMA        esspf125rp
 *  OBJETIVO        Atualiza‡Æo tabela de pre‡o
 *  AUTOR           TOTVS - LASF
 *  DATA            06/2021
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINI€åES
 *
 *------------------------------------------------------------------------------
 */
{include/i-prgvrs.i esspf125RP 20.09.28.001 } /*** 010016 ***/

/* Rotina para tratamento de licen‡as TOTVS */
{utp/ut-glob.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esspf125RP MPD}
&ENDIF
&global-define programa nome-do-programa


{include/i_dbinst.i}  /* versao das bases e bases instaladas */
{lib/rowErrors.i}
{lib/utilidades.i}
{lib/log2.i}
{lib/mensagens2.i}

DEFINE TEMP-TABLE tt-ped-venda  NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.


DEFINE TEMP-TABLE tt-ped-item   NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD cliente-ini      AS INT
    FIELD cliente-fim      AS INT
    FIELD nome-abrev-ini   AS CHAR
    FIELD nome-abrev-fim   AS CHAR
    FIELD nr-pedcli-ini    AS CHAR
    FIELD nr-pedcli-fim    AS CHAR
    FIELD data-emis-ini    AS DATE
    FIELD data-emis-fim    AS DATE
    FIELD data-impl-ini    AS DATE
    FIELD data-impl-fim    AS DATE.

DEF TEMP-TABLE tt-alteracao NO-UNDO
    FIELD nome-abrev LIKE ped-venda.nome-abrev    
    FIELD nr-pedcli  LIKE ped-venda.nr-pedcli     
    FIELD it-codigo  LIKE ped-item.it-codigo
    FIELD nr-tabpre1 LIKE emitente.nr-tabpre      
    FIELD preco1     LIKE ped-item.vl-preuni   
    FIELD nr-tabpre2 LIKE tb-preco.nr-tabpre      
    FIELD preco2     LIKE ped-item.vl-preuni   
    FIELD vl-preuni  LIKE ped-item.vl-preuni
        INDEX ch_item
              nome-abrev 
              nr-pedcli 
              it-codigo.


DEF TEMP-TABLE tt-erro NO-UNDO 
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

DEF BUFFER bf-ped-venda FOR ped-venda.

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter Table for tt-raw-digita.

DEF VAR h-acomp      AS HANDLE.
DEF VAR p-nr-tabpre  AS CHAR   NO-UNDO.
DEF VAR l-atualiza   AS LOG    NO-UNDO.

create tt-param.
Raw-transfer raw-param to tt-param.

FIND FIRST tt-param NO-ERROR.

FUNCTION obterPrecoItem2 RETURNS DECIMAL
    (INPUT pTabpre      AS CHAR,
     INPUT pItem        AS CHAR,
     INPUT pRefer       AS CHAR,
     INPUT pUn          AS CHAR,
     INPUT pQuantidade  AS DECIMAL
     ):


    FOR FIRST   preco-item  NO-LOCK WHERE
                preco-item.nr-tabpre        =  pTabpre    
            AND preco-item.it-codigo        =  pItem      
            AND preco-item.cod-refer        =  pRefer     
            AND preco-item.cod-unid-med     =  pUn        
            AND preco-item.quant-min        <= pQuantidade
                USE-INDEX ch-itemtab:

/*         MESSAGE                                */
/*             preco-item.desco-quant             */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        RETURN preco-item.preco-venda.
    END.

    RETURN ?.

END FUNCTION.

/* include padrÆo para vari veis para o log  */
{include/i-rpvar.i}

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.
/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

find MGCAD.empresa
    where empresa.ep-codigo = v_cdn_empres_usuar
    no-lock no-error.
find first param-global no-lock no-error.

{utp/ut-liter.i DISTRIBUI€ÇO * }
assign c-sistema = return-value.
{utp/ut-liter.i ATUALIZA€ÇO_TABELA_DE_PRE€O_PEDIDOS * }
assign c-titulo-relat = RETURN-VALUE.

assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "20.09.28":U
       c-revisao     = "001"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


IF i-num-ped-exec-rpw <> 0 THEN DO:
    DEF VAR c-num-ped-exec AS CHAR NO-UNDO.
    ASSIGN c-num-ped-exec = "_" + STRING(i-num-ped-exec-rpw) + "_" + TRIM(c-seg-usuario).
    ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,".lst",c-num-ped-exec + ".lst").
    ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,".txt",c-num-ped-exec + ".txt").
    ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,".csv",c-num-ped-exec + ".csv").
    ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,".tmp",c-num-ped-exec + ".tmp").
END.

Do On Stop Undo, Leave:

    {include/i-rpcab.i}
    {include/i-rpout.i}

    VIEW FRAME f-cabec.
    View FRAME f-rodape.

    RUN utp/ut-acomp.p persistent set h-acomp.
    
    {utp/ut-liter.i INTEGRA€ÇO *}
    
    run pi-inicializar in h-acomp (input RETURN-VALUE).

    RUN pi-processa-pedidos.

    {include/i-rpclo.i}
    
    run pi-finalizar in h-acomp.

End.

PROCEDURE pi-processa-pedidos:

    EMPTY TEMP-TABLE tt-alteracao.

    FOR EACH ped-venda NO-LOCK 
        WHERE ped-venda.cod-emitente >= tt-param.cliente-ini
          AND ped-venda.cod-emitente <= tt-param.cliente-fim
          AND ped-venda.nome-abrev   >= tt-param.nome-abrev-ini
          AND ped-venda.nome-abrev   <= tt-param.nome-abrev-fim
          AND ped-venda.nr-pedcli    >= tt-param.nr-pedcli-ini
          AND ped-venda.nr-pedcli    <= tt-param.nr-pedcli-fim
          AND ped-venda.dt-emissao   >= tt-param.data-emis-ini
          AND ped-venda.dt-emissao   <= tt-param.data-emis-fim
          AND ped-venda.dt-implant   >= tt-param.data-impl-ini
          AND ped-venda.dt-implant   <= tt-param.data-impl-fim
          AND ped-venda.cod-sit-ped  < 3
          AND NOT CAN-FIND(FIRST web-ped-doacao
                           WHERE web-ped-doacao.cod-estabel = ped-venda.cod-estabel
                             AND web-ped-doacao.nome-abrev  = ped-venda.nome-abrev 
                             AND web-ped-doacao.nr-pedcli   = ped-venda.nr-pedcli):

        FOR FIRST emitente FIELDS(cod-emitente nome-abrev nr-tabpre) NO-LOCK 
            WHERE emitente.nome-abrev  = ped-venda.nome-abrev:
        END.

        ASSIGN p-nr-tabpre = ""
               l-atualiza  = NO.

        RUN pi-acompanhar IN h-acomp (input "Pedido: " + ped-venda.nr-pedcli + "/" + STRING(ped-venda.dt-implant)).

        FOR EACH ped-item OF ped-venda NO-LOCK WHERE
                 ped-item.cod-sit-item = 1,
            FIRST ITEM NO-LOCK WHERE
                  ITEM.it-codigo    = ped-item.it-codigo              
                  :

            FOR FIRST web-ped-venda NO-LOCK WHERE
                      web-ped-venda.nome-abrev  = ped-venda.nome-abrev
                AND   web-ped-venda.nr-pedcli   = ped-venda.nr-pedcli
                AND   web-ped-venda.it-codigo   = ped-item.it-codigo
                :
            END.

            /* Pegando a tabela de pre‡os correta */
            /* BHFS.04/11/2020 - INI */
            FIND FIRST tb-preco NO-LOCK
                 WHERE tb-preco.nr-tabpre= emitente.nr-tabpre
                   AND tb-preco.situacao = 1 /* Ativo */
                   AND tb-preco.dt-inival <= web-ped-venda.dt-gera-pedido
                   AND tb-preco.dt-fimval >= web-ped-venda.dt-gera-pedido NO-ERROR.

            IF NOT AVAIL tb-preco THEN DO:
                FOR FIRST tb-preco NO-LOCK
                    WHERE tb-preco.dt-inival <= web-ped-venda.dt-gera-pedido
                      AND tb-preco.dt-fimval >= web-ped-venda.dt-gera-pedido
                      AND tb-preco.situacao = 1, /* Ativo */
                    FIRST ext-tb-preco OF tb-preco
                    WHERE ext-tb-preco.lg-tab-historico = YES NO-LOCK:
                END.
            END.
            IF NOT AVAIL ext-tb-preco THEN NEXT.

            RUN ajustarItemPedido (INPUT tb-preco.nr-tabpre).

            FOR EACH rowerrors:
                DISPLAY 
                    rowerrors.errornumber
                    rowerrors.errordesc.

            END.

            ASSIGN p-nr-tabpre = tb-preco.nr-tabpre
                   l-atualiza  = YES.

            CREATE tt-alteracao.
            ASSIGN tt-alteracao.nome-abrev = ped-venda.nome-abrev    
                   tt-alteracao.nr-pedcli  = ped-venda.nr-pedcli     
                   tt-alteracao.it-codigo  = ped-item.it-codigo
                   tt-alteracao.nr-tabpre1 = emitente.nr-tabpre      
                   tt-alteracao.preco1    = obterPrecoItem2   
                                             (INPUT emitente.nr-tabpre           ,
                                              INPUT ped-item.it-codigo,
                                              INPUT ""                           ,
                                              INPUT ITEM.un                      ,
                                              INPUT ped-item.qt-pedida
                                              )
                   tt-alteracao.nr-tabpre2 = tb-preco.nr-tabpre      
                   tt-alteracao.preco2     = obterPrecoItem2   
                                             (INPUT tb-preco.nr-tabpre  ,
                                              INPUT ped-item.it-codigo,
                                              INPUT ""                           ,
                                              INPUT ITEM.un                      ,
                                              INPUT ped-item.qt-pedida
                                              )
                   tt-alteracao.vl-preuni  =  ped-item.vl-preuni.
        END.
        IF l-atualiza THEN 
           RUN ajustarPedido(INPUT p-nr-tabpre).

        FOR EACH rowerrors:
            DISPLAY 
                rowerrors.errornumber
                rowerrors.errordesc.

        END.

    END.

    FOR EACH tt-alteracao.

        DISP tt-alteracao.nome-abrev  
             tt-alteracao.nr-pedcli   
             tt-alteracao.it-codigo  
             tt-alteracao.nr-tabpre1  COLUMN-LABEL "Tabela Cliente" 
             tt-alteracao.preco1      COLUMN-LABEL "Pre‡o Tabela Cliente"
             tt-alteracao.nr-tabpre2  COLUMN-LABEL "Tabela Atualizada"
             tt-alteracao.preco2      COLUMN-LABEL "Pre‡o Tabela Atualizada"
             tt-alteracao.vl-preuni 
             WITH DOWN FRAME f-atu WIDTH 300 STREAM-IO.
    END.

END PROCEDURE.

PROCEDURE ajustarPedido:                                                                                                                 
DEFINE INPUT  PARAMETER pTabela AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h_bodi159 AS HANDLE      NO-UNDO.

DEF BUFFER b-ped-venda FOR ped-venda.

    ALTERAR_PED_VENDA:
    DO TRANSACTION:

        RUN dibo/bodi159.p PERSISTENT SET h_bodi159.
        
        EMPTY TEMP-TABLE tt-ped-venda.

        /* Cria tabela temporaria do pedido de venda */
        CREATE tt-ped-venda.
        BUFFER-COPY ped-venda TO tt-ped-venda
            ASSIGN tt-ped-venda.r-rowid = ROWID(ped-venda).


        ASSIGN tt-ped-venda.nr-tabpre        = pTabela
                   .
    
        RUN emptyRowErrors  IN h_bodi159.
        RUN openQueryStatic IN h_bodi159 (INPUT "Main":U).
        RUN gotoKey         IN h_bodi159 (tt-ped-venda.nome-abrev  ,
                                          tt-ped-venda.nr-pedcli).
        RUN setRecord               IN h_bodi159 (INPUT TABLE tt-ped-venda).
        RUN validateRecord       IN h_bodi159 (INPUT "UPDATE").
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN getRowErrors            IN h_bodi159 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            RUN destroyBO IN h_bodi159.
            DELETE PROCEDURE h_bodi159.
            ASSIGN h_bodi159 = ?.
            UNDO ALTERAR_PED_VENDA, RETURN "NOK".       
        END.

        RUN updateRecord        IN h_bodi159 .
        IF RETURN-VALUE <> "OK" THEN DO:
            RUN getRowErrors            IN h_bodi159 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            RUN destroyBO IN h_bodi159.
            DELETE PROCEDURE h_bodi159.
            ASSIGN h_bodi159 = ?.
            UNDO ALTERAR_PED_VENDA, RETURN "NOK".
        END.
        FIND FIRST tt-ped-venda NO-ERROR.
        IF AVAIL tt-ped-venda THEN DO:

           FIND FIRST b-ped-venda NO-LOCK 
                WHERE b-ped-venda.nome-abrev  = tt-ped-venda.nome-abrev 
                  AND b-ped-venda.nr-pedcli   = tt-ped-venda.nr-pedcli  NO-ERROR.
           IF AVAIL b-ped-venda THEN DO:

              IF b-ped-venda.completo = NO THEN 
                 RUN pi-completa-pedido(INPUT ROWID(ped-venda)).
           END.

        END.
    END.

    RUN destroyBO IN h_bodi159.
    DELETE PROCEDURE h_bodi159.
    ASSIGN h_bodi159 = ?.


    RETURN "OK".

END PROCEDURE.

PROCEDURE pi-completa-pedido:
    DEF INPUT PARAMETER p-rowid AS ROWID NO-UNDO.

    DEF VAR bodi159-com AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(bodi159-com) THEN DO:
       RUN dibo/bodi159com.p PERSISTENT SET bodi159-com.
    END.

    RUN completeOrder in bodi159-com(INPUT p-rowid,
                                     OUTPUT TABLE Rowerrors).


    IF VALID-HANDLE(bodi159-com) THEN 
       DO:
          RUN destroyBO       in bodi159-com.
          DELETE PROCEDURE bodi159-com.
       END.

END PROCEDURE.

PROCEDURE ajustarItemPedido:                                                                                                                 
DEFINE INPUT  PARAMETER pTabela AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h_bodi154    AS HANDLE      NO-UNDO.
DEFINE VARIABLE h_bodi154cal AS HANDLE      NO-UNDO.

DEFINE VARIABLE de-novo-preco LIKE ped-item.vl-pretab NO-UNDO.


    ALTERAR_PED_ITEM:
    DO TRANSACTION:

        RUN dibo/bodi154.p    PERSISTENT SET h_bodi154.
        RUN dibo/bodi154cal.p PERSISTENT SET h_bodi154cal.
        
        EMPTY TEMP-TABLE tt-ped-item.

        /* Cria tabela temporaria do pedido de venda */
        CREATE tt-ped-item.
        BUFFER-COPY ped-item TO tt-ped-item
            ASSIGN tt-ped-item.r-rowid = ROWID(ped-item).
/*
        ASSIGN de-novo-preco = obterPrecoItem2   
             (INPUT tb-preco.nr-tabpre  ,
              INPUT ped-item.it-codigo,
              INPUT ""                           ,
              INPUT ITEM.un                      ,
              INPUT ped-item.qt-pedida
              ).
*/
        ASSIGN tt-ped-item.nr-tabpre        = pTabela.
    
        RUN emptyRowErrors  IN h_bodi154.
        RUN openQueryStatic IN h_bodi154 (INPUT "Main":U).
        RUN gotoKey         IN h_bodi154 (tt-ped-item.nome-abrev  ,
                                          tt-ped-item.nr-pedcli,
                                          tt-ped-item.nr-sequencia,
                                          tt-ped-item.it-codigo ,
                                          ""
                                          ).
        RUN setRecord            IN h_bodi154 (INPUT TABLE tt-ped-item).
        RUN validateRecord       IN h_bodi154 (INPUT "UPDATE").
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN getRowErrors            IN h_bodi154 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            RUN destroyBO IN h_bodi154.
            DELETE PROCEDURE h_bodi154.
            ASSIGN h_bodi154 = ?.
            UNDO ALTERAR_PED_ITEM, RETURN "NOK".       
        END.

        RUN updateRecord        IN h_bodi154 .
        IF RETURN-VALUE <> "OK" THEN DO:
            RUN getRowErrors            IN h_bodi154 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            RUN destroyBO IN h_bodi154.
            DELETE PROCEDURE h_bodi154.
            ASSIGN h_bodi154 = ?.
            UNDO ALTERAR_PED_ITEM, RETURN "NOK".
        END.
    END.

    RUN destroyBO IN h_bodi154.
    DELETE PROCEDURE h_bodi154.
    ASSIGN h_bodi154 = ?.

    IF VALID-HANDLE(h_bodi154cal) THEN DO:
        DELETE PROCEDURE h_bodi154cal.
        ASSIGN h_bodi154cal = ?.
    END.

    RETURN "OK".

END PROCEDURE.

