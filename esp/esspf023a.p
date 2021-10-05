/*----------------------------------------------------------------------------------------------/
 Programa..: esspf0023a.p
 Objetivo..: Interface Integracao Titulos SHOPIFY - ACR
 Data......: 09/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/******************************* Definitions **************************************************/
/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
USING PROGRESS.Json.ObjectModel.*.

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{esp/esspf023.i}
{method/dbotterr.i}
{include/boini.i} /** tt-bo-erro **/
{utp/utapi019.i}

{lib/utilidades.i}
{lib/log2.i} 
/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson       AS LONGCHAR        NO-UNDO.
DEFINE VARIABLE lRetJson        AS LOGICAL         NO-UNDO.
DEFINE VARIABLE iCountMain      AS INTEGER         NO-UNDO.
DEFINE VARIABLE iCountSec       AS INTEGER         NO-UNDO.
DEFINE VARIABLE iErro           AS INTEGER         NO-UNDO.

def var de-total-movto     as decimal format ">>>,>>>,>>>,>>9" initial 0 no-undo.
def var i-cont-seq         as integer initial 0 no-undo.
def var i-cod-emitente     like emitente.cod-emitente no-undo.
def var i-tip-parametro    as integer   no-undo.
def var i-cod-erro         as integer format ">>>>,>>9" no-undo.
def var c-val-parametro    as character no-undo.
def var c-msg-erro         as character no-undo.
DEF VAR c_msg_ajuda        as character no-undo.
DEF VAR c_cod_plano_contabil as character no-undo.
def var c-cta_ctbl_fatur   as character format "x(20)" no-undo.
def var c_cod_plano_ccusto as character no-undo.
def var c_cod_ccusto       as character no-undo.
def var c-cod-unid-negoc   as character format "x(3)"  no-undo.
def var c-tp-fluxo-financ  as character format "x(12)" no-undo.
def var c-sequencia-refer  as character format "x(11)" no-undo.
def var c-sequencia-titulo as character format "x(11)" no-undo.
DEF VAR c-estab            as character format "x(5)"  no-undo.
def var c-cod-refer        as character format "x(10)" no-undo.
def var c-cod-titulo       as character format "x(10)" no-undo.
def var c-cod-empresa      like estabelecimento.cod_empresa no-undo.
def var r-recid-lote       as recid no-undo.
def var h-acomp            as handle no-undo.
def var cEstab             as char.
DEF VAR c_cod_espec_docto  AS CHAR.
DEF VAR c_cod_ser_docto    AS CHAR.
DEF VAR c_cod_portador     AS CHAR.
DEF VAR c_cod_cart_bcia    AS CHAR.
DEF VAR c-cpf-cnpj AS CHAR.
DEF VAR v-dtPed AS DATE.
DEF VAR v-dtPag AS DATE.
DEF VAR v-dia AS INT.
DEF VAR v-mes AS INT.
DEF VAR v-ano AS INT.
DEF VAR c-retorno AS CHAR.
DEF VAR c-caminho AS CHAR.
DEF VAR l-erro AS LOG.


/**** Definicao de vari?aveis utilizadas na chamada da API ************/
def var v_hdl_programa            as handle format ">>>>>>9" no-undo.
def var p_cod_matriz_trad_org_ext as char initial "EMS2" no-undo.
def var p_log_atualiza_refer_acr  as log  no-undo.
def var p_log_assume_dat_emis     as log  no-undo.

DEFINE VARIABLE h-boad098       AS HANDLE          NO-UNDO. /* Busca cliente */
/**** Definicao de temp-table ****************************************/
{prgfin/acr/acr900zi.i}

/**** Definicao de buffer ****************************************/
def buffer b_tit_acr for tit_acr.
DEF BUFFER b-es-api-param-acr-spf FOR es-api-param-acr-spf.
/* --------------------------------------------------------------------------------------------
    Functions
----------------------------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM TABLE FOR ttPayments.
DEFINE OUTPUT PARAM TABLE FOR RowErrors.


/******************************* Main Block **************************************************/

/* run utp/ut-acomp.p persistent set h-acomp.                                  */
/* run pi-inicializar in h-acomp (input "Iniciando Integracao Financeiro..."). */

EMPTY TEMP-TABLE RowErrors.
EMPTY TEMP-TABLE tt_integr_acr_repres_comis_2.
EMPTY TEMP-TABLE tt_integr_acr_item_lote_impl_8.
EMPTY TEMP-TABLE tt_integr_acr_aprop_relacto_2.

IF LOG-MANAGER:LOGFILE-NAME = "" OR LOG-MANAGER:LOGFILE-NAME     = ? THEN
    RUN ativarClientLOg.

//MESSAGE "INICIANDO INTEGRACAO FINANCEIRA".
LOG-MANAGER:WRITE-MESSAGE("23A - INICIANDO INTEGRACAO FINANCEIRA" + CHR(10) + CHR(10)).

FIND FIRST ttPayments NO-LOCK NO-ERROR.
find first es-api-param-acr-spf no-lock no-error.

FIND FIRST param_estab_acr NO-LOCK
     WHERE param_estab_acr.cod_estab       = cEstab
       AND param_estab_acr.dat_inic_valid <= TODAY
       AND param_estab_acr.dat_fim_valid  >= TODAY NO-ERROR.

ASSIGN l-erro = NO
       iErro  = 0.

/* Procurando pelo cliente*/
assign i-cod-emitente = 0.

IF NOT VALID-HANDLE(h-boad098) THEN
    RUN adbo/boad098.p PERSISTENT SET h-boad098.

RUN findCGC IN h-boad098 (INPUT ttPayments.cpfCnpj, OUTPUT c-return).
ASSIGN c-retorno = RETURN-VALUE.

IF c-retorno <> "OK" THEN DO:

    ASSIGN c-cpf-cnpj = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPayments.cpfCnpj,"/",""),"\",""),".",""),",",""),"-","").
    
    RUN findCGC IN h-boad098 (INPUT c-cpf-cnpj, OUTPUT c-return).
    ASSIGN c-retorno = RETURN-VALUE.

    IF c-retorno <> "OK" THEN DO:

        find first emitente use-index cgc where emitente.cgc = c-cpf-cnpj no-lock no-error.
        
        IF NOT AVAIL emitente THEN
            find first emitente use-index cgc where REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(emitente.cgc,"/",""),"\",""),".",""),",",""),"-","") = c-cpf-cnpj no-lock no-error.
    
        if avail emitente then
            assign i-cod-emitente = emitente.cod-emitente.
        ELSE DO:
            RUN piErro(INPUT 17006,
                       INPUT "ERRO: Cliente inexistente com CGC " + ttPayments.cpfCnpj,
                       INPUT "ERRO: Cliente n∆o encontrado.").
            RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
            /*RETURN "NOK":U.*/
            ASSIGN l-erro = YES.
        END.
    END.
    ELSE DO:
        find first emitente use-index cgc where emitente.cgc = c-cpf-cnpj no-lock no-error.
        if avail emitente then
            assign i-cod-emitente = emitente.cod-emitente.
    END.
END.
ELSE DO:
    find first emitente use-index cgc where emitente.cgc = ttPayments.cpfCnpj no-lock no-error.
    if avail emitente then
        assign i-cod-emitente = emitente.cod-emitente.
    ELSE DO:
        ASSIGN c-cpf-cnpj = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ttPayments.cpfCnpj,"/",""),"\",""),".",""),",",""),"-","").
    
        find first emitente use-index cgc where emitente.cgc = c-cpf-cnpj no-lock no-error.
        
        IF NOT AVAIL emitente THEN
            find first emitente use-index cgc where REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(emitente.cgc,"/",""),"\",""),".",""),",",""),"-","") = c-cpf-cnpj no-lock no-error.
    
        if avail emitente then
            assign i-cod-emitente = emitente.cod-emitente.
        ELSE DO:
            RUN piErro(INPUT 17006,
                       INPUT "ERRO: Cliente com CNPJ encontrado mas com erro na busca: CGC " + c-cpf-cnpj,
                       INPUT "ERRO: Cliente com erro de busca.").
            RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
            /*RETURN "NOK":U.*/
            ASSIGN l-erro = YES.
        END.
    END.
END.

IF VALID-HANDLE(h-boad098) THEN
    DELETE PROCEDURE h-boad098.

RUN pi-valida-entrada.
IF RETURN-VALUE = "NOK" THEN DO:
/*     run pi-finalizar in h-acomp. */
    RETURN "NOK".
END.

RUN pi-carrega-params.
IF RETURN-VALUE = "NOK" THEN DO:
/*     run pi-finalizar in h-acomp. */
    RETURN "NOK".
END.

RUN pi-carrega-tts.
IF RETURN-VALUE = "NOK" THEN DO:
/*     run pi-finalizar in h-acomp. */
    RETURN "NOK".
END.

RUN pi-chama-api.
IF RETURN-VALUE = "NOK" THEN DO:
/*     run pi-finalizar in h-acomp. */
    RETURN "NOK".
END.

RUN pi-erros-integr.
IF RETURN-VALUE = "NOK" THEN DO:

/*     run pi-finalizar in h-acomp. */
    RETURN "NOK".
END.

LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - FIM" +  CHR(10)).

/* run pi-finalizar in h-acomp. */
RETURN "OK".

PROCEDURE pi-valida-entrada.
    if NOT avail es-api-param-acr-spf THEN DO:
        RUN piErro(INPUT 17006,
                   INPUT "ERRO: ParÉmetros de estabelecimento n∆o encontrado.",
                   INPUT "ERRO: ParÉmetros de estabelecimento n∆o encontrado.").
        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
        ASSIGN l-erro = YES.
    END.

    IF ttPayments.nrPedido = "" OR ttPayments.nrPedido = ? THEN DO:
        RUN piErro(INPUT 17006,
                   INPUT "ERRO: N£mero do pedido deve estar preenchido.",
                   INPUT "ERRO: N£mero do pedido deve estar preenchido.").
        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
        ASSIGN l-erro = YES.
    END.

    IF DEC(ttPayments.vlPedido) <= 0 THEN DO:
        RUN piErro(INPUT 17006,
                   INPUT "ERRO: Valor do pedido deve ser maior que zero. Valor recebido: " + STRING(ttPayments.vlPedido),
                   INPUT "ERRO: Valor do pedido deve ser maior que zero.").
        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
        ASSIGN l-erro = YES.
    END.
    
/*     MESSAGE "A" SKIP                                    */
/*             "dtPedido   : " ttPayments.dtPedido    SKIP */
/*             "dtPagamento: " ttPayments.dtPagamento      */
/*         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.       */


    ASSIGN ttPayments.dtPedido    = REPLACE(REPLACE(REPLACE(REPLACE(ttPayments.dtPedido,"/",""),"\",""),"-",""),".","")
           ttPayments.dtPagamento = REPLACE(REPLACE(REPLACE(REPLACE(ttPayments.dtPagamento,"/",""),"\",""),"-",""),".","").

    IF LENGTH(ttPayments.dtPedido) = 8 THEN
        ASSIGN v-dia = int(SUBSTRING(ttPayments.dtPedido,7,2))
               v-mes = int(SUBSTRING(ttPayments.dtPedido,5,2))
               v-ano = int(SUBSTRING(ttPayments.dtPedido,1,4))
               v-dtPed = DATE(v-mes,v-dia,v-ano).
    ELSE                 
        ASSIGN v-dia = int(SUBSTRING(ttPayments.dtPedido,3,2))
               v-mes = int(SUBSTRING(ttPayments.dtPedido,5,2))
               v-ano = int("20" + SUBSTRING(ttPayments.dtPedido,1,2))
               v-dtPed = DATE(v-mes,v-dia,v-ano).

    IF LENGTH(ttPayments.dtPagamento) = 8 THEN
        ASSIGN v-dia = int(SUBSTRING(ttPayments.dtPagamento,7,2))
               v-mes = int(SUBSTRING(ttPayments.dtPagamento,5,2))
               v-ano = int(SUBSTRING(ttPayments.dtPagamento,1,4))
               v-dtPag = DATE(v-mes,v-dia,v-ano).
    ELSE
        ASSIGN v-dia = int(SUBSTRING(ttPayments.dtPagamento,3,2))
               v-mes = int(SUBSTRING(ttPayments.dtPagamento,5,2))
               v-ano = int("20" + SUBSTRING(ttPayments.dtPagamento,1,2))
               v-dtPag = DATE(v-mes,v-dia,v-ano).

    IF v-dtPed > v-dtPag THEN DO:
        RUN piErro(INPUT 17006,
                   INPUT "ERRO: Data do pagamento n∆o pode ser anterior Ö data do pedido. Datas recebidas: - DtPedido: " + STRING(v-dtPed,"99/99/9999") + " - DtPagto: " + STRING(v-dtPag,"99/99/9999"),
                   INPUT "ERRO: Data do pagamento n∆o pode ser anterior Ö data do pedido.").
        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
        ASSIGN l-erro = YES.
    END.

    IF l-erro THEN
        RETURN "NOK":U.
    ELSE
        RETURN "OK".
END PROCEDURE.

PROCEDURE pi-carrega-params.
    
    if avail es-api-param-acr-spf THEN DO:
        assign cEstab                    = es-api-param-acr-spf.cod_estab
               c_cod_espec_docto         = es-api-param-acr-spf.cod_espec_docto
               c_cod_ser_docto           = es-api-param-acr-spf.cod_ser_docto
               p_log_atualiza_refer_acr  = es-api-param-acr-spf.log_atualiza_refer_acr /* Atualiza lote sim ou nao */
               p_log_assume_dat_emis     = es-api-param-acr-spf.log_assume_dat_emis
               p_cod_matriz_trad_org_ext = es-api-param-acr-spf.cod_matriz_trad_org_ext
               c_cod_plano_contabil      = es-api-param-acr-spf.cod_plano_cta_ctbl
               c-cta_ctbl_fatur          = es-api-param-acr-spf.cta_trans_fatur
               c_cod_plano_ccusto        = es-api-param-acr-spf.cod_plano_ccusto
               c_cod_ccusto              = es-api-param-acr-spf.cod_ccusto      
               c-cod-unid-negoc          = es-api-param-acr-spf.cod_unid_neg
               c-tp-fluxo-financ         = es-api-param-acr-spf.cod_tip_fluxo_financ 
               c-sequencia-refer         = es-api-param-acr-spf.seq_refer
               c_cod_portador            = es-api-param-acr-spf.cod_portador
               c_cod_cart_bcia           = es-api-param-acr-spf.cod_cart_bcia
        .
    END.

    /*Busca a Refer?ncia do t?tulo*/
    run pi-retorna-referencia (output c-cod-refer).

    /*
    ASSIGN c-caminho = "\\192.168.0.131\datasul\Teste\ERP\quarentena\Spf\logIntegracao\esint023a" + STRING(TODAY,"99-99-9999") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".
    OUTPUT TO VALUE (c-caminho).
    */
    
    LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - PARAMETROS :"                             + CHR(10) +
                              "cEstab                   : " + cEstab                    + CHR(10) +
                              "c_cod_espec_docto        : " + c_cod_espec_docto         + CHR(10) +
                              "c_cod_ser_docto          : " + c_cod_ser_docto           + CHR(10) +
                              "p_log_atualiza_refer_acr : " + STRING(p_log_atualiza_refer_acr)  + CHR(10) +
                              "p_log_assume_dat_emis    : " + STRING(p_log_assume_dat_emis   )  + CHR(10) +
                              "p_cod_matriz_trad_org_ext: " + p_cod_matriz_trad_org_ext + CHR(10) +
                              "c_cod_plano_contabil     : " + c_cod_plano_contabil      + CHR(10) +
                              "c-cta_ctbl_fatur         : " + c-cta_ctbl_fatur          + CHR(10) +
                              "c_cod_plano_ccusto       : " + c_cod_plano_ccusto        + CHR(10) +
                              "c_cod_ccusto             : " + c_cod_ccusto              + CHR(10) +
                              "c-cod-unid-negoc         : " + c-cod-unid-negoc          + CHR(10) +
                              "c-tp-fluxo-financ        : " + c-tp-fluxo-financ         + CHR(10) +
                              "c-sequencia-refer        : " + c-sequencia-refer         + CHR(10) +
                              "c_cod_portador           : " + c_cod_portador            + CHR(10) +
                              "c_cod_cart_bcia          : " + c_cod_cart_bcia           + CHR(10) +
                              "c-cod-refer              : " + c-cod-refer               + CHR(10) + CHR(10) 
                              ).
    /*OUTPUT CLOSE.*/

    IF INT(SUBSTRING(c-sequencia-refer,4,2)) <> DAY(TODAY) OR
       INT(SUBSTRING(c-sequencia-refer,6,2)) <> MONTH(TODAY) OR
       INT(SUBSTRING(c-sequencia-refer,8,4)) <> YEAR(TODAY) THEN DO:
        ASSIGN c-sequencia-refer = "".
    END.
    
    IF AVAIL param_estab_acr AND c_cod_portador = "" AND c_cod_cart_bcia = "" THEN
        ASSIGN c_cod_portador  = param_estab_acr.cod_portad_padr   
               c_cod_cart_bcia = param_estab_acr.cod_cart_bcia_padr.

    FOR EACH trad_org_ext NO-LOCK
       WHERE trad_org_ext.cod_matriz_trad_org_ext = p_cod_matriz_trad_org_ext
         AND trad_org_ext.cod_tip_unid_organ      = "999"     /*998 (EMP) ou 999 (EST)*/
	     AND trad_org_ext.cod_unid_organ_ext      = cEstab. /* Est no EMS 5 */
	    
        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = trad_org_ext.cod_unid_organ NO-ERROR. /* Est no EMS 2 */
        IF AVAIL estabelec THEN
            ASSIGN c-cta_ctbl_fatur = estabelec.ct-recven.
    END.

    assign c-cod-empresa = es-api-param-acr-spf.cod_empresa.

    IF c-cod-empresa = "" THEN DO:
        FIND FIRST estabelecimento where estabelecimento.cod_estab = cEstab no-lock no-error.
        if avail estabelecimento then
            assign c-cod-empresa = estabelecimento.cod_empresa.
        else
            assign c-cod-empresa = string(i-ep-codigo-usuario).
    END.

END PROCEDURE.

PROCEDURE pi-carrega-tts.


/*     for each ttPayments NO-LOCK: */

/*     run pi-acompanhar in h-acomp (input "Integrando os Titulos no EMS5: " + string(ttPayments.nrPedido)). */


    /* como o nome abreviado vem numerico, a busca do codigo do emitente sera feita peldo CNPJ
    find emitente where emitente.nome-abrev = ttPayments.c_nom_abrev no-lock no-error.
    if avail emitente then
        assign i-cod-emitente = emitente.cod-emitente.
    else
        assign i-cod-emitente = 0.*/

    /*Criando Temp Tables*/

    /*lotes de implantacao*/
    find first tt_integr_acr_lote_impl where
               tt_integr_acr_lote_impl.tta_cod_empresa = c-cod-empresa and
               tt_integr_acr_lote_impl.tta_cod_estab   = cEstab
               no-lock no-error.
    if not avail tt_integr_acr_lote_impl then do:
        create tt_integr_acr_lote_impl.
        assign tt_integr_acr_lote_impl.tta_cod_empresa         = c-cod-empresa
               tt_integr_acr_lote_impl.tta_cod_estab           = cEstab
               tt_integr_acr_lote_impl.tta_cod_refer           = c-cod-refer
             /*tt_integr_acr_lote_impl.tta_cod_espec_docto     = "an" Esp?cie s? deve ser informada quando for Previs?o/Provis?o*/
               tt_integr_acr_lote_impl.tta_ind_tip_espec_docto = "Normal"
               tt_integr_acr_lote_impl.tta_dat_transacao       = TODAY
               tt_integr_acr_lote_impl.tta_ind_orig_tit_acr    = "ACREMS50"
               i-cont-seq = 0.
    
        LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - LOTE:"                            + CHR(10) +
                                  "cod_empresa        : " + tt_integr_acr_lote_impl.tta_cod_empresa          + CHR(10) +
                                  "cod_estab          : " + tt_integr_acr_lote_impl.tta_cod_estab            + CHR(10) +
                                  "cod_refer          : " + tt_integr_acr_lote_impl.tta_cod_refer            + CHR(10) +
                                  "ind_tip_espec_docto: " + STRING(tt_integr_acr_lote_impl.tta_ind_tip_espec_docto)  + CHR(10) +
                                  "dat_transacao      : " + STRING(tt_integr_acr_lote_impl.tta_dat_transacao      )  + CHR(10) +
                                  "ind_orig_tit_acr   : " + STRING(tt_integr_acr_lote_impl.tta_ind_orig_tit_acr)     + CHR(10) + CHR(10)
                                  ).


        assign r-recid-lote = recid(tt_integr_acr_lote_impl).

        /*
        release tt_integr_acr_lote_impl.
        find first tt_integr_acr_lote_impl no-lock.*/
    end.

    assign de-total-movto = de-total-movto + DEC(ttPayments.vlPedido)
           i-cont-seq = i-cont-seq + 1
    .
/*
ttPayments.cpfCnpj
ttPayments.nrPedido   
ttPayments.vlPedido
ttPayments.dtPedido
ttPayments.dtPagamento
*/

    /*itens dos lote de implanta??o*/
    create tt_integr_acr_item_lote_impl_8.
    assign tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr      = r-recid-lote
           tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr = recid(tt_integr_acr_item_lote_impl_8)
           tt_integr_acr_item_lote_impl_8.tta_cod_refer                  = c-cod-refer
           tt_integr_acr_item_lote_impl_8.tta_num_seq_refer              = i-cont-seq
           tt_integr_acr_item_lote_impl_8.tta_cdn_cliente                = i-cod-emitente
           tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto            = c_cod_espec_docto
           tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto        = "Normal" /*"Antecipa??o"*/
           tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto              = c_cod_ser_docto
           tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr                = "DG" + ttPayments.nrPedido
           tt_integr_acr_item_lote_impl_8.tta_cod_parcela                = "01" /*string(ttPayments.i_cod_parcela,"99")*/
           tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto             = v-dtPed
           tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr         = v-dtPag
           tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac          = v-dtPag
         /*tt_integr_acr_item_lote_impl_8.tta_cod_finalid_econ_ext       = "Corrente" deixar em branco*/
           tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ             = "Real"
           tt_integr_acr_item_lote_impl_8.tta_val_tit_acr                = DEC(ttPayments.vlPedido)
           tt_integr_acr_item_lote_impl_8.tta_cod_portador               = c_cod_portador
           tt_integr_acr_item_lote_impl_8.tta_cod_cart_bcia              = c_cod_cart_bcia
           tt_integr_acr_item_lote_impl_8.tta_val_cotac_indic_econ       = 1
           tt_integr_acr_item_lote_impl_8.tta_des_text_histor            = "Implantaá∆o autom†tica - Shopify"
/*                tt_integr_acr_item_lote_impl_8.tta_cod_cartcred               = ttPayments.c_num_cartao_cred  /*C?digo Cart?o*/   */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_mes_ano_valid_cartao   = ttPayments.c_mes_ano_validade /*Validade Cart?o*/ */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_banco                  = ttPayments.c_cod_banco                                                                    */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_agenc_bcia             = ttPayments.c_cod_agencia                                                                  */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_cta_corren_bco         = ttPayments.c_conta_corrente                                                               */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_digito_cta_corren      = ttPayments.c_dig_ccorrente                                                                */
/*                tt_integr_acr_item_lote_impl_8.tta_dat_compra_cartao_cr       = ttPayments.dat_efetivacao     /*Data Efetiva??o Venda*/                                   */
/*                tt_integr_acr_item_lote_impl_8.tta_cod_autoriz_cartao_cr      = ttPayments.c_cod_pre_autoriza /*C?d Pr?-Autoriza??o*/                                     */
/*                tt_integr_acr_item_lote_impl_8.ttv_cod_comprov_vda            = ttPayments.c_num_venda        /*Comprovante Venda*/                                       */
/*                tt_integr_acr_item_lote_impl_8.ttv_num_parc_cartcred          = ttPayments.i_num_parcelas     /*Quantidade Parcelas*/                                     */
/*                tt_integr_acr_item_lote_impl_8.ttv_cod_autoriz_bco_emissor    = ttPayments.c_cod_autoriza     /*Codigo Autoriza??o*/                                      */
/*                tt_integr_acr_item_lote_impl_8.ttv_cod_lote_origin            = ttPayments.c_lote_venda       /*Lote Orig Venda*/                                         */
/*                tt_integr_acr_item_lote_impl_8.tta_qtd_dias_carenc_juros_acr  = ttPayments.i_dias_juros       /*Dias Juros*/                                              */
/*                tt_integr_acr_item_lote_impl_8.tta_val_perc_juros_dia_atraso  = ttPayments.i_perc_juros       /*Perc Juros Dia*/                                          */
/*                tt_integr_acr_item_lote_impl_8.tta_qtd_dias_carenc_multa_acr  = ttPayments.i_dias_multa       /*Dias Multa*/                                              */
/*                tt_integr_acr_item_lote_impl_8.tta_val_perc_multa_atraso      = ttPayments.de_perc_multa      /*Perc Multa Atraso*/                                       */
/*                tt_integr_acr_item_lote_impl_8.tta_ind_tip_calc_juros         = if ttPayments.i_tipo_calc_juros = 1 then "Simples" else "Composto" /*Tipo C?lculo Juros*/ */
           tt_integr_acr_item_lote_impl_8.tta_log_liquidac_autom         = NO /*Liquidac Autom?tica*/
           .


    LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - ITEM LOTE" + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr     : " + STRING(tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr     )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr: " + STRING(tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr)  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_refer                 : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_refer                 )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_num_seq_refer             : " + STRING(tt_integr_acr_item_lote_impl_8.tta_num_seq_refer             )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cdn_cliente               : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cdn_cliente               )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto           : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto           )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto       : " + STRING(tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto       )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto             : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto             )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr               : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr               )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_parcela               : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_parcela               )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto            : " + STRING(tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto            )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr        : " + STRING(tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr        )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac         : " + STRING(tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac         )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_finalid_econ_ext      : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_finalid_econ_ext      )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ            : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ            )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_val_tit_acr               : " + STRING(tt_integr_acr_item_lote_impl_8.tta_val_tit_acr               )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_portador              : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_portador              )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_cod_cart_bcia             : " + STRING(tt_integr_acr_item_lote_impl_8.tta_cod_cart_bcia             )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_val_cotac_indic_econ      : " + STRING(tt_integr_acr_item_lote_impl_8.tta_val_cotac_indic_econ      )  + CHR(10) +
                              "tt_integr_acr_item_lote_impl_8.tta_des_text_histor           : " + STRING(tt_integr_acr_item_lote_impl_8.tta_des_text_histor           )  + CHR(10) + CHR(10)
                             ).
    /*
    if ttPayments.i_forma_pagto = 2 /*Cart?o de D?bito*/ or ttPayments.i_forma_pagto = 3 /*Cart?o de Cr?dito*/ then

        assign tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr_bco = trim(ttPayments.c_num_venda)    /*Comprovante Venda*/   + "-" +
                                                                    trim(ttPayments.c_cod_autoriza) /*Codigo Autoriza??o*/  + "-" +
                                                                    string(ttPayments.i_cod_parcela,"99").*/


    /*Apropria??es Cont?beis dos t?tulos*/
    create tt_integr_acr_aprop_ctbl_pend.
    assign tt_integr_acr_aprop_ctbl_pend.ttv_rec_item_lote_impl_tit_acr = tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr
           tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc             = c-cod-unid-negoc
           tt_integr_acr_aprop_ctbl_pend.tta_cod_tip_fluxo_financ       = c-tp-fluxo-financ
           tt_integr_acr_aprop_ctbl_pend.tta_val_aprop_ctbl             = DEC(ttPayments.vlPedido)
           tt_integr_acr_aprop_ctbl_pend.tta_cod_pais                   = ""
           tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_federac           = ""
           tt_integr_acr_aprop_ctbl_pend.tta_cod_imposto                = ""
           tt_integr_acr_aprop_ctbl_pend.tta_cod_classif_impto          = ""
           tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_cta_ctbl         = c_cod_plano_contabil
           tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl               = c-cta_ctbl_fatur /*substring(conta-contabil,1,8)*/
           tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_ccusto           = c_cod_plano_ccusto
           tt_integr_acr_aprop_ctbl_pend.tta_cod_ccusto                 = c_cod_ccusto.

    LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - APROP"  + CHR(10) +
                              "rec_item_lote_impl_tit_acr: " + STRING(tt_integr_acr_aprop_ctbl_pend.ttv_rec_item_lote_impl_tit_acr)  + CHR(10) +
                              "cod_unid_negoc            : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc            )  + CHR(10) +
                              "cod_tip_fluxo_financ      : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_tip_fluxo_financ      )  + CHR(10) +
                              "val_aprop_ctbl            : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_val_aprop_ctbl            )  + CHR(10) +
                              "cod_pais                  : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_pais                  )  + CHR(10) +
                              "cod_unid_federac          : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_federac          )  + CHR(10) +
                              "cod_imposto               : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_imposto               )  + CHR(10) +
                              "cod_classif_impto         : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_classif_impto         )  + CHR(10) +
                              "cod_plano_cta_ctbl        : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_cta_ctbl        )  + CHR(10) +
                              "cod_cta_ctbl              : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl              )  + CHR(10) +
                              "cod_plano_ccusto          : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_ccusto          )  + CHR(10) +
                              "cod_ccusto                : " + STRING(tt_integr_acr_aprop_ctbl_pend.tta_cod_ccusto                )  + CHR(10) + CHR(10)
                              ).
    /*release tt_integr_acr_aprop_ctbl_pend.*/
    /*end.*/

    for each tt_integr_acr_item_lote_impl_8:
        create tt_integr_acr_item_lote_impl.
        buffer-copy tt_integr_acr_item_lote_impl_8
                 to tt_integr_acr_item_lote_impl.
    end.
END PROCEDURE.

PROCEDURE pi-chama-api.

    EMPTY TEMP-TABLE tt_log_erros_atualiz.

    run prgfin/acr/acr900zi.py persistent set v_hdl_programa.

    LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - Antes da chamada da API" +  CHR(10) +
                                                 string(p_cod_matriz_trad_org_ext) + CHR(10) +
                                                 string(p_log_atualiza_refer_acr ) + CHR(10) +
                                                 string(p_log_assume_dat_emis    ) + CHR(10) +
                                                 "HANDLE v_hdl_programa :" + STRING(VALID-HANDLE(v_hdl_programa)) + CHR(10) + CHR(10) 
                              ).

    run pi_main_code_integr_acr_new_9 in v_hdl_programa (input 11,
                                                         input p_cod_matriz_trad_org_ext,
                                                         input p_log_atualiza_refer_acr,
                                                         input p_log_assume_dat_emis,
                                                         input table tt_integr_acr_repres_comis_2,
                                                         input-output table tt_integr_acr_item_lote_impl_8,
                                                         input table tt_integr_acr_aprop_relacto_2).
    
    LOG-MANAGER:WRITE-MESSAGE("esspf023a.p - AP‡S da chamada da API" +  CHR(10)).
    delete procedure v_hdl_programa.

END PROCEDURE.


PROCEDURE pi-erros-integr.


    LOG-MANAGER:WRITE-MESSAGE("esspf023A.p - pi-erros-integr - ACR? " + STRING(CAN-FIND (FIRST tt_log_erros_atualiz)) + CHR(10) + CHR(10) ).
    /*
    MESSAGE "pi-erros-integr - ACR" SKIP
            CAN-FIND (FIRST tt_log_erros_atualiz NO-LOCK).
    */

    IF CAN-FIND (FIRST tt_log_erros_atualiz NO-LOCK) THEN DO:
        for each tt_log_erros_atualiz:
        
            if tt_log_erros_atualiz.tta_num_seq_refer = 0 /*Erro generico*/ then do:
        
                find first b_tit_acr where  /*verifica se o titulo foi integrado no EMS5*/
                           b_tit_acr.cod_estab       = cEstab and
                           b_tit_acr.cod_espec_docto = c_cod_espec_docto and
                           b_tit_acr.cod_ser_docto   = c_cod_ser_docto and
                           b_tit_acr.cod_tit_acr     = ttPayments.nrPedido and
                           b_tit_acr.cod_parcela     = "01" no-lock no-error.
                if not avail b_tit_acr then do:
    
                    assign c-msg-erro  = tt_log_erros_atualiz.ttv_des_msg_erro
                           c_msg_ajuda = tt_log_erros_atualiz.ttv_des_msg_ajuda
                           i-cod-erro  = tt_log_erros_atualiz.ttv_num_mensagem.
    
                    RUN piErro(INPUT i-cod-erro,
                               INPUT c-msg-erro,
                               INPUT c-msg-erro
                               ).
                end.
            end.
        
            else do:
        
                assign c-msg-erro  = tt_log_erros_atualiz.ttv_des_msg_erro
                       c_msg_ajuda = tt_log_erros_atualiz.ttv_des_msg_ajuda
                       i-cod-erro  = tt_log_erros_atualiz.ttv_num_mensagem.
    
                RUN piErro(INPUT i-cod-erro,
                           INPUT c-msg-erro,
                           INPUT c-msg-erro
                           ).
                
            end.
        
        
            /*
            message
            "estab          " tt_log_erros_atualiz.tta_cod_estab                    skip
            "refer          " tt_log_erros_atualiz.tta_cod_refer                    skip
            "seq-refer      " tt_log_erros_atualiz.tta_num_seq_refer                skip
            "num-msg        " tt_log_erros_atualiz.ttv_num_mensagem                 skip
            "des-msg        " tt_log_erros_atualiz.ttv_des_msg_erro                 skip
            "des-msg-ajuda  " tt_log_erros_atualiz.ttv_des_msg_ajuda                skip
            "ind_tp_relacto " tt_log_erros_atualiz.ttv_ind_tip_relacto              skip
            "num_relacto    " tt_log_erros_atualiz.ttv_num_relacto                 view-as alert-box.
            */
        end.

        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Erro").
    END.
    ELSE DO:
        RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "dev.progress4gl@gmail.com", "Sucesso").
        /*MESSAGE "piEnviaNotificacaoUsuario: dev.progress4gl@gmail.com".*/
    END.
END PROCEDURE.

Procedure pi-retorna-referencia:

    define output param p-cod-refer as character format "x(10)" no-undo.

    def var i-pos-letra as integer no-undo.

    def var c-letra as char format "!"  no-undo.
    def var i-seq   as int  format "99" no-undo.
    def var dt-data    as date format "99/99/9999" no-undo.

    def var c-lista as char format "!"
        initial "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z" no-undo.

DEF VAR c-refer AS CHAR.
DEF VAR i AS INTEGER.

/* data invertida */
c-refer = SUBSTRING(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99").

/* primeiro caracter maiusculo */
c-refer = c-refer + CHR( RANDOM(65,90) ).

/* 3 caracteres minusculos */
DO i = 1 TO 3.
    c-refer = c-refer + CHR( RANDOM(97,122) ).
END.

assign p-cod-refer = c-refer.

/*
    find FIRST b-es-api-param-acr-spf EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL b-es-api-param-acr-spf THEN
        assign c-sequencia-refer = b-es-api-param-acr-spf.seq_refer.

    dt-data = ?.
    IF c-sequencia-refer <> "" THEN
        ASSIGN dt-data = date(int(substr(c-sequencia-refer,6,2)),int(substr(c-sequencia-refer,4,2)),int(substr(c-sequencia-refer,8,4))).
    
    if c-sequencia-refer = "" OR dt-data <> TODAY then do:
        assign c-letra = "A"
               i-seq   = 1
               dt-data    = today.
    end.

    else do:
        assign c-letra = substr(c-sequencia-refer,1,1)
               i-seq   = int(substr(c-sequencia-refer,2,2))
               dt-data    = date(int(substr(c-sequencia-refer,6,2)),int(substr(c-sequencia-refer,4,2)),int(substr(c-sequencia-refer,8,4))).

        if dt-data = today then do:

            if i-seq = 99 then do:

                assign i-pos-letra = lookup(c-letra,c-lista).

                assign c-letra = entry((i-pos-letra + 1),c-lista)
                       i-seq   = 1.
            end.
            else
                assign i-seq = i-seq + 1.

        end.

        else
            assign c-letra = "A"
                   i-seq   = 1
                   dt-data    = today.
    end.

    assign b-es-api-param-acr-spf.seq_refer = c-letra + string(i-seq,"99") + string(day(dt-data),"99") + string(month(dt-data),"99") + string(year(dt-data),"9999").

    assign p-cod-refer = "S" + substring(string(year(dt-data),"9999"),3,2) + string(month(dt-data),"99") + string(day(dt-data),"99") + c-letra + string(i-seq,"99").
*/
end procedure.


PROCEDURE piErro:
    DEFINE INPUT PARAM cErrorNumber        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAM cErrorDescription   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM cErrorHelp          AS CHARACTER NO-UNDO.
    
    CREATE RowErrors.
    ASSIGN iErro                      = iErro + 1
           RowErrors.ErrorSequence    = iErro
           RowErrors.ErrorNumber      = cErrorNumber /*17006*/
           RowErrors.ErrorType        = "Error"
           RowErrors.ErrorDescription = cErrorDescription
           RowErrors.ErrorHelp        = cErrorHelp.

END PROCEDURE.


PROCEDURE piEnviaNotificacaoUsuario:
   DEFINE INPUT PARAM pRemetente  AS CHAR NO-UNDO.
   DEFINE INPUT PARAM pDestino    AS CHAR NO-UNDO.
   DEFINE INPUT PARAM pAcaoEmail  AS CHAR NO-UNDO.
   
  
   DEFINE VARIABLE cMensagem   AS CHAR NO-UNDO.
   DEFINE VARIABLE cAssunto    AS CHAR NO-UNDO.
   DEFINE VARIABLE cDestino    AS CHAR NO-UNDO.
   DEFINE VARIABLE cRemetente  AS CHAR NO-UNDO.

   /*

   ASSIGN cDestino   = pDestino
          cRemetente = pRemetente.

   run utp/utapi019.p persistent set h-utapi019.           
   for each tt-envio2:                                     
       delete tt-envio2.                                   
   END.                                                    
   for each tt-mensagem:                                   
       delete tt-mensagem.                                 
   END.

   
   ASSIGN cAssunto  = "Integraá∆o de t°tulos ACR - SHOPIFY".

   IF pAcaoEmail = "SUCESSO" THEN
   DO:
      ASSIGN cMensagem = SUBSTITUTE("Prezado(s), Informo que foi registrado pagamento do pedido &1 do cliente &2 - CPF/CNPJ &3",
                         ttPayments.nrPedido, emitente.nome-emit, ttPayments.cpfCnpj).

   END.
   ELSE
   DO:
      ASSIGN cMensagem = SUBSTITUTE("Prezado(s), Informo que foram gerados, erros ao tentar integrar o pagamento do pedido &1 do cliente &2 de CPF/CNPJ &3, verifique o monitor de integraá∆o.", 
                         ttPayments.nrPedido, emitente.nome-emit, ttPayments.cpfCnpj).
   END.

/*    MESSAGE "Email" SKIP                          */
/*            "cDestino  : " cDestino    skip       */
/*            "cRemetente: " cRemetente  skip       */
/*            "cAssunto  : " cAssunto    skip       */
/*            "cMensagem : " cMensagem   skip       */
/*        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

   CREATE tt-envio2.                                       
   ASSIGN tt-envio2.versao-integracao = 1                  
            tt-envio2.destino           = cDestino     
            tt-envio2.remetente         = cRemetente         
            tt-envio2.assunto           = cAssunto           
            tt-envio2.mensagem          = cMensagem             
            tt-envio2.importancia       = 2                  
            tt-envio2.log-enviada       = no                 
            tt-envio2.log-lida          = no                 
            tt-envio2.acomp             = no                 
            tt-envio2.formato           = "HTML"             
            tt-envio2.arq-anexo         = "".            
                                                            
   CREATE tt-mensagem.                                     
   ASSIGN tt-mensagem.seq-mensagem = 1                     
            tt-mensagem.mensagem = cMensagem.                   
                                                            
   run pi-execute2 in h-utapi019 (input  table tt-envio2  ,
                                  input  table tt-mensagem, 
                                  output table tt-erros).
   IF RETURN-VALUE <> "OK" THEN
   DO:
      DELETE PROCEDURE h-utapi019.

        RUN piErro(INPUT 17006,
                   INPUT "Houveram erros ao enviar notificaá‰es",
                   INPUT "Houveram erros ao enviar notificaá‰es").
      
      FOR EACH tt-erros:
         RUN piErro(INPUT 17006,
                   INPUT SUBSTITUTE("&1 - &2", tt-erros.desc-erro),
                   INPUT SUBSTITUTE("&1 - &2", tt-erros.desc-erro)).      
      END.
      RETURN "NOK".
   END.

   DELETE PROCEDURE h-utapi019.
   */
                                                         
   RETURN "OK".
END.


PROCEDURE ativarClientlog:
DEFINE VARIABLE cHora AS CHARACTER   NO-UNDO.

cHora = STRING(TIME, "HH:MM:SS").

SESSION:DEBUG-ALERT = YES.

ASSIGN LOG-MANAGER:LOGFILE-NAME     = 
    "c:\temp\clientlog23_"  + 
    STRING(YEAR(today) , "9999") + 
    STRING(MONTH(today) , "9999") + 
    STRING(DAY(today) , "9999") + 
    SUBSTRING(chora, 1, 2) +
    SUBSTRING(chora, 4, 2) +
    SUBSTRING(chora, 7, 2) + ".txt"
    .

ASSIGN LOG-MANAGER:LOGGING-LEVEL = 4
       LOG-MANAGER:LOG-ENTRY-TYPES = "4GLMessages,4GLTrace,FileID".
/* ASSIGN LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTrace".  */
/* ASSIGN LOG-MANAGER:LOGGING-LEVEL = 2            . */


/*                                                     */
/* -clientlog  mylog.lg -logginglevel 2 -logentrytypes */
/* 4GLTrace                                            */


END PROCEDURE.

PROCEDURE desativar-clientlog.

    ASSIGN SESSION:DEBUG-ALERT = NO.
    LOG-MANAGER:CLOSE-LOG().

END PROCEDURE.
