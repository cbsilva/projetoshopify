/********************************************************************************
***
*** hpp/acr900zi.i - Temp-Tables API Implanta‡Æo de Documentos CR.
***
********************************************************************************/

/*Temp-table 1: Gerar  os abatimentos de antecipa‡äes.*/

def new shared temp-table tt_integr_acr_abat_antecip no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field ttv_rec_abat_antecip_acr         as recid format ">>>>>>9"
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_acr                  as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_abtdo_antecip_tit_abat   as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Abtdo" column-label "Vl Abtdo"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_estab                    ascending
          tta_cod_estab_ext                ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cod_tit_acr                  ascending
          tta_cod_parcela                  ascending
    .

/*Temp-table 2: Gerar  abatimento de previsÆo ACR.*/

def new shared temp-table tt_integr_acr_abat_prev no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_acr                  as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_abtdo_prev_tit_abat      as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Abat" column-label "Vl Abat"
    field tta_log_zero_sdo_prev            as logical format "Sim/NÆo" initial no label "Zera Saldo" column-label "Zera Saldo"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_estab                    ascending
          tta_cod_estab_ext                ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cod_tit_acr                  ascending
          tta_cod_parcela                  ascending
    .

/*Temp-table 3: Gerar  as apropria‡äes cont beis.*/

def new shared temp-table tt_integr_acr_aprop_ctbl_pend no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
    field tta_cod_cta_ctbl_ext             as character format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext         as character format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_unid_negoc_ext           as character format "x(8)" label "Unid Neg¢cio Externa" column-label "Unid Neg¢cio Externa"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_ccusto_ext               as character format "x(8)" label "Centro Custo Externo" column-label "CCusto Externo"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_cod_fluxo_financ_ext         as character format "x(20)" label "Tipo Fluxo Externo" column-label "Tipo Fluxo Externo"
    field tta_val_aprop_ctbl               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federa‡Æo" column-label "UF"
    field tta_log_impto_val_agreg          as logical format "Sim/NÆo" initial no label "Impto Val Agreg" column-label "Imp Vl Agr"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_cod_pais                     as character format "x(3)" label "Pa¡s" column-label "Pa¡s"
    field tta_cod_pais_ext                 as character format "x(20)" label "Pa¡s Externo" column-label "Pa¡s Externo"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_cta_ctbl_ext             ascending
          tta_cod_sub_cta_ctbl_ext         ascending
          tta_cod_unid_negoc               ascending
          tta_cod_unid_negoc_ext           ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_ccusto_ext               ascending
          tta_cod_tip_fluxo_financ         ascending
          tta_cod_fluxo_financ_ext         ascending
          tta_log_impto_val_agreg          ascending
    .


/*Temp-table 4: Armazena os valores de receita e despesa rateados por unidade de neg¢cio e tipo de fluxo.*/

def new shared temp-table tt_integr_acr_aprop_desp_rec no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_cta_ctbl_ext             as character format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext         as character format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field tta_cod_unid_negoc_ext           as character format "x(8)" label "Unid Neg¢cio Externa" column-label "Unid Neg¢cio Externa"
    field tta_cod_fluxo_financ_ext         as character format "x(20)" label "Tipo Fluxo Externo" column-label "Tipo Fluxo Externo"
    field tta_val_perc_rat_ctbz            as decimal format ">>9.99" decimals 2 initial 0 label "Perc Rateio" column-label "% Rat"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_tip_abat                 as character format "x(8)" label "Tipo de Abatimento" column-label "Tipo de Abatimento"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_ind_tip_aprop_recta_despes   as character format "x(20)" label "Tipo Apropria‡Æo" column-label "Tipo Apropria‡Æo"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_cta_ctbl_ext             ascending
          tta_cod_sub_cta_ctbl_ext         ascending
          tta_cod_unid_negoc_ext           ascending
          tta_cod_fluxo_financ_ext         ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_unid_negoc               ascending
          tta_cod_tip_fluxo_financ         ascending
          tta_ind_tip_aprop_recta_despes   ascending
          tta_cod_tip_abat                 ascending
    .


/*Temp-table 5: Armazena os valores de  rateio das liquida‡äes contra antecipa‡Æo.*/

def new shared temp-table tt_integr_acr_aprop_liq_antec no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field ttv_rec_abat_antecip_acr         as recid format ">>>>>>9"
    field tta_cod_fluxo_financ_ext         as character format "x(20)" label "Tipo Fluxo Externo" column-label "Tipo Fluxo Externo"
    field ttv_cod_fluxo_financ_tit_ext     as character format "x(20)"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_cod_unid_negoc_tit           as character format "x(3)" label "Unid Negoc T¡tulo" column-label "Unid Negoc T¡tulo"
    field tta_cod_tip_fluxo_financ_tit     as character format "x(12)" label "Tp Fluxo Financ Tit" column-label "Tp Fluxo Financ Tit"
    field tta_val_abtdo_antecip            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Abatido" column-label "Vl Abatido"
    .

/*Temp-table 6: Contˆm os valores das apropria‡äes cont beis dos outros t¡tulos relacionados ao item do lote de implanta‡Æo.*/

def new shared temp-table tt_integr_acr_aprop_relacto no-undo 
    field ttv_rec_relacto_pend_tit_acr     as recid format ">>>>>>9" initial ?
    field tta_cod_cta_ctbl_ext             as character format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext         as character format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field tta_cod_unid_negoc_ext           as character format "x(8)" label "Unid Neg¢cio Externa" column-label "Unid Neg¢cio Externa"
    field tta_cod_fluxo_financ_ext         as character format "x(20)" label "Tipo Fluxo Externo" column-label "Tipo Fluxo Externo"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_val_aprop_ctbl               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field tta_ind_tip_aprop_ctbl           as character format "x(30)" initial "Saldo" label "Tipo Aprop Ctbl" column-label "Tipo Aprop Ctbl"
    .

/*Temp-table 7: sÆo as informa‡äes dos cheques usados no pagamento de t¡tulos ACR.*/

def new shared temp-table tt_integr_acr_cheq no-undo 
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco" 
    field tta_cod_agenc_bcia               as character format "x(10)" label "Agˆncia Banc ria" column-label "Agˆncia Banc ria" 
    field tta_cod_cta_corren               as character format "x(10)" label "Conta Corrente" column-label "Cta Corrente" 
    field tta_num_cheque                   as integer format ">>>>,>>>,>>9" initial ? label "Num Cheque" column-label "Num Cheque" 
    field tta_dat_emis_cheq                as date format "99/99/9999" initial ? label "Data EmissÆo" column-label "Dt Emiss" 
    field tta_dat_depos_cheq_acr           as date format "99/99/9999" initial ? label "Dep¢sito" column-label "Dep¢sito" 
    field tta_dat_prev_depos_cheq_acr      as date format "99/99/9999" initial ? label "PrevisÆo Dep¢sito" column-label "PrevisÆo Dep¢sito" 
    field tta_dat_desc_cheq_acr            as date format "99/99/9999" initial ? label "Data Desconto" column-label "Data Desconto" 
    field tta_dat_prev_desc_cheq_acr       as date format "99/99/9999" initial ? label "Data Prev Desc" column-label "Data Prev Desc" 
    field tta_val_cheque                   as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Cheque" column-label "Valor Cheque" 
    field tta_nom_emit                     as character format "x(40)" label "Nome Emitente" column-label "Nome Emitente" 
    field tta_nom_cidad_emit               as character format "x(30)" label "Cidade Emitente" column-label "Cidade Emitente" 
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab" 
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext" 
    field tta_cod_id_feder                 as character format "x(20)" initial ? label "ID Federal" column-label "ID Federal" 
    field tta_cod_motiv_devol_cheq         as character format "x(5)" label "Motivo Devolu‡Æo" column-label "Motivo Devolu‡Æo" 
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda" 
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa" 
    field tta_cod_usuar_cheq_acr_terc      as character format "x(12)" label "Usu rio" column-label "Usu rio" 
    field tta_log_pend_cheq_acr            as logical format "Sim/NÆo" initial no label "Cheque Pendente" column-label "Cheque Pendente" 
    field tta_log_cheq_terc                as logical format "Sim/NÆo" initial no label "Cheque Terceiro" column-label "Cheque Terceiro" 
    field tta_log_cheq_acr_renegoc         as logical format "Sim/NÆo" initial no label "Cheque Reneg" column-label "Cheque Reneg" 
    field tta_log_cheq_acr_devolv          as logical format "Sim/NÆo" initial no label "Cheque Devolvido" column-label "Cheque Devolvido" 
    field tta_num_pessoa                   as integer format ">>>,>>>,>>9" initial ? label "Pessoa" column-label "Pessoa" 
    field tta_cod_pais                     as character format "x(3)" label "Pa¡s" column-label "Pa¡s" 
    index tt_id                            is primary unique 
          tta_cod_banco                    ascending 
          tta_cod_agenc_bcia               ascending 
          tta_cod_cta_corren               ascending 
          tta_num_cheque                   ascending 
    . 


/*Temp-table 8: Indicar  os impostos vinculados ao t¡tulo.*/

def new shared temp-table tt_integr_acr_impto_impl_pend no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_pais                     as character format "x(3)" label "Pa¡s" column-label "Pa¡s"
    field tta_cod_pais_ext                 as character format "x(20)" label "Pa¡s Externo" column-label "Pa¡s Externo"
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federa‡Æo" column-label "UF"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequˆncia" column-label "NumSeq"
    field tta_val_rendto_tribut            as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Rendto Tribut vel" column-label "Vl Rendto Tribut"
    field tta_val_aliq_impto               as decimal format ">9.99" decimals 2 initial 0.00 label "Al¡quota" column-label "Aliq"
    field tta_val_imposto                  as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Vl Imposto"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
    field tta_cod_cta_ctbl_ext             as character format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext         as character format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field tta_ind_clas_impto               as character format "X(14)" initial "Retido" label "Classe Imposto" column-label "Classe Imposto"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_val_cotac_indic_econ         as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cota‡Æo" column-label "Cota‡Æo"
    field tta_dat_cotac_indic_econ         as date format "99/99/9999" initial ? label "Data Cota‡Æo" column-label "Data Cota‡Æo"
    field tta_val_impto_indic_econ_impto   as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Val Finalid Impto" column-label "Val Finalid Impto"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_pais                     ascending
          tta_cod_pais_ext                 ascending
          tta_cod_unid_federac             ascending
          tta_cod_imposto                  ascending
          tta_cod_classif_impto            ascending
          tta_num_seq                      ascending
    . 

/*Temp-table 9: Gerar  item do lote de implanta‡Æo - Esta temp-table apenas estar  definida, sendo que os registros serÆo criados na temp-table tt_integr_acr_item_lote_impl_8.*/

def new shared temp-table tt_integr_acr_item_lote_impl no-undo
    field ttv_rec_lote_impl_tit_acr        as recid format ">>>>>>9" initial ?
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field tta_cdn_cliente                  as Integer format ">>>,>>>,>>9" initial 0 label "Cliente" column-label "Cliente"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_acr                  as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_portad_ext               as character format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_cart_bcia                as character format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_modalid_ext              as character format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field tta_cod_cond_cobr                as character format "x(8)" label "Condi‡Æo Cobran‡a" column-label "Cond Cobran‡a"
    field tta_cod_motiv_movto_tit_acr      as character format "x(8)" label "Motivo Movimento" column-label "Motivo Movimento"
    field tta_cod_histor_padr              as character format "x(8)" label "Hist¢rico PadrÆo" column-label "Hist¢rico PadrÆo"
    field tta_cdn_repres                   as Integer format ">>>,>>9" initial 0 label "Representante" column-label "Representante"
    field tta_dat_vencto_tit_acr           as date format "99/99/9999" initial ? label "Vencimento" column-label "Vencimento"
    field tta_dat_prev_liquidac            as date format "99/99/9999" initial ? label "Prev Liquida‡Æo" column-label "Prev Liquida‡Æo"
    field tta_dat_desconto                 as date format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  EmissÆo" column-label "Dt EmissÆo"
    field tta_val_tit_acr                  as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor" column-label "Valor"
    field tta_val_desconto                 as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_val_perc_desc                as decimal format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_val_perc_juros_dia_atraso    as decimal format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_val_perc_multa_atraso        as decimal format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_val_base_calc_comis          as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Base Calc Comis" column-label "Base Calc Comis"
    field tta_des_text_histor              as character format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_qtd_dias_carenc_multa_acr    as decimal format ">>9" initial 0 label "Dias Carenc Multa" column-label "Dias Carenc Multa"
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_agenc_bcia               as character format "x(10)" label "Agˆncia Banc ria" column-label "Agˆncia Banc ria"
    field tta_cod_cta_corren_bco           as character format "x(20)" label "Conta Corrente Banco" column-label "Conta Corrente Banco"
    field tta_cod_digito_cta_corren        as character format "x(2)" label "D¡gito Cta Corrente" column-label "D¡gito Cta Corrente"
    field tta_cod_instruc_bcia_1_movto     as character format "x(4)" label "Instr Banc ria 1" column-label "Instr Banc 1"
    field tta_cod_instruc_bcia_2_movto     as character format "x(4)" label "Instr Banc ria 2" column-label "Instr Banc 2"
    field tta_qtd_dias_carenc_juros_acr    as decimal format ">>9" initial 0 label "Dias Carenc Juros" column-label "Dias Juros"
    field tta_val_liq_tit_acr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Vl L¡quido" column-label "Vl L¡quido"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp‚cie" column-label "Tipo Esp‚cie"
    field tta_cod_cond_pagto               as character format "x(8)" label "Condi‡Æo Pagamento" column-label "Condi‡Æo Pagamento"
    field ttv_cdn_agenc_fp                 as Integer format ">>>9" label "Agˆncia"
    field tta_ind_sit_tit_acr              as character format "X(13)" initial "Normal" label "Situa‡Æo T¡tulo" column-label "Situa‡Æo T¡tulo"
    field tta_log_liquidac_autom           as logical format "Sim/NÆo" initial no label "Liquidac Autom tica" column-label "Liquidac Autom tica"
    field tta_num_id_tit_acr               as integer format "9999999999" initial 0 label "Token Cta Receber" column-label "Token Cta Receber"
    field tta_num_id_movto_tit_acr         as integer format "9999999999" initial 0 label "Token Movto Tit  ACR" column-label "Token Movto Tit  ACR"
    field tta_num_id_movto_cta_corren      as integer format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field tta_cod_admdra_cartao_cr         as character format "x(5)" label "Administradora" column-label "Administradora"
    field tta_cod_cartcred                 as character format "x(20)" label "C¢digo CartÆo" column-label "C¢digo CartÆo"
    field tta_cod_mes_ano_valid_cartao     as character format "XX/XXXX" label "Validade CartÆo" column-label "Validade CartÆo"
    field tta_cod_autoriz_cartao_cr        as character format "x(6)" label "C¢d Pr‚-Autoriza‡Æo" column-label "C¢d Pr‚-Autoriza‡Æo"
    field tta_dat_compra_cartao_cr         as date format "99/99/9999" initial ? label "Data Efetiv Venda" column-label "Data Efetiv Venda"
    field tta_cod_conces_telef             as character format "x(5)" label "Concession ria" column-label "Concession ria"
    field tta_num_ddd_localid_conces       as integer format "999" initial 0 label "DDD" column-label "DDD"
    field tta_num_prefix_localid_conces    as integer format ">>>9" initial 0 label "Prefixo" column-label "Prefixo"
    field tta_num_milhar_localid_conces    as integer format "9999" initial 0 label "Milhar" column-label "Milhar"
    field tta_log_tip_cr_perda_dedut_tit   as logical format "Sim/NÆo" initial no label "Credito com Garantia" column-label "Cred Garant"
    field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_ind_ender_cobr               as character format "X(15)" initial "Cliente" label "Endere‡o Cobran‡a" column-label "Endere‡o Cobran‡a"
    field tta_nom_abrev_contat             as character format "x(15)" label "Abreviado Contato" column-label "Abreviado Contato"
    field tta_log_db_autom                 as logical format "Sim/NÆo" initial no label "D‚bito Autom tico" column-label "D‚bito Autom tico"
    field tta_log_destinac_cobr            as logical format "Sim/NÆo" initial no label "Destin Cobran‡a" column-label "Destin Cobran‡a"
    field tta_ind_sit_bcia_tit_acr         as character format "X(12)" initial "Liberado" label "Sit Banc ria" column-label "Sit Banc ria"
    field tta_cod_tit_acr_bco              as character format "x(20)" label "Num T¡tulo Banco" column-label "Num T¡tulo Banco"
    field tta_cod_agenc_cobr_bcia          as character format "x(10)" label "Agˆncia Cobran‡a" column-label "Agˆncia Cobr"
    field tta_dat_abat_tit_acr             as date format "99/99/9999" initial ? label "Abat" column-label "Abat"
    field tta_val_perc_abat_acr            as decimal format ">>9.9999" decimals 4 initial 0 label "Perc Abatimento" column-label "Abatimento"
    field tta_val_abat_tit_acr             as decimal format ">>>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Abatimento" column-label "Vl Abatimento"
    field tta_des_obs_cobr                 as character format "x(40)" label "Obs Cobran‡a" column-label "Obs Cobran‡a"
    index tt_id                            is primary unique
          ttv_rec_lote_impl_tit_acr        ascending
          tta_num_seq_refer                ascending
    .


/*Temp-table 10: Gerar  item do lote de implanta‡Æo - Nesta temp-table deverÆo ser criados os registros.*/

def temp-table tt_integr_acr_item_lote_impl_8 no-undo
    field ttv_rec_lote_impl_tit_acr        as recid format ">>>>>>9" initial ?
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field tta_cdn_cliente                  as Integer format ">>>,>>>,>>9" initial 0 label "Cliente" column-label "Cliente"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_acr                  as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_portad_ext               as character format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_cart_bcia                as character format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_modalid_ext              as character format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field tta_cod_cond_cobr                as character format "x(8)" label "Condi‡Æo Cobran‡a" column-label "Cond Cobran‡a"
    field tta_cod_motiv_movto_tit_acr      as character format "x(8)" label "Motivo Movimento" column-label "Motivo Movimento"
    field tta_cod_histor_padr              as character format "x(8)" label "Hist¢rico PadrÆo" column-label "Hist¢rico PadrÆo"
    field tta_cdn_repres                   as Integer format ">>>,>>9" initial 0 label "Representante" column-label "Representante"
    field tta_dat_vencto_tit_acr           as date format "99/99/9999" initial ? label "Vencimento" column-label "Vencimento"
    field tta_dat_prev_liquidac            as date format "99/99/9999" initial ? label "Prev Liquida‡Æo" column-label "Prev Liquida‡Æo"
    field tta_dat_desconto                 as date format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  EmissÆo" column-label "Dt EmissÆo"
    field tta_val_tit_acr                  as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor" column-label "Valor"
    field tta_val_desconto                 as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_val_perc_desc                as decimal format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_val_perc_juros_dia_atraso    as decimal format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_val_perc_multa_atraso        as decimal format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_val_base_calc_comis          as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Base Calc Comis" column-label "Base Calc Comis"
    field tta_des_text_histor              as character format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_qtd_dias_carenc_multa_acr    as decimal format ">>9" initial 0 label "Dias Carenc Multa" column-label "Dias Carenc Multa"
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_agenc_bcia               as character format "x(10)" label "Agˆncia Banc ria" column-label "Agˆncia Banc ria"
    field tta_cod_cta_corren_bco           as character format "x(20)" label "Conta Corrente Banco" column-label "Conta Corrente Banco"
    field tta_cod_digito_cta_corren        as character format "x(2)" label "D¡gito Cta Corrente" column-label "D¡gito Cta Corrente"
    field tta_cod_instruc_bcia_1_movto     as character format "x(4)" label "Instr Banc ria 1" column-label "Instr Banc 1"
    field tta_cod_instruc_bcia_2_movto     as character format "x(4)" label "Instr Banc ria 2" column-label "Instr Banc 2"
    field tta_qtd_dias_carenc_juros_acr    as decimal format ">>9" initial 0 label "Dias Carenc Juros" column-label "Dias Juros"
    field tta_val_liq_tit_acr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Vl L¡quido" column-label "Vl L¡quido"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp‚cie" column-label "Tipo Esp‚cie"
    field tta_cod_cond_pagto               as character format "x(8)" label "Condi‡Æo Pagamento" column-label "Condi‡Æo Pagamento"
    field ttv_cdn_agenc_fp                 as Integer format ">>>9" label "Agˆncia"
    field tta_ind_sit_tit_acr              as character format "X(13)" initial "Normal" label "Situa‡Æo T¡tulo" column-label "Situa‡Æo T¡tulo"
    field tta_log_liquidac_autom           as logical format "Sim/NÆo" initial no label "Liquidac Autom tica" column-label "Liquidac Autom tica"
    field tta_num_id_tit_acr               as integer format "9999999999" initial 0 label "Token Cta Receber" column-label "Token Cta Receber"
    field tta_num_id_movto_tit_acr         as integer format "9999999999" initial 0 label "Token Movto Tit  ACR" column-label "Token Movto Tit  ACR"
    field tta_num_id_movto_cta_corren      as integer format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field tta_cod_admdra_cartao_cr         as character format "x(5)" label "Administradora" column-label "Administradora"
    field tta_cod_cartcred                 as character format "x(20)" label "C¢digo CartÆo" column-label "C¢digo CartÆo"
    field tta_cod_mes_ano_valid_cartao     as character format "XX/XXXX" label "Validade CartÆo" column-label "Validade CartÆo"
    field tta_cod_autoriz_cartao_cr        as character format "x(6)" label "C¢d Pr‚-Autoriza‡Æo" column-label "C¢d Pr‚-Autoriza‡Æo"
    field tta_dat_compra_cartao_cr         as date format "99/99/9999" initial ? label "Data Efetiv Venda" column-label "Data Efetiv Venda"
    field tta_cod_conces_telef             as character format "x(5)" label "Concession ria" column-label "Concession ria"
    field tta_num_ddd_localid_conces       as integer format "999" initial 0 label "DDD" column-label "DDD"
    field tta_num_prefix_localid_conces    as integer format ">>>9" initial 0 label "Prefixo" column-label "Prefixo"
    field tta_num_milhar_localid_conces    as integer format "9999" initial 0 label "Milhar" column-label "Milhar"
    field tta_log_tip_cr_perda_dedut_tit   as logical format "Sim/NÆo" initial no label "Credito com Garantia" column-label "Cred Garant"
    field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_ind_ender_cobr               as character format "X(15)" initial "Cliente" label "Endere‡o Cobran‡a" column-label "Endere‡o Cobran‡a"
    field tta_nom_abrev_contat             as character format "x(15)" label "Abreviado Contato" column-label "Abreviado Contato"
    field tta_log_db_autom                 as logical format "Sim/NÆo" initial no label "D‚bito Autom tico" column-label "D‚bito Autom tico"
    field tta_log_destinac_cobr            as logical format "Sim/NÆo" initial no label "Destin Cobran‡a" column-label "Destin Cobran‡a"
    field tta_ind_sit_bcia_tit_acr         as character format "X(12)" initial "Liberado" label "Sit Banc ria" column-label "Sit Banc ria"
    field tta_cod_tit_acr_bco              as character format "x(20)" label "Num T¡tulo Banco" column-label "Num T¡tulo Banco"
    field tta_cod_agenc_cobr_bcia          as character format "x(10)" label "Agˆncia Cobran‡a" column-label "Agˆncia Cobr"
    field tta_dat_abat_tit_acr             as date format "99/99/9999" initial ? label "Abat" column-label "Abat"
    field tta_val_perc_abat_acr            as decimal format ">>9.9999" decimals 4 initial 0 label "Perc Abatimento" column-label "Abatimento"
    field tta_val_abat_tit_acr             as decimal format ">>>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Abatimento" column-label "Vl Abatimento"
    field tta_des_obs_cobr                 as character format "x(40)" label "Obs Cobran‡a" column-label "Obs Cobran‡a"
    field tta_val_cotac_indic_econ         as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cota‡Æo" column-label "Cota‡Æo"
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_ind_tip_calc_juros           as character format "x(10)" initial "Simples" label "Tipo C lculo Juros" column-label "Tipo C lculo Juros"
    field ttv_cod_comprov_vda              as character format "x(12)" label "Comprovante Venda" column-label "Comprovante Venda"
    field ttv_num_parc_cartcred            as integer format ">9" label "Quantidade Parcelas" column-label "Quantidade Parcelas"
    field ttv_cod_autoriz_bco_emissor      as character format "x(6)" label "Codigo Autoriza‡Æo" column-label "Codigo Autoriza‡Æo"
    field ttv_cod_lote_origin              as character format "x(7)" label "Lote Orig Venda" column-label "Lote Orig Venda"
    field ttv_cod_estab_vendor             as character format "x(5)" label "Estabelecimento" column-label "Estabelecimento"
    field ttv_num_planilha_vendor          as integer format ">>>,>>>,>>9" label "Planilha Vendor" column-label "Planilha Vendor"
    field ttv_cod_cond_pagto_vendor        as character format "x(3)" initial "0" label "Condi‡Æo Pagto" column-label "Condi‡Æo Pagto"
    field ttv_val_cotac_tax_vendor_clien   as decimal format ">>9.9999999999" decimals 10 label "Taxa Vendor Cliente" column-label "Taxa Vendor Cliente"
    field ttv_dat_base_fechto_vendor       as date format "99/99/9999" label "Data Base" column-label "Data Base"
    field ttv_qti_dias_carenc_fechto       as Integer format "->>9" label "Dias Carˆncia" column-label "Dias Carˆncia"
    field ttv_log_assume_tax_bco           as logical format "Sim/NÆo" initial no label "Assume Taxa Banco" column-label "Assume Taxa Banco"
    field ttv_log_vendor                   as logical format "Sim/NÆo" initial no
    field ttv_cod_estab_portad             as character format "x(8)"
    field tta_cod_proces_export            as character format "x(12)" label "Processo Exporta‡Æo" column-label "Processo Exporta‡Æo"
    field ttv_val_cr_pis                   as decimal format ">>>,>>>,>>9.99" decimals 2 label "Valor Cred PIS/PASEP" column-label "Vl Cred PIS/PASEP"
    field ttv_val_cr_cofins                as decimal format ">>>,>>>,>>9.99" decimals 2 label "Valor Cr‚dito COFINS" column-label "Credito COFINS"
    field ttv_val_cr_csll                  as decimal format ">>>,>>>,>>9.99" decimals 2 label "Valor Cr‚dito CSLL" column-label "Credito CSLL"
    field tta_cod_indic_econ_desemb        as character format "x(8)" label "Moeda Desembolso" column-label "Moeda Desembolso"
    field tta_val_base_calc_impto          as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Base Calculo Impto" column-label "Base Calculo Impto"
    field tta_log_retenc_impto_impl        as logical format "Sim/NÆo" initial no label "Ret Imposto Impl" column-label "Ret Imposto Impl"
    field ttv_cod_nota_fisc_faturam        as character format "x(16)" label "Nota Fiscal" column-label "Nota Fiscal"
    index tt_id                            is primary unique
          ttv_rec_lote_impl_tit_acr        ascending
          tta_num_seq_refer                ascending
    .


/*Temp-table 11: Gerar  lote de implanta‡Æo.*/

def new shared temp-table tt_integr_acr_lote_impl no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field ttv_cod_empresa_ext              as character format "x(3)" label "C¢digo Empresa Ext" column-label "C¢d Emp Ext"
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_dat_transacao                as date format "99/99/9999" initial today label "Data Transa‡Æo" column-label "Dat Transac"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp‚cie" column-label "Tipo Esp‚cie"
    field tta_ind_orig_tit_acr             as character format "X(8)" initial "ACREMS50" label "Origem Tit Cta Rec" column-label "Origem Tit Cta Rec"
    field tta_val_tot_lote_impl_tit_acr    as decimal format ">>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Total Movimento" column-label "Total Movimento"
    field tta_val_tot_lote_infor_tit_acr   as decimal format ">>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Total Informado" column-label "Total Informado"
    field tta_ind_tip_cobr_acr             as character format "X(10)" initial "Normal" label "Tipo Cobran‡a" column-label "Tipo Cobran‡a"
    field ttv_log_lote_impl_ok             as logical format "Sim/NÆo" initial no
    field tta_log_liquidac_autom           as logical format "Sim/NÆo" initial no label "Liquidac Autom tica" column-label "Liquidac Autom tica"
    index tt_id                            is primary unique
          tta_cod_estab                    ascending
          tta_cod_estab_ext                ascending
          tta_cod_refer                    ascending
    .


/*Temp-table 12: Indicar  os pedidos de venda relacionados ao t¡tulo.*/

def new shared temp-table tt_integr_acr_ped_vda_pend no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_ped_vda                  as character format "x(12)" label "Pedido Venda" column-label "Pedido Venda"
    field tta_cod_ped_vda_repres           as character format "x(12)" label "Pedido Repres" column-label "Pedido Repres"
    field tta_val_perc_particip_ped_vda    as decimal format ">>9.99" decimals 2 initial 0 label "Particip Ped Vda" column-label "Particip"
    field tta_val_origin_ped_vda           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Original Ped Venda" column-label "Original Ped Venda"
    field tta_val_sdo_ped_vda              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Pedido Venda" column-label "Saldo Pedido Venda"
    field tta_des_ped_vda                  as character format "x(40)" label "Pedido Venda" column-label "Pedido Venda"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_ped_vda                  ascending
    .

/*Temp-table 13: Indicar  os relacionamentos com outros t¡tulos, caso o item do lote seja uma Nota de Cr‚dito, Nota de D‚bito ou Aviso de D‚bito.*/

def new shared temp-table tt_integr_acr_relacto_pend no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_estab_tit_acr_pai        as character format "x(5)" label "Estab Tit Pai" column-label "Estab Tit Pai"
    field ttv_cod_estab_tit_acr_pai_ext    as character format "x(5)" label "Estab Tit Pai" column-label "Estab Tit Pai"
    field tta_num_id_tit_acr_pai           as integer format "9999999999" initial 0 label "Token" column-label "Token"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_acr                  as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_relacto_tit_acr          as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Relacto" column-label "Vl Relacto"
    field tta_log_gera_alter_val           as logical format "Sim/NÆo" initial no label "Gera Alter Valor" column-label "Gera Alter Valor"
    field tta_ind_motiv_acerto_val         as character format "X(12)" initial "Altera‡Æo" label "Motivo Acerto Valor" column-label "Motivo Acerto Valor"
    .

/*Temp-table 14: Indicar  os cheques usados para pagamento de AN e T¡tulos Normais … vista.*/

def new shared temp-table tt_integr_acr_relacto_pend_cheq no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_agenc_bcia               as character format "x(10)" label "Agˆncia Banc ria" column-label "Agˆncia Banc ria"
    field tta_cod_cta_corren               as character format "x(10)" label "Conta Corrente" column-label "Cta Corrente"
    field tta_num_cheque                   as integer format ">>>>,>>>,>>9" initial ? label "Num Cheque" column-label "Num Cheque"
    field tta_val_vincul_cheq_acr          as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Vinculado" column-label "Valor Vinculado"
    field tta_cdn_bco_cheq_salario         as Integer format ">>9" initial 0 label "Banco Cheque Sal rio" column-label "Banco Cheque Sal rio"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cod_banco                    ascending
          tta_cod_agenc_bcia               ascending
          tta_cod_cta_corren               ascending
          tta_num_cheque                   ascending
    . 

/*Temp-table 15: Indicar  os representantes para comissäes.*/

def new shared temp-table tt_integr_acr_repres_pend no-undo 
    field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
    field tta_cdn_repres                   as Integer format ">>>,>>9" initial 0 label "Representante" column-label "Representante"
    field tta_val_perc_comis_repres        as decimal format ">>9.99" decimals 4 initial 0 label "% ComissÆo" column-label "% ComissÆo"
    field tta_val_perc_comis_repres_emis   as decimal format ">>9.99" decimals 2 initial 0 label "% Comis EmissÆo" column-label "% Comis EmissÆo"
    field tta_val_perc_comis_abat          as decimal format ">>9.99" decimals 2 initial 0 label "% Comis Abatimento" column-label "% Comis Abatimento"
    field tta_val_perc_comis_desc          as decimal format ">>9.99" decimals 2 initial 0 label "% Comis Desconto" column-label "% Comis Desconto"
    field tta_val_perc_comis_juros         as decimal format ">>9.99" decimals 2 initial 0 label "% Comis Juros" column-label "% Comis Juros"
    field tta_val_perc_comis_multa         as decimal format ">>9.99" decimals 2 initial 0 label "% Comis Multa" column-label "% Comis Multa"
    field tta_val_perc_comis_acerto_val    as decimal format ">>9.99" decimals 2 initial 0 label "% Comis AVA" column-label "% Comis AVA"
    field tta_log_comis_repres_proporc     as logical format "Sim/NÆo" initial no label "Comis Proporcional" column-label "Comis Propor"
    field tta_ind_tip_comis                as character format "X(15)" initial "Valor Bruto" label "Tipo ComissÆo" column-label "Tipo ComissÆo"
    index tt_id                            is primary unique
          ttv_rec_item_lote_impl_tit_acr   ascending
          tta_cdn_repres                   ascending
    . 

/*Temp-table 16: Retorna os erros ocorridos na atualiza‡Æo para cada item do lote de implanta‡Æo.*/

def new shared temp-table tt_log_erros_atualiz no-undo 
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab" 
    field tta_cod_refer                    as character format "x(10)" label "Refer¼ncia" column-label "Refer¼ncia" 
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequ¼ncia" column-label "Seq" 
    field ttv_num_mensagem                 as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero Mensagem" 
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia" 
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda" 
    field ttv_ind_tip_relacto              as character format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac" 
    field ttv_num_relacto                  as integer format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento" 
    .

/*Temp-table 17: */

def temp-table tt_integr_acr_repres_comis_2 no-undo
     field ttv_rec_item_lote_impl_tit_acr   as recid format ">>>>>>9"
     field tta_cdn_repres                   as Integer format ">>>,>>9" initial 0 label "Representante" column-label "Representante"
     field tta_ind_tip_comis_ext            as character format "X(15)" initial "Nenhum" label "Tipo Comis Externo" column-label "Tipo Comis Externo"
     field ttv_ind_liber_pagto_comis        as character format "X(20)" initial "Nenhum" label "Lib Pagto Comis" column-label "Lib Comis"
     field ttv_ind_sit_comis_ext            as character format "X(10)" initial "Nenhum" label "Sit Comis Ext" column-label "Sit Comis Ext".


/*Temp-table 18: */

def temp-table tt_integr_acr_aprop_relacto_2 no-undo
    field ttv_rec_relacto_pend_tit_acr     as recid format ">>>>>>9" initial ?
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    .

Def new shared temp-table tt_params_generic_api no-undo
    field ttv_rec_id                       as recid format ">>>>>>9"
    field ttv_cod_tabela                   as character format "x(28)" label "Tabela" column-label "Tabela"
    field ttv_cod_campo                    as character format "x(25)" label "Campo" column-label "Campo"
    field ttv_cod_valor                    as character format "x(8)" label "Valor" column-label "Valor"
    index tt_idx_param_generic             is primary unique
          ttv_cod_tabela                   ascending
          ttv_rec_id                       ascending
          ttv_cod_campo                    ascending
    .

/*

Exemplo Cria‡Æo temp-table tt_params_generic_api para campo SAFRA:
create tt_params_generic_api.
assign tt_params_generic_api.ttv_rec_id     = (Campo Recid da temp-table) tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr
       tt_params_generic_api.ttv_cod_campo  = 'Safra':U
       tt_params_generic_api.ttv_cod_tabela = ' tt_integr_acr_item_lote_impl':U
       tt_params_generic_api.ttv_cod_valor  = (Valor Da Safra).

Exemplo Cria‡Æo temp-table tt_params_generic_api para campo Devolve Imposto Retido?:
    find b_tit_acr no-lock 
       where b_tit_acr.cod_estab       = tt_integr_acr_abat_antecip.tta_cod_estab    
         and b_tit_acr.cod_espec_docto = tt_integr_acr_abat_antecip.tta_cod_espec_doc
         and b_tit_acr.cod_ser_docto   = tt_integr_acr_abat_antecip.tta_cod_ser_docto
         and b_tit_acr.cod_tit_acr     = tt_integr_acr_abat_antecip.tta_cod_tit_acr  
         and b_tit_acr.cod_parcela     = tt_integr_acr_abat_antecip.tta_cod_parcela no-error.

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Devolve Imposto Retido?':U                              
       tt_params_generic_api.ttv_cod_valor  = string(yes).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Valor Estornado':U                              
       tt_params_generic_api.ttv_cod_valor  = string(7.92).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Gera Reten‡Æo Imposto Antecipa‡Æo':U                              
       tt_params_generic_api.ttv_cod_valor  = string(yes).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = (Campo Recid da temp-table)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_item_lote_impl':U
       tt_params_generic_api.ttv_cod_campo  = '% Antecip Desconto':U                              
       tt_params_generic_api.ttv_cod_valor  = string(2.5).
*/

def new shared temp-table tt_integr_acr_relacto_pend_aux no-undo
    field ttv_rec_lote_impl_tit_acr        as recid format ">>>>>>9" initial ?
    field ttv_log_nota_vincul              as logical format "Sim/N’o" initial yes
    .

/*
    View Gera‡Æo de T¡tulos: MV_TITULO_ACR
    - Dados T¡tulo:
    
    Atributo	                Descricao	        Tipo	        Obr	Observa‡Æo Dados T¡tulo
    cod_empresa	                Empresa	            VARCHAR(03)	    Sim	 
    cod_estab	                Estabelecimento	    VARCHAR(03)	    Sim	 
    dat_transacao               Data transa‡Æo	    DATE	        Sim	Data da contabiliza‡Æo do movimento
    ind_orig_tit_acr	        Origem T¡tulo	    VARCHAR(08)	    Sim	Fixo "SIAF"
    cdn_cliente	                Cliente	Number	                    Sim	 
    cod_espec_docto	            Esp‚cie Documento	VARCHAR(03)	    Sim	Fixo "MC"
    cod_ser_docto	            S‚rie Documento	    VARCHAR(03)	    Sim	 
    cod_tit_acr	                T­tulo	            VARCHAR(10	    Sim	 
    cod_parcela	                Parcela	            VARCHAR(02)	    Sim	 
    cod_indic_econ	            Moeda	            VARCHAR(08)	    Sim	 Fixo "MC"
    cod_portador	            Portador	        VARCHAR(05)	    NÆo	 
    cod_cart_bcia	            Carteira banc ria	VARCHAR(03)	    NÆo	 
    cod_histor_padr	            Hist¢rico PadrÆo	VARCHAR(08)	    NÆo	 
    cdn_repres	                Representante	    Number	        Sim	Fixo "999"
    dat_vencto_tit_acr	        Vencimento	        DATE	        Sim	 
    dat_prev_liquidac	        Prev Liquida‡Æo	    DATE	        Sim	 
    dat_emis_docto	            Data EmissÆo	    DATE	        Sim	 
    val_tit_acr	                Valor	            DATE	        Sim	 
    val_perc_juros_dia_atraso	Perc Jur Dia Atraso	Number	        NÆo	 
    val_perc_multa_atraso	    Perc Multa Atraso	Number	        NÆo	 
    des_text_histor	            Hist¢rico	        VARCHAR(2000)	NÆo	 
    val_liq_tit_acr	            Vl L¡quido	        Number	        Sim	 
    ind_tip_espec_docto	        Tipo Esp‚cie	    VARCHAR(17)	    Sim	Fixo "Normal" 
    
    - Dados Liquida‡Æo:
    
    log_liquidac_autom	        Liquidac Autom tica	Number	Sim	0 - NÆo  1 - Sim    Se cheque enviar "1", se cartÆo enviar "0"
    cod_admdra_cartao_cr	    Administradora	VARCHAR(5)	Sim	Somente para cartäes de cr‚dito
    cod_cartcred	            C¢digo CartÆo	VARCHAR(20)	Sim	 Somente para cartäes de cr‚dito
    cod_mes_ano_valid_cartao	Validade CartÆo	VARCHAR(07)	Sim	Formato  XX/XXXX Somente para cartäes de cr‚dito
    cod_autoriz_cartao_cr	    C¢d Pr‚-Autoriza‡Æo	VARCHAR(06)	Sim	Somente para cartäes de cr‚dito 
    dat_compra_cartao_cr	    Data Efetiva Venda	DATE	Sim	Somente para cartäes de cr‚dito 
    cod_banco	                Banco	VARCHAR(08)	Sim	 Somente para cheques
    cod_agenc_bcia	            Agˆncia Banc ria	VARCHAR(10)	Sim	Enviar conforme m scara EMS Somente para cheques
    cod_cta_corren	            Conta Corrente	VARCHAR(10)	Sim	 Enviar conforme m scara EMS Somente para cheques
    num_cheque	                Num Cheque	Number	Sim	 Somente para cheques
    dat_emis_cheq	            Data EmissÆo	Date	Sim	 Somente para cheques
    dat_depos_cheq_acr	        Dep¢sito	Date	Sim	 Somente para cheques
    dat_prev_depos_cheq_acr	    PrevisÆo Dep¢sito	Date	Sim	 Somente para cheques
    dat_desc_cheq_acr	        Data Desconto	Date	Sim	 Somente para cheques
    dat_prev_desc_cheq_acr	    Data Prev Desc	Date	Sim	 Somente para cheques
    val_cheque	                Valor Cheque	Number	Sim	 Somente para cheques
    nom_emit	                Nome Emitente	VARCHAR(40)	Sim	Somente no caso de cheque de terceiros
    nom_cidad_emit	            Cidade Emitente	VARCHAR(30)	Sim	Somente no caso de cheque de terceiros
    cod_estab	                Estabelecimento	VARCHAR(03)	Sim	Somente no caso de cheque de terceiros
    cod_id_feder	            ID Federal (CPF/CNPJ)	VARCHAR(20)	Sim	Somente no caso de cheque de terceiros
    cod_indic_econ	            Moeda	VARCHAR(08)	Sim 	Fixo "Real"
    cod_usuar_cheq_acr_terc	    Usu rio	VARCHAR(12)	Sim	Usu rio do EMS que tem permissao de implanta‡Æo de cheques
    log_cheq_terc	            Cheque Terceiro	Number	Sim	0 - NÆo 1 - Sim
    log_cheq_acr_renegoc	    Cheque Reneg	Number	Sim	0 - NÆo 1 - Sim
    num_pessoa	                Pessoa	Number	Sim	Somente no caso de cheque de terceiros
    cod_pais	                Pa¡s	VARCHAR(03)	Sim	Somente no caso de cheque de terceiros
    cod_comprov_vda	            Comprovante Venda	VARCHAR(12)	NÆo	Usar apenas no caso de cartÆo de cr‚dito
    num_parc_cartcred	        Quantidade Parcelas	Number	NÆo	Usar apenas no caso de cartÆo de cr‚dito
    cod_autoriz_bco_emissor	    Codigo Autoriza‡Æo	VARCHAR(03)	NÆo	Usar apenas no caso de cartÆo de cr‚dito
    cod_lote_origin	            Lote Orig Venda	VARCHAR(03)	NÆo	Usar apenas no caso de cartÆo de cr‚dito
    
    View Apropria‡Æo Cont bil: 
    
    cod_estab	            Estabelecimento	    VARCHAR(03)	Sim	 Mesmo dado enviado na view do t¡tulo
    cod_espec_docto	        Esp‚cie Documento	VARCHAR(03)	Sim	 Mesmo dado enviado na view do t¡tulo
    cod_ser_docto	        S‚rie Documento	    VARCHAR(03)	Sim	 Mesmo dado enviado na view do t¡tulo
    cod_tit_acr	            T­tulo	            VARCHAR(10	Sim	 Mesmo dado enviado na view do t¡tulo
    cod_parcela	            Parcela	            VARCHAR(02)	Sim	 Mesmo dado enviado na view do t¡tulo
    cod_plano_cta_ctbl	    Plano Contas        VARCHAR(08)	Sim	
    cod_cta_ctbl	        Conta Cont bil      VARCHAR(20)	Sim	
    cod_unid_negoc	        Unid Neg¢cio        VARCHAR(03)	Sim	
    cod_plano_ccusto	    Plano Centros Custo	VARCHAR(08)	NÆo	
    cod_ccusto	            Centro Custo	    VARCHAR(11)	NÆo	
    cod_tip_fluxo_financ	Tipo Fluxo Financ	VARCHAR(12)	Sim	
    val_aprop_ctbl	        Valor Aprop Ctbl	Number	    Sim	
*/
/* Fim de Include */
