{utp/ut-glob.i}

DEFINE TEMP-TABLE tt_epc NO-UNDO
    FIELD cod_event        AS CHARACTER    
    FIELD cod_PARAMETER    AS CHARACTER    
    FIELD val_PARAMETER    AS CHARACTER
    INDEX id IS PRIMARY cod_PARAMETER cod_event ASCENDING.

DEFINE INPUT        PARAMETER p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_epc.

FOR EACH tt_epc NO-LOCK:

   FIND FIRST item_lote_impl_tit_acr
       WHERE ROWID(item_lote_impl_tit_acr ) = TO-ROWID( tt_epc.val_PARAMETER ) exclusive-lock NO-ERROR.
       
   IF AVAIL item_lote_impl_tit_acr THEN DO:       
       
      FIND FIRST trad_org_ext
           WHERE trad_org_ext.cod_matriz_trad_org_ext = 'ATIVO'
             AND trad_org_ext.cod_tip_unid_organ      = '999'
             and trad_org_ext.cod_unid_organ          = item_lote_impl_tit_acr.cod_estab NO-LOCK NO-ERROR.
       
      FIND FIRST nota-fiscal 
           WHERE nota-fiscal.cod-estabel = trad_org_ext.cod_unid_organ_ext
             AND nota-fiscal.serie       = item_lote_impl_tit_acr.cod_ser_docto
             AND nota-fiscal.nr-nota-fis = item_lote_impl_tit_acr.cod_tit_acr NO-LOCK NO-ERROR.
            
      IF AVAILABLE nota-fiscal THEN DO:
      
            FIND FIRST es-spf-ped-venda
                 where es-spf-ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                   and es-spf-ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli no-lock no-error.
            
            IF AVAIL es-spf-ped-venda THEN DO:
            
                FIND FIRST es-api-param-acr 
                     WHERE es-api-param-acr.cod_empresa = i-ep-codigo-usuario
                       AND es-api-param-acr.cod_estab   = item_lote_impl_tit_acr.cod_estab no-lock no-error.

                FIND FIRST lote_impl_tit_acr 
                        OF item_lote_impl_tit_acr EXCLUSIVE-LOCK NO-ERROR.                       

                IF AVAIL lote_impl_tit_acr THEN DO:
                   
                    IF AVAIL es-spf-ped-venda THEN
                        lote_impl_tit_acr.dat_transacao  = es-spf-ped-venda.dt-pagamento.
                 
                    RELEASE lote_impl_tit_acr.
                 
                END.                                       
                        
                item_lote_impl_tit_acr.dat_emis_docto = item_lote_impl_tit_acr.dat_emis_docto.
                

                IF AVAIL es-api-param-acr THEN
                    ASSIGN item_lote_impl_tit_acr.cod_cart_bcia   = es-api-param-acr.cod_cart_bcia
                           item_lote_impl_tit_acr.cod_espec_docto = es-api-param-acr.cod_espec_docto
                           item_lote_impl_tit_acr.cod_portador    = es-api-param-acr.COD_PORTADOR
                           item_lote_impl_tit_acr.cod_ser_docto   = es-api-param-acr.cod_ser_docto.
                           
    //                           item_lote_impl_tit_acr.cod_tit_acr     = 
                           
                               
            END.                   


      END.
      
      
           RELEASE item_lote_impl_tit_acr.
      
   end.
   
   
   
end.

