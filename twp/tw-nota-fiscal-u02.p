{utp/ut-glob.i}

def parameter buffer b-nota for nota-fiscal.

//message 'inicio' view-as alert-box.

    find first es-spf-ped-venda
         where es-spf-ped-venda.nome-abrev = b-nota.nome-ab-cli
           and es-spf-ped-venda.nr-pedcli  = b-nota.nr-pedcli no-lock no-error.
       
       
    if avail es-spf-ped-venda then do:

        CREATE  es-api-export.                                            
        ASSIGN  es-api-export.id-movto          = NEXT-VALUE(seq_import)
                es-api-export.cd-tipo-integr    = 24 /*-- Faturamento --*/   
                es-api-export.chave             = es-spf-ped-venda.nr-shopify
                es-api-export.data-movto        = NOW                     
                es-api-export.data-inicio       = NOW                     
                es-api-export.data-fim          = ?                       
                es-api-export.ind-situacao      = 0 /*--- Pendente ---*/  
                es-api-export.cod-status        = 0. /*--- sem status ---*/
                
        //        es-api-export.c-json            = jsonRecebido.   
    
    end.
RETURN "OK".

