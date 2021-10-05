{utp/ut-glob.i}

def parameter buffer b-nota for nota-fiscal.

//message 'inicio' view-as alert-box.

CREATE  es-api-import-spf.                                            
ASSIGN  es-api-import-spf.id-movto          = NEXT-VALUE(seq_import)
        es-api-import-spf.cd-tipo-integr    = 23 /*-- Faturamento --*/   
        es-api-import-spf.chave             = b-nota.cod-estabel + '|' + b-nota.serie + '|' + b-nota.nr-nota-fis 
        es-api-import-spf.data-movto        = NOW                     
        es-api-import-spf.data-inicio       = NOW                     
        es-api-import-spf.data-fim          = ?                       
        es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/  
        es-api-import-spf.cod-status        = 0. /*--- sem status ---*/
        
//        es-api-import-spf.c-json            = jsonRecebido.   


RETURN "OK".

