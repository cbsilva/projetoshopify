{utp/ut-glob.i}
{esp/esspf022.i}
{esp/esspf022ped.i}


/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM TABLE FOR ttPedido.


for each ttPedido no-lock,
    first emitente no-lock
    where emitente.cgc = ttPedido.cnpjEmitente,
    FIRST ped-venda 
    where ped-venda.nome-abrev = emitente.nome-abrev
      and ped-venda.nr-pedcli  = 'DG' + ttPedido.pedidoCliente no-lock.

    if  not valid-handle(hbodi159com) or
        hbodi159com:type <> "PROCEDURE":U or
        hbodi159com:file-name <> "dibo/bodi159com.p":U then do:           
        run dibo/bodi159com.p persistent set hbodi159com.
    end.
    
        
    run setUserLog in hbodi159com (input c-seg-usuario).       
    run calculateOrder in hbodi159com (INPUT ROWID(ped-venda)).
    
    
    /*elimina todos os handles usado na bodi159*/
    if  valid-handle(hbodi159com) then do:
        /*run destroyBO in hbodi159com.*/
        RUN destroy IN hbodi159com.
        ASSIGN hbodi159com = ?.
    end. 
    
    
end.
