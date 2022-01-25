/*----------------------------------------------------------------------------------------------/
 Programa..: tw-ret-nf-eletro.p
 Objetivo..: Notificar autorizacao da NF para SHOPIFY
 Data......: 20/10/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/

{include/i-license-manager.i tw-ret-nf-eletro  MFT}
{utp/ut-glob.i}

DEF PARAMETER BUFFER p-table     FOR ret-nf-eletro.
DEF PARAMETER BUFFER p-old-table FOR ret-nf-eletro.

IF p-table.cod-msg = '51758' THEN DO:  
    
    
    find first nota-fiscal 
         where nota-fiscal.cod-estabel = p-table.cod-estabel
           and nota-fiscal.serie       = p-table.cod-serie 
           and nota-fiscal.nr-nota-fis = p-table.nr-nota-fis no-lock no-error.
           
    if avail nota-fiscal then do:           
    
    
        run twp\tw-nota-fiscal-u02.p  (BUFFER nota-fiscal).
        run twp\tw-nota-fiscal-u02-mail.p (BUFFER nota-fiscal).
    
    end.
    
END.

return "ok".
