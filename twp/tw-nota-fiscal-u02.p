/*----------------------------------------------------------------------------------------------/
 Programa..: tw-nota-fiscal.p
 Objetivo..: Notificar autorizacao da NF para SHOPIFY
 Data......: 30/08/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/
{include/i-license-manager.i tw-nota-fiscal  MFT}
{utp\ut-glob.i}

DEF PARAMETER BUFFER p-table FOR nota-fiscal.
DEF PARAMETER BUFFER p-old-table FOR nota-fiscal.


IF p-old-table.idi-sit-nf-eletro <> 3 AND p-table.idi-sit-nf-eletro = 3  THEN DO:

    
    run twp\tw-nota-fiscal-u02-spf.p  (BUFFER p-table).
    run twp\tw-nota-fiscal-u02-mail.p (BUFFER p-table).

END.


