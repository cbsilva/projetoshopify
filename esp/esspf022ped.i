DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
  FIELD r-rowid AS ROWID.  

DEF TEMP-TABLE tt-ped-item LIKE ped-item
    FIELD r-rowid AS ROWID.  

DEF TEMP-TABLE tt-ped-param NO-UNDO
    FIELD relacao-item-cli          AS log INIT NO      
    FIELD tp-relacao-item-cli   AS int INIT 1   
    FIELD qtde-un-medida-cli    AS log  INIT NO 
    FIELD Multiplicar-qtde          AS log INIT NO      
    FIELD atribuir-preco-comp   AS log INIT NO  
    FIELD tp-exp-nat-oper           AS int INIT 1
    FIELD tp-exp-dt-entrega         AS Int INIT 1
    FIELD exp-nat-cons-final    AS log INIT no
    FIELD exp-nat-cod-mensagem  AS log INIT no  
    FIELD Atualizar-entregas    AS Log INIT YES
    FIELD Arredondar-qtde-lote  AS Log INIT no  
    FIELD gerar-proc-exp            AS Log INIT no      
    FIELD Itinerario            AS INT INIT 0.

/* variaveis de programa */
def var hBOdi154    as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hBOdi157    as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hBOdi159    as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hBOdi159sdf as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hBOdi154sdf as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hBOdi159com as handle no-undo.    /* Define a vari˜vel de handle para BO. */
def var hShowMsg    as handle no-undo.
def var h-acomp     as handle no-undo.    /* Define a vari˜vel de handle para BO. */

