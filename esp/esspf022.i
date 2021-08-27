DEFINE TEMP-TABLE ttPedido NO-UNDO SERIALIZE-NAME "Order"
   FIELD cnpjEmitente  AS CHARACTER
   FIELD pedidoCliente AS CHARACTER.

DEFINE TEMP-TABLE ttItensPedido NO-UNDO SERIALIZE-NAME "ItemOrderList"
   FIELD nrSeqPed   AS INTEGER
   FIELD codigoItem AS CHARACTER
   FIELD qtdPedida  AS INTEGER
   FIELD precoUnit  AS DECIMAL.


DEFINE TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda
   FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
   FIELD r-rowid AS ROWID.