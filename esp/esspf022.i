DEFINE TEMP-TABLE ttPedido NO-UNDO SERIALIZE-NAME "Order"
   FIELD cnpjEmitente  AS CHARACTER
   FIELD pedidoCliente AS CHARACTER
   FIELD pedidoShopify AS CHARACTER
   FIELD dataPagamento AS CHARACTER.

DEFINE TEMP-TABLE ttItensPedido NO-UNDO SERIALIZE-NAME "ItemOrderList"
   FIELD nrSeqPed   AS INTEGER
   FIELD codigoItem AS CHARACTER
   FIELD qtdPedida  AS INTEGER
   FIELD precoUnit  AS DECIMAL.


