/* ***************************  Definitions  ************************** */

DEFINE VARIABLE jsonRetorno         AS JsonArray            NO-UNDO.
DEFINE VARIABLE jsonRecebido        AS LONGCHAR             NO-UNDO.
DEFINE VARIABLE oRequestParser      AS JsonAPIRequestParser NO-UNDO.
DEFINE VARIABLE oJsonArrayMain      AS JsonArray            NO-UNDO.
DEFINE VARIABLE oJsonObjectMain     AS JsonObject           NO-UNDO.
DEFINE VARIABLE iCountMain          AS INTEGER              NO-UNDO.
DEFINE VARIABLE cNrPedido           AS CHARACTER INITIAL '' NO-UNDO.
DEFINE VARIABLE i-prox-numero       AS INTEGER              NO-UNDO.
