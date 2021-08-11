&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          movmac           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esspf004-V01 2.09.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-api-param-spf
&Scoped-define FIRST-EXTERNAL-TABLE es-api-param-spf


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-api-param-spf.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-api-param-spf.des-tipo-integr ~
es-api-param-spf.ativo es-api-param-spf.ind-tipo-trans es-api-param-spf.tip-integracao ~
es-api-param-spf.cd-sistema es-api-param-spf.host-integr es-api-param-spf.porta-integr ~
es-api-param-spf.path-integr es-api-param-spf.dir-export es-api-param-spf.dir-env ~
es-api-param-spf.programa-integr es-api-param-spf.val-token 
&Scoped-define ENABLED-TABLES es-api-param-spf
&Scoped-define FIRST-ENABLED-TABLE es-api-param-spf
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-api-param-spf.cd-tipo-integr ~
es-api-param-spf.des-tipo-integr es-api-param-spf.ativo es-api-param-spf.ind-tipo-trans ~
es-api-param-spf.tip-integracao es-api-param-spf.cd-sistema ~
es-api-param-spf.host-integr es-api-param-spf.porta-integr es-api-param-spf.path-integr ~
es-api-param-spf.dir-export es-api-param-spf.dir-env es-api-param-spf.programa-integr ~
es-api-param-spf.val-token 
&Scoped-define DISPLAYED-TABLES es-api-param-spf
&Scoped-define FIRST-DISPLAYED-TABLE es-api-param-spf
&Scoped-Define DISPLAYED-OBJECTS c-des-app 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-api-param-spf.cd-tipo-integr 
&Scoped-define ADM-ASSIGN-FIELDS es-api-param-spf.cd-tipo-integr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cd-tipo-integr|y|y|movmac.es-api-param-spf.cd-tipo-integr
cd-sistema||y|mgcam.es-api-param-spf.cd-sistema
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cd-tipo-integr",
     Keys-Supplied = "cd-tipo-integr,cd-sistema"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-des-app AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 76.43 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 18.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-api-param-spf.cd-tipo-integr AT ROW 1.13 COL 14.57 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11.43 BY 1
     es-api-param-spf.des-tipo-integr AT ROW 2.71 COL 14.72 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 41.14 BY 1
     es-api-param-spf.ativo AT ROW 2.83 COL 76 WIDGET-ID 22
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es-api-param-spf.ind-tipo-trans AT ROW 3.71 COL 16.72 NO-LABEL WIDGET-ID 24
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Importaá∆o", 1,
"Exportaá∆o", 2
          SIZE 24 BY 1
     es-api-param-spf.tip-integracao AT ROW 3.75 COL 76 NO-LABEL WIDGET-ID 48
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Fila", 1,
"Cont°nua", 2
          SIZE 29 BY .75
     es-api-param-spf.cd-sistema AT ROW 4.71 COL 14.72 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 11.43 BY 1
     c-des-app AT ROW 4.71 COL 26.57 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     es-api-param-spf.host-integr AT ROW 5.71 COL 12.72 WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 70.57 BY 1
     es-api-param-spf.porta-integr AT ROW 5.75 COL 91.57 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 11.43 BY 1
     es-api-param-spf.path-integr AT ROW 6.75 COL 16.72 NO-LABEL WIDGET-ID 32
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 88 BY 4
     es-api-param-spf.dir-export AT ROW 10.75 COL 16.72 NO-LABEL WIDGET-ID 36
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 88 BY 4
     es-api-param-spf.dir-env AT ROW 14.75 COL 16.86 NO-LABEL WIDGET-ID 40
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 88 BY 4
     es-api-param-spf.programa-integr AT ROW 18.96 COL 1.86 WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 35.57 BY .88
     es-api-param-spf.val-token AT ROW 20 COL 15 COLON-ALIGNED WIDGET-ID 46 FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 87 BY .79
     "Diret¢rio Log.:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 15 COL 6.86 WIDGET-ID 42
     "Path:" VIEW-AS TEXT
          SIZE 4 BY .75 AT ROW 7 COL 12 WIDGET-ID 34
     "Diret¢rio Exportaá∆o:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 11 COL 2 WIDGET-ID 38
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcam.es-api-param-spf
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 20.25
         WIDTH              = 106.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-des-app IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       c-des-app:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN es-api-param-spf.cd-tipo-integr IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN es-api-param-spf.host-integr IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-api-param-spf.programa-integr IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-api-param-spf.val-token IN FRAME f-main
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-api-param-spf.cd-sistema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-spf.cd-sistema V-table-Win
ON F5 OF es-api-param-spf.cd-sistema IN FRAME f-main /* C¢digo Sistema */
DO:
   {include/zoomvar.i 
        &prog-zoom="esp/es0301-z01.w"
        &campo=es-api-param-spf.cd-sistema
        &campozoom=cd-sistema
        &campo1=c-des-app
        &campozoom1=des-sistema}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-spf.cd-sistema V-table-Win
ON LEAVE OF es-api-param-spf.cd-sistema IN FRAME f-main /* C¢digo Sistema */
DO:
   RUN pi-mostrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-spf.cd-sistema V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-spf.cd-sistema IN FRAME f-main /* C¢digo Sistema */
DO:
   APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

es-api-param-spf.cd-sistema:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cd-tipo-integr':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es-api-param-spf
           &WHERE = "WHERE es-api-param-spf.cd-tipo-integr eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-api-param-spf"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-api-param-spf"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    {include/i-valid.i}
    
    IF adm-new-record 
    THEN DO:
       FIND FIRST es-api-param-spf NO-LOCK
            WHERE es-api-param-spf.cd-tipo-integr = es-api-param-spf.cd-tipo-integr:INPUT-VALUE IN FRAME {&FRAME-NAME} 
            NO-ERROR.
       IF AVAIL es-api-param-spf
       THEN DO:
          APPLY "ENTRY" TO es-api-param-spf.cd-tipo-integr IN FRAME {&FRAME-NAME}.
          RUN utp/ut-msgs.p ("show",17006,"C¢digo j† cadastrado.").
          RETURN 'ADM-ERROR':U.
       END.
    END.
    
    IF es-api-param-spf.cd-tipo-integr:INPUT-VALUE IN FRAME {&FRAME-NAME} = 0
    THEN DO:
       APPLY "ENTRY" TO es-api-param-spf.cd-tipo-integr IN FRAME {&FRAME-NAME}.
       RUN utp/ut-msgs.p ("show",17006,"C¢digo n∆o preenchido.").
       RETURN 'ADM-ERROR':U.
    END.

    IF es-api-param-spf.des-tipo-integr:INPUT-VALUE IN FRAME {&FRAME-NAME} = ""
    THEN DO:
       APPLY "ENTRY" TO es-api-param-spf.des-tipo-integr IN FRAME {&FRAME-NAME}.
       RUN utp/ut-msgs.p ("show",17006,"Descriá∆o n∆o preenchida.").
       RETURN 'ADM-ERROR':U.
    END.
    

    FIND FIRST es-api-app-spf NO-LOCK
         WHERE es-api-app-spf.cd-sistema = es-api-param-spf.cd-sistema:INPUT-VALUE IN FRAME {&FRAME-NAME}
         NO-ERROR.
    IF NOT AVAIL es-api-app-spf
    THEN DO:
       APPLY "ENTRY" TO es-api-param-spf.cd-sistema IN FRAME {&FRAME-NAME}.
       RUN utp/ut-msgs.p ("show",17006,"Sistema n∆o cadastrado.").
       RETURN 'ADM-ERROR':U.
    END.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U 
    then return 'ADM-ERROR':U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-mostrar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostrar V-table-Win 
PROCEDURE pi-mostrar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
      c-des-app:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   FIND FIRST es-api-app-spf NO-LOCK
        WHERE es-api-app-spf.cd-sistema = es-api-param-spf.cd-sistema:INPUT-VALUE IN FRAME {&FRAME-NAME}
        NO-ERROR.
   IF AVAIL es-api-app-spf
   THEN ASSIGN
      c-des-app:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-api-app-spf.des-sistema.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cd-tipo-integr" "es-api-param-spf" "cd-tipo-integr"}
  {src/adm/template/sndkycas.i "cd-sistema" "es-api-param-spf" "cd-sistema"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-api-param-spf"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

