;;; CSVLABEL.lsp  v0.4
;;; BricsCAD AutoLISP: CSV-linked framed MTEXT with OpenDCL palette.
;;; CSV and labels.ini are selected independently and may live away from this .lsp.
;;; Commands: CLABELSOURCE, CLABELTEMPLATES, CLABEL, CLABELSET,
;;;           CLABELSYNC, CLABELFORMAT.
;;; Created labels store (stable record ID, template name) as XData.
;;; Source paths and default template/text size are saved in the DWG.

(vl-load-com)

(setq cl:*app* "CSVLABEL_V1")       ; Reuse v0.1 metadata on existing labels.
(setq cl:*config-key* "CSVLABEL_CONFIG_V1")
(setq cl:*default-name* "detail")

(defun cl:trim (s)
  (vl-string-trim " \t\r\n" s))

(defun cl:strip-bom (s / first)
  ;; Remove UTF-8 BOM if a CSV editor saved one.
  (if (> (strlen s) 0)
    (progn
      (setq first (ascii (substr s 1 1)))
      (cond
        ((= first 65279) (substr s 2))
        ((and (>= (strlen s) 3) (= first 239)
              (= (ascii (substr s 2 1)) 187)
              (= (ascii (substr s 3 1)) 191))
         (substr s 4))
        (T s)))
    s))

(defun cl:csv-line (s / i n c out part quoted)
  ;; Supports quoted commas and doubled quote escapes.
  ;; One CSV record must fit on one physical line.
  (setq i 1 n (strlen s) out nil part "" quoted nil)
  (while (<= i n)
    (setq c (substr s i 1))
    (cond
      ((= c "\"")
       (if (and quoted (< i n) (= (substr s (1+ i) 1) "\""))
         (progn (setq part (strcat part "\"")) (setq i (1+ i)))
         (setq quoted (not quoted))))
      ((and (= c ",") (not quoted))
       (setq out (cons part out) part ""))
      (T (setq part (strcat part c))))
    (setq i (1+ i)))
  (if quoted nil (reverse (cons part out))))

(defun cl:duplicates-p (items / found)
  (setq found nil)
  (while (and items (not found))
    (if (member (car items) (cdr items)) (setq found T))
    (setq items (cdr items)))
  found)

(defun cl:zip-row (headers cells / result)
  (setq result nil)
  (while headers
    (setq result (cons (cons (car headers) (car cells)) result))
    (setq headers (cdr headers) cells (cdr cells)))
  (reverse result))

(defun cl:read-csv (path / fp line headers fields rows row id line-no problem)
  ;; Return rows indexed by uppercase ID, or nil on any error.
  (setq fp (open path "r"))
  (if (not fp)
    (progn (princ (strcat "\nCSVLABEL: Cannot open CSV: " path)) nil)
    (progn
      (setq line (read-line fp) rows nil line-no 1 problem nil)
      (if (null line)
        (setq problem "Empty CSV")
        (progn
          (setq headers (cl:csv-line (cl:strip-bom line)))
          (if (not headers)
            (setq problem "Invalid CSV header")
            (progn
              (setq headers (mapcar '(lambda (x) (strcase (cl:trim x))) headers))
              (cond
                ((not (member "ID" headers)) (setq problem "Missing 'id' column"))
                ((member "" headers) (setq problem "Empty column name"))
                ((cl:duplicates-p headers) (setq problem "Duplicate column names")))))))
      (while (and (not problem) (setq line (read-line fp)))
        (setq line-no (1+ line-no))
        (if (/= (cl:trim line) "")
          (progn
            (setq fields (cl:csv-line line))
            (cond
              ((not fields) (setq problem (strcat "Bad quotes on line " (itoa line-no))))
              ((/= (length fields) (length headers))
               (setq problem (strcat "Column count mismatch on line " (itoa line-no))))
              (T
               (setq row (cl:zip-row headers fields)
                     id (cl:trim (cdr (assoc "ID" row))))
               (cond
                 ((= id "") (setq problem (strcat "Blank id on line " (itoa line-no))))
                 ((assoc (strcase id) rows) (setq problem (strcat "Duplicate id: " id)))
                 (T (setq rows (cons (cons (strcase id) row) rows)))))))))
      (close fp)
      (cond
        (problem (princ (strcat "\nCSVLABEL: " problem ".")) nil)
        ((null rows) (princ "\nCSVLABEL: CSV has no records.") nil)
        (T (reverse rows))))))

(defun cl:read-templates (path / fp line clean section name eq key val entries problem n)
  ;; labels.ini: [name] followed by one template=... entry. \\n means a newline.
  ;; Returns (("detail" . "{id}: {Description}...") ...), or nil.
  (setq fp (open path "r"))
  (if (not fp)
    (progn (princ (strcat "\nCSVLABEL: Cannot open templates: " path)) nil)
    (progn
      (setq entries nil section nil problem nil n 0)
      (while (setq line (read-line fp))
        (setq n (1+ n) clean (cl:trim line))
        (cond
          ((or (= clean "") (= (substr clean 1 1) ";")
               (= (substr clean 1 1) "#")) nil)
          ((and (= (substr clean 1 1) "[")
                (= (substr clean (strlen clean) 1) "]"))
           (setq name (strcase (cl:trim (substr clean 2 (- (strlen clean) 2))) T))
           (cond
             ((= name "") (setq problem (strcat "Empty template name at line " (itoa n)))
              (setq section nil))
             ((assoc name entries) (setq problem (strcat "Repeated template section: " name))
              (setq section nil))
             (T (setq section name entries (cons (cons name nil) entries)))))
          ((setq eq (vl-string-search "=" clean))
           (setq key (strcase (cl:trim (substr clean 1 eq)))
                 val (cl:trim (substr clean (+ eq 2))))
           (cond
             ((not section) (setq problem (strcat "Entry outside a [section] at line " (itoa n))))
             ((= key "TEMPLATE")
              (if (cdr (assoc section entries))
                (setq problem (strcat "Duplicate template= in [" section "]"))
                (setq entries (subst (cons section val) (assoc section entries) entries)))
             ;; Unknown options are ignored for future compatibility.
             )))
          (T (setq problem (strcat "Invalid template file line " (itoa n))))))
      (close fp)
      (if (not problem)
        (foreach entry entries
          (if (or (null (cdr entry)) (= (cdr entry) ""))
            (setq problem (strcat "Missing template= in [" (car entry) "]")))))
      (cond
        (problem (princ (strcat "\nCSVLABEL: " problem ".")) nil)
        ((null entries) (princ "\nCSVLABEL: No templates found.") nil)
        (T (reverse entries))))))

(defun cl:path-absolute-p (p)
  (or (vl-string-search ":" p)
      (= (substr p 1 1) "/")
      (= (substr p 1 1) "\\")))

(defun cl:trim-trailing-slash (p)
  (while (and (> (strlen p) 1)
              (member (substr p (strlen p) 1) '("/" "\\")))
    (setq p (substr p 1 (1- (strlen p)))))
  p)

(defun cl:store-path (path / dir dwg-dir)
  ;; A CSV beside the saved DWG is stored by filename for portability.
  (setq dir (vl-filename-directory path)
        dwg-dir (getvar "DWGPREFIX"))
  (if (and dir dwg-dir (/= dwg-dir "")
           (= (strcase (cl:trim-trailing-slash dir))
              (strcase (cl:trim-trailing-slash dwg-dir))))
    (strcat (vl-filename-base path) (vl-filename-extension path))
    path))

(defun cl:resolve-path (stored)
  (if (cl:path-absolute-p stored)
    stored
    (strcat (getvar "DWGPREFIX") stored)))

(defun cl:ini-path (csv-path)
  (strcat (vl-filename-directory csv-path) "/labels.ini"))

(defun cl:get-config (/ rec)
  ;; XRecord: CSV path, last-used template NAME, text height, wrap width,
  ;; separate template path (group 3, introduced in v0.3).
  ;; Empty template path retains v0.2's "labels.ini beside CSV" behavior.
  (setq rec (dictsearch (namedobjdict) cl:*config-key*))
  (if rec
    (list
      (if (assoc 1 rec) (cdr (assoc 1 rec)) "")
      (if (assoc 2 rec) (cdr (assoc 2 rec)) cl:*default-name*)
      (if (assoc 40 rec) (cdr (assoc 40 rec)) 2.5)
      (if (assoc 41 rec) (cdr (assoc 41 rec)) 0.0)
      (if (assoc 3 rec) (cdr (assoc 3 rec)) ""))
    (list "" cl:*default-name* 2.5 0.0 "")))

(defun cl:replace-group (ed code value / current)
  (setq current (assoc code ed))
  (if current
    (subst (cons code value) current ed)
    (append ed (list (cons code value)))))

(defun cl:put-config (cfg / nod existing ed rec)
  (setq nod (namedobjdict)
        existing (dictsearch nod cl:*config-key*))
  (if existing
    (progn
      (setq ed (entget (cdr (assoc -1 existing))))
      (setq ed (cl:replace-group ed 1 (nth 0 cfg)))
      (setq ed (cl:replace-group ed 2 (nth 1 cfg)))
      (setq ed (cl:replace-group ed 40 (nth 2 cfg)))
      (setq ed (cl:replace-group ed 41 (nth 3 cfg)))
      (setq ed (cl:replace-group ed 3 (nth 4 cfg)))
      (entmod ed))
    (progn
      (setq rec (entmakex
                  (list '(0 . "XRECORD") '(100 . "AcDbXrecord") '(280 . 1)
                        (cons 1 (nth 0 cfg)) (cons 2 (nth 1 cfg))
                        (cons 40 (nth 2 cfg)) (cons 41 (nth 3 cfg))
                        (cons 3 (nth 4 cfg)))))
      (if rec (dictadd nod cl:*config-key* rec) nil))))

(defun cl:template-path (cfg / stored)
  (setq stored (nth 4 cfg))
  (if (and stored (/= stored ""))
    (cl:resolve-path stored)
    (if (/= (car cfg) "")
      (cl:ini-path (cl:resolve-path (car cfg)))
      "")))

(defun cl:project-data (/ cfg path rows templates ini)
  ;; One read per command. Do not update anything with missing source/template.
  (setq cfg (cl:get-config))
  (if (= (car cfg) "")
    (progn (princ "\nCSVLABEL: Run CLABELSOURCE first.") nil)
    (progn
      (setq path (cl:resolve-path (car cfg)))
      (setq rows (cl:read-csv path)
            ini (cl:template-path cfg))
      (if rows (setq templates (cl:read-templates ini)))
      (if (and rows templates)
        (list cfg rows templates)
        nil))))

(defun cl:template-name (preferred templates)
  (if (assoc (strcase preferred T) templates)
    (strcase preferred T)
    (caar templates)))

(defun cl:join (items separator / result)
  (setq result "")
  (while items
    (setq result (strcat result (car items)))
    (setq items (cdr items))
    (if items (setq result (strcat result separator))))
  result)

(defun cl:ask-template (templates default / answer)
  (setq default (cl:template-name default templates))
  (princ (strcat "\nTemplates: " (cl:join (mapcar 'car templates) ", ")))
  (setq answer (cl:trim (getstring T (strcat "\nTemplate <" default ">: "))))
  (if (= answer "") (setq answer default))
  (setq answer (strcase answer T))
  (if (assoc answer templates)
    answer
    (progn (princ (strcat "\nCSVLABEL: Unknown template: " answer)) nil)))

(defun cl:replace-all (s needle replacement / position start result)
  (setq start 0 result "")
  (while (setq position (vl-string-search needle s start))
    (setq result (strcat result (substr s (1+ start) (- position start)) replacement))
    (setq start (+ position (strlen needle))))
  (strcat result (substr s (1+ start))))

(defun cl:escape-mtext (s)
  ;; CSV data are literal text, never MTEXT control sequences.
  (setq s (cl:replace-all s "\\" "\\\\"))
  (setq s (cl:replace-all s "{" "\\{"))
  (cl:replace-all s "}" "\\}"))

(defun cl:render (tpl row / i n c closing key entry result)
  ;; Replace {any CSV column} (case insensitive), and literal \\n with MTEXT newline.
  (setq i 1 n (strlen tpl) result "")
  (while (<= i n)
    (setq c (substr tpl i 1))
    (cond
      ((and (= c "\\") (< i n) (= (substr tpl (1+ i) 1) "n"))
       (setq result (strcat result "\\P") i (+ i 2)))
      ((and (= c "{") (setq closing (vl-string-search "}" tpl i)))
       (setq key (strcase (cl:trim (substr tpl (1+ i) (- closing i)))))
       (setq entry (assoc key row))
       (setq result
         (strcat result
           (if entry (cl:escape-mtext (cdr entry)) (strcat "[MISSING:" key "]"))))
       (setq i (+ closing 2)))
      (T (setq result (strcat result c) i (1+ i)))))
  result)

(defun cl:entity-data (ename / xdata grp values)
  ;; Returns (record-id template-name). Older v0.1 labels have ID only.
  (setq xdata (assoc -3 (entget ename (list cl:*app*))))
  (if xdata
    (progn
      (setq grp (cadr xdata)
            values (mapcar 'cdr (vl-remove-if-not
                                 '(lambda (p) (= (car p) 1000))
                                 (cdr grp))))
      (if values (list (car values) (cadr values)) nil))
    nil))

(defun cl:set-entity-data (ename id template-name / ed)
  (regapp cl:*app*)
  (setq ed (entget ename (list cl:*app*)))
  (setq ed (vl-remove-if '(lambda (p) (= (car p) -3)) ed))
  (if (entmod
        (append ed (list (list -3 (list cl:*app*
                                         (cons 1000 id)
                                         (cons 1000 template-name))))))
    (progn (entupd ename) T)
    nil))

(defun cl:mtext-chunks (s / result)
  (setq result nil)
  (while (> (strlen s) 200)
    (setq result (cons (cons 3 (substr s 1 200)) result))
    (setq s (substr s 201)))
  (append (reverse result) (list (cons 1 s))))

(defun cl:make-label (pt content cfg)
  ;; Group 90: 16 = text frame, 2 = drawing-background mask.
  ;; Width 0 means no automatic text wrapping.
  (entmakex
    (append
      (list '(0 . "MTEXT") '(100 . "AcDbEntity")
            (cons 8 (getvar "CLAYER"))
            '(100 . "AcDbMText")
            (cons 10 (trans pt 1 0))
            (cons 40 (nth 2 cfg))
            (cons 41 (nth 3 cfg))
            (cons 7 (getvar "TEXTSTYLE"))
            '(71 . 5) '(72 . 1) '(73 . 1) '(44 . 1.0))
      (cl:mtext-chunks content)
      (list '(90 . 18) '(63 . 7) '(45 . 1.2)))))

(defun cl:set-text (ename content / obj result)
  (setq obj (vlax-ename->vla-object ename))
  (setq result (vl-catch-all-apply 'vla-put-TextString (list obj content)))
  (if (vl-catch-all-error-p result)
    nil
    (progn (vla-update obj) T)))

(defun cl:refresh-one (ename rows templates default / meta id template-name row tpl)
  (setq meta (cl:entity-data ename))
  (if (null meta)
    'skipped
    (progn
      (setq id (car meta)
            template-name (if (cadr meta) (cadr meta) default)
            row (assoc (strcase id) rows)
            tpl (assoc (strcase template-name T) templates))
      (cond
        ((not row) (princ (strcat "\n  Missing CSV id: " id)) 'missing)
        ((not tpl) (princ (strcat "\n  Missing template: " template-name)) 'missing)
        ((not (cl:set-text ename (cl:render (cdr tpl) (cdr row)))) 'failed)
        (T 'updated)))))

(defun cl:sync-status (status counters)
  ;; Counters: updated, missing, failed.
  (cond
    ((eq status 'updated)
     (list (1+ (car counters)) (cadr counters) (caddr counters)))
    ((eq status 'missing)
     (list (car counters) (1+ (cadr counters)) (caddr counters)))
    ((eq status 'failed)
     (list (car counters) (cadr counters) (1+ (caddr counters))))
    (T counters)))

(defun cl:sync-block-labels (rows templates default counters / block name flags head en ed)
  ;; Traverse regular block definitions as well as top-level MTEXT.
  ;; Shared block definitions display one shared record/template across inserts.
  (setq block (tblnext "BLOCK" T))
  (while block
    (setq name (cdr (assoc 2 block))
          flags (cdr (assoc 70 block)))
    (if (and name flags (= (logand flags 12) 0)
             (not (vl-string-search "*MODEL_SPACE" (strcase name)))
             (not (vl-string-search "*PAPER_SPACE" (strcase name))))
      (progn
        (setq head (tblobjname "BLOCK" name)
              en (if head (entnext head) nil))
        (while en
          (setq ed (entget en))
          (if (= (cdr (assoc 0 ed)) "ENDBLK")
            (setq en nil)
            (progn
              (if (and (= (cdr (assoc 0 ed)) "MTEXT") (cl:entity-data en))
                (setq counters (cl:sync-status (cl:refresh-one en rows templates default) counters)))
              (setq en (entnext en)))))))
    (setq block (tblnext "BLOCK")))
  counters)

(defun c:CLABELSOURCE (/ cfg path saved rows)
  ;; Pick the data source independently. Does not require a template file yet.
  (setq cfg (cl:get-config))
  (setq path (getfiled "Select project CSV" (cl:resolve-path (car cfg)) "csv" 0))
  (if path
    (progn
      (setq rows (cl:read-csv path))
      (if rows
        (progn
          (setq saved (cl:store-path path))
          (if (or (> (strlen saved) 240)
                  (not (cl:put-config (list saved (nth 1 cfg) (nth 2 cfg)
                                             (nth 3 cfg) (nth 4 cfg)))))
            (princ "\nCSVLABEL: Could not save source (check path length).")
            (princ (strcat "\nCSVLABEL: CSV linked: " (itoa (length rows))
                           " records. Use CLABELTEMPLATES to choose labels.ini.")))))))
  (princ))

(defun c:CLABELTEMPLATES (/ cfg path saved templates)
  ;; Independent templates source: can be in another project folder.
  (setq cfg (cl:get-config))
  (setq path (getfiled "Select project labels.ini" (cl:template-path cfg) "ini" 0))
  (if path
    (progn
      (setq templates (cl:read-templates path))
      (if templates
        (progn
          (setq saved (cl:store-path path))
          (if (or (> (strlen saved) 240)
                  (not (cl:put-config (list (nth 0 cfg)
                                             (cl:template-name (nth 1 cfg) templates)
                                             (nth 2 cfg) (nth 3 cfg) saved))))
            (princ "\nCSVLABEL: Could not save templates path (check length).")
            (princ (strcat "\nCSVLABEL: Templates linked: "
                           (itoa (length templates)) " named formats.")))))))
  (princ))

(defun c:CLABEL (/ project cfg rows templates id row template-name pt en)
  (setq project (cl:project-data))
  (if project
    (progn
      (setq cfg (car project) rows (cadr project) templates (caddr project))
      (setq id (cl:trim (getstring T "\nCSV record ID: "))
            row (assoc (strcase id) rows))
      (cond
        ((not row) (princ (strcat "\nCSVLABEL: Unknown ID: " id)))
        ((setq template-name (cl:ask-template templates (nth 1 cfg)))
         (if (setq pt (getpoint "\nPlace label: "))
           (progn
             (setq en (cl:make-label pt (cl:render (cdr (assoc template-name templates)) (cdr row)) cfg))
             (if (and en (cl:set-entity-data en id template-name))
               (progn
                 (cl:put-config (list (car cfg) template-name (nth 2 cfg) (nth 3 cfg) (nth 4 cfg)))
                 (princ (strcat "\nCSVLABEL: Placed " id " [" template-name "].")))
               (progn
                 (if en (entdel en))
                 (princ "\nCSVLABEL: Failed to create label.")))))))))
  (princ))

(defun c:CLABELSET (/ project cfg rows templates picked en meta oldid oldtemplate
                     id row template-name)
  (setq project (cl:project-data))
  (if project
    (progn
      (setq cfg (car project) rows (cadr project) templates (caddr project))
      (setq picked (entsel "\nSelect CSV label: "))
      (if picked
        (progn
          (setq en (car picked) meta (cl:entity-data en))
          (if (not meta)
            (princ "\nCSVLABEL: That object is not a CSV label.")
            (progn
              (setq oldid (car meta)
                    oldtemplate (if (cadr meta) (cadr meta) (nth 1 cfg)))
              (setq id (cl:trim (getstring T (strcat "\nRecord ID <" oldid ">: "))))
              (if (= id "") (setq id oldid))
              (setq row (assoc (strcase id) rows))
              (if (not row)
                (princ (strcat "\nCSVLABEL: Unknown ID: " id))
                (if (setq template-name (cl:ask-template templates oldtemplate))
                  (if (cl:set-text en (cl:render (cdr (assoc template-name templates)) (cdr row)))
                    (if (cl:set-entity-data en id template-name)
                      (progn (cl:apply-style en template-name cfg)
                             (princ (strcat "\nCSVLABEL: Updated " id " [" template-name "].")))
                      (princ "\nCSVLABEL: Text updated but metadata could not be saved."))
                    (princ "\nCSVLABEL: Could not update the label."))))))))))
  (princ))

(defun c:CLABELFORMAT (/ cfg height width)
  ;; Text size and wrap width apply to new labels, not existing geometry.
  (setq cfg (cl:get-config))
  (initget 6)
  (setq height (getreal (strcat "\nNew-label text height <" (rtos (nth 2 cfg) 2 2) ">: ")))
  (if (null height) (setq height (nth 2 cfg)))
  (initget 4)
  (setq width (getreal (strcat "\nWrap width (0 = no wrap) <" (rtos (nth 3 cfg) 2 2) ">: ")))
  (if (null width) (setq width (nth 3 cfg)))
  (if (cl:put-config (list (nth 0 cfg) (nth 1 cfg) height width (nth 4 cfg)))
    (princ "\nCSVLABEL: Size saved for new labels.")
    (princ "\nCSVLABEL: Could not save settings."))
  (princ))

(defun c:CLABELSYNC (/ project cfg rows templates ss i en counters doc result)
  (setq project (cl:project-data))
  (if project
    (progn
      (setq cfg (car project) rows (cadr project) templates (caddr project))
      (setq counters (list 0 0 0) i 0)
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      ;; Current model/paper-space MTEXT.
      (setq ss (ssget "_X" (list '(0 . "MTEXT") (list -3 (list cl:*app*)))))
      (if ss
        (while (< i (sslength ss))
          (setq en (ssname ss i))
          (setq counters (cl:sync-status
            (cl:refresh-one en rows templates (cl:template-name (nth 1 cfg) templates))
            counters))
          (setq i (1+ i))))
      ;; MTEXT stored inside ordinary block definitions.
      (setq counters (cl:sync-block-labels rows templates
        (cl:template-name (nth 1 cfg) templates) counters))
      (vla-EndUndoMark doc)
      ;; Refresh INSERT graphics when a shared block definition changed.
      (vl-catch-all-apply 'vla-Regen (list doc 1))
      (princ
        (strcat "\nCSVLABEL: " (itoa (car counters)) " updated, "
                (itoa (cadr counters)) " missing record/template, "
                (itoa (caddr counters)) " failed."))))
  (princ))

(regapp cl:*app*)
(princ "\nCSVLABEL core v0.4 loaded: CLABELSOURCE, CLABELTEMPLATES, CLABEL, CLABELSET, CLABELSYNC, CLABELFORMAT.")
(princ)

;;; ======================= v0.4 extensions ===============================
;;; v0.3 metadata and config keys are deliberately unchanged.
;;; OpenDCL Studio 10.1.2.2 converts the accompanying JSON into .odcl.

(setq cl:*styles* nil
      cl:*active-template* "detail"
      clui:*project* nil
      clui:*shown* nil
      clui:*id* nil
      clui:*template* nil
      clui:*odcl-path* nil
      clui:*busy* nil)

(defun cl:read-styles (path / f line s section pos key val result pair props)
  ;; Simple optional key/value entries in the existing labels.ini sections.
  (setq result nil section nil f (open path "r"))
  (if f
    (progn
      (while (setq line (read-line f))
        (setq s (cl:trim line))
        (cond
          ((or (= s "") (member (substr s 1 1) '(";" "#"))) nil)
          ((and (= (substr s 1 1) "[")
                (= (substr s (strlen s) 1) "]"))
           (setq section (strcase (cl:trim (substr s 2 (- (strlen s) 2))) T))
           (if (not (assoc section result))
             (setq result (append result (list (cons section nil))))))
          ((and section (setq pos (vl-string-search "=" s)))
           (setq key (strcase (cl:trim (substr s 1 pos)))
                 val (cl:trim (substr s (+ pos 2)))
                 pair (assoc section result)
                 props (cdr pair))
           (if (assoc key props)
             (setq props (subst (cons key val) (assoc key props) props))
             (setq props (append props (list (cons key val)))))
           (setq result (subst (cons section props) pair result)))))
      (close f)))
  result)

(defun cl:style-param (name key default / template pair)
  (setq template (assoc (strcase name T) cl:*styles*))
  (setq pair (if template (assoc (strcase key) (cdr template)) nil))
  (if pair (cdr pair) default))

(defun cl:positive-real (str fallback / x)
  (setq x (if str (distof str 2) nil))
  (if (and x (> x 0)) x fallback))

(defun cl:nonnegative-real (str fallback / x)
  (setq x (if str (distof str 2) nil))
  (if (and x (>= x 0)) x fallback))

(defun cl:true-p (value)
  (member (strcase (cl:trim value)) '("TRUE" "YES" "ON" "1")))

(defun cl:aci-color (value / x u)
  (setq u (strcase (cl:trim value)))
  (cond ((or (= u "BYLAYER") (= u "")) 256)
        ((= u "RED") 1) ((= u "YELLOW") 2) ((= u "GREEN") 3)
        ((= u "CYAN") 4) ((= u "BLUE") 5) ((= u "MAGENTA") 6)
        ((= u "WHITE") 7) ((= u "BLACK") 7)
        ((and (setq x (atoi u)) (<= 1 x) (<= x 255)) x)
        (T 256)))

(defun cl:layer-name (str)
  (if (or (= (cl:trim str) "")
          (vl-string-search "*" str)
          (vl-string-search "|" str)
          (vl-string-search "<" str)
          (vl-string-search ">" str)
          (vl-string-search "?" str))
    "CSV_LABELS"
    (cl:trim str)))

(defun cl:ensure-layer (name / layers found result)
  ;; Only called in document command context, never in a synchronous UI event.
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq result (vl-catch-all-apply 'vla-Item (list layers name)))
  (if (vl-catch-all-error-p result)
    (setq result (vl-catch-all-apply 'vla-Add (list layers name))))
  (not (vl-catch-all-error-p result)))

(defun cl:style (name cfg / h w layer color border mask pad)
  ;; Order: height width layer ACI border mask padding.
  (setq h (cl:positive-real (cl:style-param name "HEIGHT" "") (nth 2 cfg))
        w (cl:nonnegative-real (cl:style-param name "WIDTH" "") (nth 3 cfg))
        layer (cl:layer-name (cl:style-param name "LAYER" "CSV_LABELS"))
        color (cl:aci-color (cl:style-param name "COLOR" "ByLayer"))
        border (cl:true-p (cl:style-param name "BORDER" "true"))
        mask (cl:true-p (cl:style-param name "BACKGROUND" "true"))
        pad (cl:positive-real (cl:style-param name "PADDING" "1.20") 1.20))
  (list h w layer color border mask pad))

(defun cl:mask-bits (style)
  (+ (if (nth 4 style) 16 0)
     (if (nth 5 style) 2 0)))

(defun cl:make-label (pt content cfg / style ent)
  ;; New MTEXT is anchored at Middle Center (group 71=5).
  (setq style (cl:style cl:*active-template* cfg))
  (if (cl:ensure-layer (nth 2 style))
    (entmakex
      (append
        (list '(0 . "MTEXT") '(100 . "AcDbEntity")
              (cons 8 (nth 2 style))
              (cons 62 (nth 3 style))
              '(100 . "AcDbMText")
              (cons 10 (trans pt 1 0))
              (cons 40 (nth 0 style))
              (cons 41 (nth 1 style))
              (cons 7 (getvar "TEXTSTYLE"))
              '(71 . 5) '(72 . 1) '(73 . 1) '(44 . 1.0))
        (cl:mtext-chunks content)
        (list (cons 90 (cl:mask-bits style))
              '(63 . 7)
              (cons 45 (nth 6 style)))))
    nil))

(defun cl:apply-style (ename template cfg / style ed bits)
  ;; Keep group 10, group 71, and all geometry untouched on existing labels.
  ;; This avoids moving older top-left-anchored v0.3 labels during migration.
  (setq style (cl:style template cfg))
  (if (not (cl:ensure-layer (nth 2 style)))
    nil
    (progn
      (setq ed (entget ename))
      (setq ed (cl:replace-group ed 8 (nth 2 style)))
      (setq ed (cl:replace-group ed 62 (nth 3 style)))
      (setq ed (cl:replace-group ed 40 (nth 0 style)))
      (setq ed (cl:replace-group ed 41 (nth 1 style)))
      (setq ed (cl:replace-group ed 90 (cl:mask-bits style)))
      (setq ed (cl:replace-group ed 63 7))
      (setq ed (cl:replace-group ed 45 (nth 6 style)))
      (if (entmod ed) (progn (entupd ename) T) nil))))

(defun cl:project-data (/ cfg path rows templates ini)
  (setq cfg (cl:get-config))
  (if (= (car cfg) "")
    (progn (princ "\nCSVLABEL: Run CLABELSOURCE first.") nil)
    (progn
      (setq path (cl:resolve-path (car cfg)))
      (setq rows (cl:read-csv path)
            ini (cl:template-path cfg))
      (if rows (setq templates (cl:read-templates ini)))
      (if (and rows templates)
        (progn (setq cl:*styles* (cl:read-styles ini))
               (list cfg rows templates))
        nil))))

(defun cl:ask-template (templates default / answer)
  (setq default (cl:template-name default templates))
  (princ (strcat "\nTemplates: " (cl:join (mapcar 'car templates) ", ")))
  (setq answer (cl:trim (getstring T (strcat "\nTemplate <" default ">: "))))
  (if (= answer "") (setq answer default))
  (setq answer (strcase answer T))
  (if (assoc answer templates)
    (progn (setq cl:*active-template* answer) answer)
    (progn (princ (strcat "\nCSVLABEL: Unknown template: " answer)) nil)))

(defun cl:refresh-one (ename rows templates default / meta id name row tpl cfg)
  ;; IDs are authoritative; a missing ID must never match another CSV column.
  (setq meta (cl:entity-data ename))
  (if (null meta)
    'skipped
    (progn
      (setq id (car meta)
            name (if (cadr meta) (cadr meta) default)
            row (assoc (strcase id) rows)
            tpl (assoc (strcase name T) templates)
            cfg (cl:get-config))
      (cond
        ((not row) (princ (strcat "\n  Missing CSV id: " id)) 'missing)
        ((not tpl) (princ (strcat "\n  Missing template: " name)) 'missing)
        ((not (cl:set-text ename (cl:render (cdr tpl) (cdr row)))) 'failed)
        ((not (cl:apply-style ename name cfg)) 'failed)
        (T 'updated)))))

(defun cl:save-template-entries (path template updates / f s line name eq key section
                                      lines result seen found pair backup out)
  ;; Preserve INI contents except the edited section. Back up before writing.
  (setq f (open path "r") lines nil result nil
        seen nil found nil section nil)
  (if (null f) nil
    (progn
      (while (setq line (read-line f))
        (setq lines (cons line lines)))
      (close f)
      (setq lines (reverse lines))
      (foreach line lines
        (setq s (cl:trim line))
        (if (and (> (strlen s) 1)
                 (= (substr s 1 1) "[")
                 (= (substr s (strlen s) 1) "]"))
          (progn
            (if (= section (strcase template T))
              (foreach pair updates
                (if (not (member (car pair) seen))
                  (setq result (append result
                    (list (strcat (strcase (car pair) T) "=" (cdr pair))))))))
            (setq section (strcase (cl:trim (substr s 2 (- (strlen s) 2))) T)
                  seen nil)
            (if (= section (strcase template T)) (setq found T))))
        (if (and (= section (strcase template T))
                 (setq eq (vl-string-search "=" s))
                 (setq key (strcase (cl:trim (substr s 1 eq))))
                 (assoc key updates))
          (progn
            (setq line (strcat (strcase key T) "=" (cdr (assoc key updates))))
            (setq seen (cons key seen))))
        (setq result (append result (list line))))
      (if (= section (strcase template T))
        (foreach pair updates
          (if (not (member (car pair) seen))
            (setq result (append result
              (list (strcat (strcase (car pair) T) "=" (cdr pair))))))))
      (if (not found) nil
        (progn
          (setq backup (strcat path ".bak"))
          (if (findfile backup) (vl-file-delete backup))
          (if (not (vl-file-copy path backup)) nil
            (progn
              (setq out (open path "w"))
              (if (null out) nil
                (progn
                  (foreach line result (write-line line out))
                  (close out)
                  T)))))))))

(defun clui:plain-text (txt)
  (setq txt (cl:replace-all txt "\\P" "\n"))
  (setq txt (cl:replace-all txt "\\{" "{"))
  (setq txt (cl:replace-all txt "\\}" "}"))
  (cl:replace-all txt "\\\\" "\\"))

(defun clui:control (name)
  ;; OpenDCL binds handle symbols for the controls of an active form.
  (eval (read (strcat "CSVLabels/Palette/" name))))

(defun clui:ctext (name)
  (dcl-Control-GetText (clui:control name)))

(defun clui:set (name value)
  ;; OpenDCL Label controls use Caption; TextBox controls use Text.
  (if (member name '("Status" "CurrentRecord"))
    (dcl-Control-SetCaption (clui:control name) value)
    (dcl-Control-SetText (clui:control name) value)))

(defun clui:checked (name)
  (= 1 (dcl-Control-GetProperty (clui:control name) "Value")))

(defun clui:checkbox (name value)
  (dcl-Control-SetProperty (clui:control name) "Value" (if value 1 0)))

(defun clui:template-value (/ raw)
  (setq raw (clui:ctext "TemplateText"))
  ;; Literal backslash-n is the editable multiline delimiter (as in v0.3).
  raw)

(defun clui:visible-list (search / out id values)
  (setq out nil search (strcase (cl:trim search)))
  (foreach pair (cadr clui:*project*)
    (setq id (car pair)
          values (mapcar 'cdr (cdr pair)))
    (if (or (= search "")
            (vl-some '(lambda (v) (vl-string-search search (strcase v)))
                     (cons id values)))
      (setq out (append out (list pair)))))
  out)

(defun clui:fill-list (search / labels pos n row id)
  (setq clui:*shown* (clui:visible-list search))
  (setq labels nil pos nil n 0)
  (foreach row clui:*shown*
    (setq id (car row))
    (if (= id clui:*id*) (setq pos n))
    (setq labels (append labels
      (list (strcat id "  |  "
         (if (assoc "ITEM" (cdr row)) (cdr (assoc "ITEM" (cdr row))) "")
         "  "
         (if (assoc "DESCRIPTION" (cdr row)) (cdr (assoc "DESCRIPTION" (cdr row))) "")))))
    (setq n (1+ n)))
  (dcl-ListBox-Clear (clui:control "Records"))
  (if labels (dcl-ListBox-AddList (clui:control "Records") labels))
  (if (and (null pos) clui:*shown*)
    (setq pos 0 clui:*id* (caar clui:*shown*)))
  (if pos (dcl-ListBox-SetCurSel (clui:control "Records") pos))
  (clui:preview))

(defun clui:load-style (/ cfg name template)
  (if (and clui:*project* clui:*template*)
    (progn
      (setq cfg (car clui:*project*) name clui:*template*)
      (setq template (assoc name (caddr clui:*project*)))
      (clui:set "TemplateText" (if template (cdr template) ""))
      (clui:set "Height" (cl:style-param name "HEIGHT" (rtos (nth 2 cfg) 2 2)))
      (clui:set "Width" (cl:style-param name "WIDTH" (rtos (nth 3 cfg) 2 2)))
      (clui:set "Layer" (cl:style-param name "LAYER" "CSV_LABELS"))
      (clui:set "Color" (cl:style-param name "COLOR" "ByLayer"))
      (clui:checkbox "Border" (cl:true-p (cl:style-param name "BORDER" "true")))
      (clui:checkbox "Background" (cl:true-p (cl:style-param name "BACKGROUND" "true")))
      (clui:preview))))

(defun clui:preview (/ row content)
  (if (and clui:*project* clui:*id*)
    (progn
      (setq row (assoc (strcase clui:*id*) (cadr clui:*project*)))
      (setq content (if row
        (clui:plain-text (cl:render (clui:template-value) (cdr row)))
        "Select a record"))
      (clui:set "Preview" content)
      (clui:set "CurrentRecord" (strcat "Stable ID: " clui:*id*)))
    (clui:set "Preview" "Select a record")))

(defun clui:reload (/ proj tpl names default)
  (setq proj (cl:project-data))
  (if (not proj)
    (progn
      (setq clui:*project* nil)
      (clui:set "Status" "Choose CSV and template files")
      (clui:set "Preview" "No project data"))
    (progn
      (setq clui:*project* proj)
      (clui:set "Status" (strcat (itoa (length (cadr proj)))
                                  " records  |  " (itoa (length (caddr proj))) " templates"))
      (setq names (mapcar 'car (caddr proj)))
      (dcl-ComboBox-Clear (clui:control "Templates"))
      (dcl-ComboBox-AddList (clui:control "Templates") names)
      (setq default (if (and clui:*template* (member clui:*template* names))
                      clui:*template* (cl:template-name (nth 1 (car proj)) (caddr proj))))
      (setq clui:*template* default)
      (dcl-ComboBox-SetCurSel (clui:control "Templates")
                              (vl-position default names))
      (clui:load-style)
      (clui:fill-list (clui:ctext "Search")))))

(defun clui:save-current (/ cfg ini tpl updates h w)
  (if (not (and clui:*project* clui:*template*))
    (progn (princ "\nCSVLABEL: No active template.") nil)
    (progn
      (setq h (clui:ctext "Height") w (clui:ctext "Width")
            tpl (clui:template-value))
      (cond
        ((= (cl:trim tpl) "") (princ "\nCSVLABEL: Template cannot be blank.") nil)
        ((null (cl:positive-real h nil))
         (princ "\nCSVLABEL: Height must be positive.") nil)
        ((null (cl:nonnegative-real w nil))
         (princ "\nCSVLABEL: Width must be nonnegative.") nil)
        (T
         (setq cfg (car clui:*project*)
               ini (cl:template-path cfg)
               updates
                 (list (cons "TEMPLATE" tpl)
                       (cons "HEIGHT" h)
                       (cons "WIDTH" w)
                       (cons "LAYER" (cl:layer-name (clui:ctext "Layer")))
                       (cons "COLOR" (clui:ctext "Color"))
                       (cons "BORDER" (if (clui:checked "Border") "true" "false"))
                       (cons "BACKGROUND" (if (clui:checked "Background") "true" "false"))))
         (if (cl:save-template-entries ini clui:*template* updates)
           (progn
             (setq clui:*project* (cl:project-data))
             (clui:set "Status" (strcat "Saved: " clui:*template*))
             T)
           (progn (princ "\nCSVLABEL: Could not save labels.ini.") nil)))))))

(defun clui:palette-active-p ()
  (and (boundp 'CSVLabels/Palette)
       (dcl-Form-IsActive CSVLabels/Palette)))

(defun c:CSVLABEL (/ path loaded)
  ;; Only one discoverable command required. Older CLABEL... commands remain.
  ;; Call _OPENDCL unconditionally: the runtime handles an already loaded copy.
  (vl-catch-all-apply 'vl-cmdf (list "_OPENDCL"))
  (if (not clui:*odcl-path*)
    (setq clui:*odcl-path*
      (if (setq path (findfile "CSVLABEL.odcl")) path
          (getfiled "Select compiled CSVLABEL.odcl" "" "odcl" 0))))
  (cond
    ((not clui:*odcl-path*)
     (princ "\nCSVLABEL: Palette file not selected."))
    ((clui:palette-active-p)
     (princ "\nCSVLABEL: Palette is already open."))
    (T
     (setq loaded (vl-catch-all-apply 'dcl-Project-Load
                   (list clui:*odcl-path* T)))
     (if (or (vl-catch-all-error-p loaded) (not loaded))
       (princ "\nCSVLABEL: OpenDCL Runtime unavailable or palette failed to load. See README.")
       (dcl-Form-Show CSVLabels/Palette))))
  (princ))

;;; OpenDCL palette event handlers. Names MUST match CSVLABEL.odcl.json events.

(defun c:CSVLabels/Palette#OnInitialize ()
  (clui:set "Search" "")
  (clui:reload)
  (princ))

(defun c:CSVLabels/Palette/Search#OnEditChanged (newText)
  (if clui:*project* (clui:fill-list newText))
  (princ))

(defun c:CSVLabels/Palette/Records#OnSelChanged (idx value)
  (if (and (numberp idx) (>= idx 0) (< idx (length clui:*shown*)))
    (progn (setq clui:*id* (car (nth idx clui:*shown*)))
           (clui:preview)))
  (princ))

(defun c:CSVLabels/Palette/Templates#OnSelChanged (idx value)
  (if (and (numberp idx) (>= idx 0)
           (< idx (length (caddr clui:*project*))))
    (progn (setq clui:*template* (car (nth idx (caddr clui:*project*))))
           (clui:load-style)))
  (princ))

(defun c:CSVLabels/Palette/TemplateText#OnEditChanged (newText)
  (clui:preview)
  (princ))

(defun c:CSVLabels/Palette/Height#OnEditChanged (newText) (princ))
(defun c:CSVLabels/Palette/Width#OnEditChanged (newText) (princ))
(defun c:CSVLabels/Palette/Color#OnEditChanged (newText) (princ))
(defun c:CSVLabels/Palette/Layer#OnEditChanged (newText) (princ))

(defun c:CLUISAVE ()
  (clui:save-current)
  (princ))

(defun c:CLUISOURCE ()
  (c:CLABELSOURCE)
  (if (clui:palette-active-p) (clui:reload))
  (princ))

(defun c:CLUITEMPLATES ()
  (c:CLABELTEMPLATES)
  (if (clui:palette-active-p) (clui:reload))
  (princ))

(defun c:CLUIRELOAD ()
  (if (clui:palette-active-p) (clui:reload))
  (princ))

(defun c:CLUISYNC ()
  (c:CLABELSYNC)
  (if (clui:palette-active-p) (clui:reload))
  (princ))

(defun c:CLUIPLACE (/ proj cfg rows templates row tpl pt en id name count keep-going)
  ;; Triggered by an OpenDCL button with EventInvoke=AllowCommand (1),
  ;; so point selection and database edits occur in document command context.
  (if (and clui:*id* clui:*template* (clui:save-current))
    (progn
      (setq proj (cl:project-data)
            cfg (car proj) rows (cadr proj) templates (caddr proj)
            id clui:*id* name clui:*template*
            row (assoc (strcase id) rows)
            tpl (assoc name templates)
            count 0)
      (if (and row tpl)
        (progn
          (setq cl:*active-template* name)
          (princ "\nCSVLABEL: Click to place; press Enter or Esc when finished.")
          ;; Catch Esc, allowing normal CAD cancellation without unloading UI.
          (setq keep-going T)
          (while keep-going
            (setq pt (vl-catch-all-apply 'getpoint
                       (list "\nLabel center [Enter to finish]: ")))
            (if (or (null pt) (vl-catch-all-error-p pt))
              (setq keep-going nil)
              (progn
                (setq en (cl:make-label pt (cl:render (cdr tpl) (cdr row)) cfg))
                (if (and en (cl:set-entity-data en id name))
                  (setq count (1+ count))
                  (if en (entdel en))))))
          (princ (strcat "\nCSVLABEL: Placed " (itoa count) " labels.")))
        (princ "\nCSVLABEL: Missing ID or template. Reload palette."))))
  (princ))

(princ "\nCSVLABEL v0.4 loaded. Run CSVLABEL for dockable palette (OpenDCL required).")
(princ)
