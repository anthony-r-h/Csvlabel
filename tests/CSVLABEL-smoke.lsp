;;; Run CSVLABELTEST after loading CSVLABEL.lsp, in an empty unsaved drawing.
;;; Fixtures are written to the OS temporary folder, never the user's CSV/INI.
(vl-load-com)

(defun cltest:assert (label condition)
  (if (not condition) (setq cltest:*failures* (1+ cltest:*failures*)))
  (setq cltest:*messages* (cons (strcat (if condition "PASS " "FAIL ") label) cltest:*messages*))
  (princ (strcat "\n" (if condition "PASS " "FAIL ") label)))

(defun cltest:write (path lines / stream)
  (setq stream (open path "w"))
  (foreach line lines (write-line line stream))
  (close stream))

(defun cltest:editor-checks ()
  (apply cltest:*editor-init* nil)
  (cltest:assert "editor renders multiple lines"
    (> (dcl-TextBox-GetLineCount "CSVLABEL" "Editor" "Preview") 1))
  (cltest:assert "layer dropdown includes drawing layer"
    (>= (dcl-ComboBox-FindStringExact "CSVLABEL" "Editor" "Layer" "0") 0))
  (cltest:assert "column picker follows CSV"
    (= 3 (dcl-ComboBox-GetCount "CSVLABEL" "Editor" "Fields")))
  (clui:put "Editor" "Height" "Text" "0")
  (cltest:assert "zero height rejected" (not (clui:save-current)))
  (clui:put "Editor" "Height" "Text" "2.5")
  (clui:put "Editor" "TemplateText" "Text" "{Species}\r\n\r\n{Size}")
  (c:CLUIEDITTEXT "{Species}\r\n\r\n{Size}")
  (cltest:assert "editor preserves blank lines"
    (= "Maple\r\n\r\n30" (clui:get "Editor" "Preview" "Text")))
  (cltest:assert "editor encodes newlines for INI"
    (= "{Species}\\n\\n{Size}" (clui:template-value)))
  (cltest:assert "preview is always white on black"
    (and (= 0 (clui:get "Editor" "Preview" "BackColor"))
         (= 16777215 (clui:get "Editor" "Preview" "ForeColor"))))
  (c:CLUISAVE))

(defun c:CSVLABELTEST (/ *error* csv ini config entity original-text)
  (defun *error* (message)
    (if cltest:*editor-init* (setq c:CLUIEDITORINIT cltest:*editor-init*))
    (dcl-SuppressMessages nil)
    (princ (strcat "\nCSVLABELTEST ERROR: " message))
    (princ))
  (if (or (= 1 (getvar "DWGTITLED"))
          (> (vla-get-Count (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))) 0))
    (princ "\nRun this test in a new, empty, unsaved drawing.")
    (progn
      (setq cltest:*complete* nil cltest:*failures* 0 cltest:*messages* nil
            csv (vl-filename-mktemp "csvlabel-test-" nil ".csv")
            ini (vl-filename-mktemp "csvlabel-test-" nil ".ini"))
      (cltest:write csv '("id,Species,Size" "A,Birch,12" "B,Oak,24" "C,Maple,30"))
      (cltest:write ini '("[custom]" "template={Species}\\n\\nSize {Size}" "height=2.5"
        "width=0" "layer=CSV_LABELS" "whiteonblack=false" "border=false" "background=false" "color=1" "[short]" "template={id}: {Species}"))
      (setq config (list csv "custom" 2.5 0.0 ini))
      (cl:put-config config)
      (c:CSVLABEL)
      (dcl-SuppressMessages T)
      (clui:put "Palette" "Search" "Text" "")
      (setq clui:*id* nil clui:*template* "custom")
      (clui:reload)
      (cltest:assert "arbitrary columns" (= 3 (dcl-Grid-GetColumnCount "CSVLABEL" "Palette" "Records")))
      (cltest:assert "all records" (= 3 (dcl-Grid-GetRowCount "CSVLABEL" "Palette" "Records")))
      (cltest:assert "multiline preview" (= "Birch\r\n\r\nSize 12" (clui:get "Palette" "Preview" "Text")))
      ;; ReplaceSel and SetCurCell exercise real OpenDCL events, not direct handlers.
      (dcl-TextBox-SetSel "CSVLABEL" "Palette" "Search" 0 -1)
      (dcl-TextBox-ReplaceSel "CSVLABEL" "Palette" "Search" "oak")
      (cltest:assert "text event filters arbitrary fields" (= 1 (dcl-Grid-GetRowCount "CSVLABEL" "Palette" "Records")))
      (cltest:assert "filtered selection" (= "B" clui:*id*))
      (dcl-TextBox-SetSel "CSVLABEL" "Palette" "Search" 0 -1)
      (dcl-TextBox-ReplaceSel "CSVLABEL" "Palette" "Search" "zzzz")
      (cltest:assert "empty search result clears ID" (null clui:*id*))
      (cltest:assert "empty result disables placement" (not (clui:get "Palette" "Place" "Enabled")))
      (dcl-TextBox-SetSel "CSVLABEL" "Palette" "Search" 0 -1)
      (dcl-TextBox-ReplaceSel "CSVLABEL" "Palette" "Search" "")
      (dcl-Grid-SetCurCell "CSVLABEL" "Palette" "Records" 2 1)
      (cltest:assert "selection event changes ID" (= "C" clui:*id*))
      (cltest:assert "selection updates preview" (= "Maple\r\n\r\nSize 30" (clui:get "Palette" "Preview" "Text")))
      (c:CLUITEMPLATE 1 "short")
      (cltest:assert "template updates preview" (= "C: Maple" (clui:get "Palette" "Preview" "Text")))
      (c:CLUITEMPLATE 0 "custom")
      (setq cltest:*editor-init* c:CLUIEDITORINIT)
      (defun c:CLUIEDITORINIT (/ result)
        (setq result (vl-catch-all-apply 'cltest:editor-checks nil))
        (if (vl-catch-all-error-p result)
          (progn
            (cltest:assert (strcat "editor: " (vl-catch-all-error-message result)) nil)
            (dcl-Form-Close "CSVLABEL" "Editor" 2)))
        (princ))
      (c:CLUIEDIT)
      (setq c:CLUIEDITORINIT cltest:*editor-init* cltest:*editor-init* nil)
      (cltest:assert "saved layout reloads" (= "{Species}\\n\\n{Size}" (cdr (assoc "custom" (cl:read-templates ini)))))
      (cltest:assert "palette reflects saved contrast"
        (= 0 (clui:get "Palette" "Preview" "BackColor")))
      (cltest:assert "italic layout works with arbitrary columns"
        (= "Title\\P{\\fArial|b0|i1;Maple}\\PTail"
           (cl:render "Title\\n[i]{Species}[/i]\\nTail" '(("SPECIES" . "Maple")))))
      (cltest:assert "preview hides italic markup and keeps blank lines"
        (= "Title\r\n\r\nMaple" (cl:expand-template "Title\\n\\n[i]{Species}[/i]" '(("SPECIES" . "Maple")) "\r\n" nil)))
      (cltest:assert "CSV text cannot inject italic or MTEXT markup"
        (= "{\\fArial|b0|i1;[i]literal[/i] \\{x\\} \\\\P}"
           (cl:render "[i]{Species}[/i]" '(("SPECIES" . "[i]literal[/i] {x} \\P")))))
      (cltest:assert "unclosed italic tag remains literal"
        (= "[i]Maple" (cl:render "[i]{Species}" '(("SPECIES" . "Maple")))))
      (cltest:assert "counters accumulate" (equal '(1 1 1) (cl:sync-status 'failed (cl:sync-status 'missing (cl:sync-status 'updated '(0 0 0))))))
      (setq cl:*active-template* "custom" entity (cl:make-label '(0 0 0) "Original" config))
      (cltest:assert "contrast label uses fixed white text"
        (= 16777215 (cdr (assoc 420 (entget entity)))))
      (cltest:assert "label has native black fill and white frame"
        (and (= 17 (cdr (assoc 90 (entget entity))))
             (= 0 (cdr (assoc 421 (entget entity))))))
      (cltest:assert "contrast label is one MTEXT object"
        (and (= "MTEXT" (cdr (assoc 0 (entget entity))))
             (= 1 (vla-get-Count (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))))))
      (cl:apply-style entity "short" config)
      (cltest:assert "all templates share framed white-on-black appearance"
        (and (= 16777215 (cdr (assoc 420 (entget entity))))
             (= 0 (cdr (assoc 421 (entget entity))))
             (= 17 (cdr (assoc 90 (entget entity))))))
      (cl:apply-style entity "custom" config)
      (cltest:assert "sync can enable contrast without moving label"
        (and (= 16777215 (cdr (assoc 420 (entget entity))))
             (= 0 (cdr (assoc 421 (entget entity))))
             (equal '(0.0 0.0 0.0) (cdr (assoc 10 (entget entity))))))
      (cl:set-entity-data entity "A" "custom")
      (cltest:assert "missing ID never matches Item"
        (eq 'missing (cl:refresh-one entity '(("B" ("ID" . "B") ("ITEM" . "A"))) '(("custom" . "{id}")) "custom")))
      (cltest:assert "missing record leaves label untouched"
        (= "Original" (vla-get-TextString (vlax-ename->vla-object entity))))
      (entdel entity)
      (dcl-SuppressMessages nil)
      (setq cltest:*complete* T)
      (princ (strcat "\nCSVLABELTEST: " (itoa cltest:*failures*) " failures."))))
  (princ))
(princ "\nRun CSVLABELTEST in a new, empty, unsaved drawing.")
(princ)
