# CSVLABEL

CSV-driven drawing labels for BricsCAD.

CSVLABEL places compact, framed text annotations in a DWG. Each label refers to a **stable `id` in a CSV** and uses a **named template** to determine which columns appear and how the text is arranged. Change the CSV or a template, then synchronize to update existing labels without moving them.

The optional dockable palette lets you search records, preview their expanded text, select a template, place one label, and synchronize. Labels use a **middle-center insertion point**.

The package includes the compiled palette and toolbar icon. Native smoke checks have been run in BricsCAD V22 with OpenDCL 10.1.2.2.

## Files

```text
CSVLABEL/
├── README.md
├── CSVLABEL.lsp              AutoLISP application
├── CSVLABEL.odcl             Compiled OpenDCL palette and template editor
├── CSVLABEL.bmp              Toolbar icon
├── source/
│   ├── CSVLABEL.odcl.json    Editable palette definition
│   └── install-ribbon.ps1    Install the single Home / Annotations icon
└── example/
    ├── Book1.csv             Example data
    └── labels.ini            Example label templates
```

The application files can live in a shared tools folder or code repository. Each drawing/project can use its own CSV and `labels.ini`; those files do not have to live beside the script.

## Requirements

- **BricsCAD for Windows** with AutoLISP and ActiveX/VLAX support.
- **OpenDCL Runtime** compatible with your BricsCAD version for the dockable interface: [OpenDCL releases](https://github.com/opendcl/OpenDCL/releases).
- A compiled `CSVLABEL.odcl` alongside `CSVLABEL.lsp` for the graphical interface. Included in this package.

OpenDCL's Studio and runtime are Windows applications. The palette is **not supported natively on macOS or Linux**. See [Building the palette](#building-the-palette) for platform-specific notes.

## Installation and first use

1. Install an OpenDCL Runtime supported by your BricsCAD release. If you also install OpenDCL Studio, it includes the runtime.
2. Keep `CSVLABEL.lsp`, `CSVLABEL.odcl`, and `CSVLABEL.bmp` together.
3. In BricsCAD, run `APPLOAD` and select `CSVLABEL.lsp`. Enable its **Autoload** option to restore it in future sessions.
4. Click the yellow tag in **Home → Annotations**, beside the existing text and dimension tools. `CSVLABEL` opens the same interface. Loading the LISP does not create or show a toolbar.
5. Click **CSV file...** and select your project's CSV.
6. Click **Templates file…** and select its `labels.ini`.

You can start by selecting `example/Book1.csv` and `example/labels.ini`. Source paths are saved with the drawing, so you normally select them only once per DWG.

The OpenDCL palette can dock at a window edge, but it is not BricsCAD's native side-panel type and cannot join the native panel icon strip.

**Install the ribbon button once:** with BricsCAD running, execute `source/install-ribbon.ps1` in Windows PowerShell. Its `-ApplicationDirectory` parameter can point to a separate installed application folder. The installer backs up the current main CUI, adds one button to the existing Home / Annotations panel, and removes earlier CSV Labels toolbar buttons. It is safe to run again without duplicating the ribbon button. A BricsCAD customization reset may require reinstalling it.

### Place a label

1. Search across any CSV field.
2. Select a row in the table. All CSV columns are shown; scroll horizontally when needed.
3. Select a named template and check the text preview.
4. Click **Place label**, then click its center in the drawing. This places exactly one label.
5. Choose another row or click **Place label** again for another copy. Press **Esc** to cancel a pending placement.

Placement uses the saved template and does not modify `labels.ini`.

The click point is the middle of the text box, not a corner. A label remains anchored there when its contents change.

### Update labels

After changing the CSV or template file, click **Sync labels**, or run `CLABELSYNC`. Existing labels refresh from their stored stable ID and template name. They keep their placement point. Synchronization is **manual**, not continuous.

### Change label appearance

Select a template and click **Edit…**. The separate editor contains the text layout, size, and layer. Press Enter in the layout for a new line, or choose a CSV column and click **Insert column**. Blank lines are preserved in the preview and drawing.

| Setting | Meaning |
| --- | --- |
| Height | Text height in drawing units |
| Wrap width | MTEXT width in drawing units; `0` means no wrapping |
| Layer | Dropdown of current drawing layers; a new layer name can also be entered |
All labels use white text, a solid black fill, and a white border. This is native MTEXT, so the label remains one object. Older `color`, `border`, `background`, and `whiteonblack` settings are ignored. Plot-style overrides can still affect printed colours.

Wrap text in `[i]…[/i]` in the layout to italicize it; for example, `[i]{Notes}[/i]`. This works with any CSV column and does not make “Notes” a special column name.

Click **Save template** to write changes to the selected `labels.ini`; the application creates a backup (`.bak`) before rewriting it. Use **Cancel** to discard edits. Then click **Sync labels** to apply saved changes to labels already in the drawing.

The preview expands CSV fields and line breaks, but is **not a pixel-perfect representation** of the final BricsCAD font, italics, wrapping, or border.

## Project data and templates

The CSV must include a column named **`id`**. Its value identifies the record permanently, regardless of row order. Other columns are free-form. For example:

```csv
id,Item,Description,Thickness,Count
1,A-01,Front panel,3mm,1
2,B-01,Stiffener,0.5mm,1
```

`id` is the authoritative lookup key. All other columns are user-defined; `Item` is only an example and has no special role in linking labels. Layouts determine which columns appear. Reordering rows does not affect links. If an ID is missing, synchronization reports it and leaves the existing label unchanged. If you change or reuse IDs, labels follow those IDs; managing that reassignment is your responsibility.

Create any number of named sections in `labels.ini`:

```ini
[compact]
template={Item} | {Description}
height=2
width=0
layer=CSV_LABELS

[cutting]
template={Item}: {Description}\n{Thickness} | x{Count}
height=2.5
width=0
layer=CUTTING_LABELS
```

- Use `{ColumnName}` to insert the value of a CSV column. Column names are case-insensitive.
- Use literal `\n` for a new line within a label.
- A missing column is displayed as `[MISSING:COLUMN]` so errors are visible.
- Each placed label remembers **its record ID and template name**. Editing a named template can change the appearance of every annotation that uses it on the next sync.

CSV files must use a single physical line per record. Quoted commas and doubled quotes are supported, but embedded line breaks inside quoted CSV fields are not.

## Optional command-line commands

The palette is designed to replace day-to-day command memorization. If necessary, these commands remain available:

| Command | Action |
| --- | --- |
| `CSVLABEL` | Open the dockable palette |
| `CSVLABELTOOLBAR` | Optional: add a button to Access (or Standard), showing that toolbar |
| `CLABELSOURCE` | Select the CSV source |
| `CLABELTEMPLATES` | Select `labels.ini` independently |
| `CLABEL` | Place one label from a record ID and template |
| `CLABELSET` | Reassign an existing label's record ID |
| `CLABELSYNC` | Refresh existing labels |
| `CLABELFORMAT` | Set default text height and width for command-line placement |

## Working with blocks

A label included in a **block definition** moves, rotates, and scales with the block. All inserts of that block definition share the same annotation data. Placing a label over an existing object without adding it to the object's block definition **does not** attach the label to the object.

## Troubleshooting

- **`CSVLABEL` cannot open a palette:** Confirm the OpenDCL Runtime is installed, `CSVLABEL.odcl` exists, and your BricsCAD version is supported. Run `APPLOAD` again after updating `CSVLABEL.lsp`.
- **The CSV or templates cannot be read:** Reselect the files from the palette and confirm the DWG's stored paths still point to accessible locations.
- **A label shows `[MISSING:...]`:** The template references a column not present in the CSV header.
- **A record is missing:** Confirm its stable `id` is present, unique, and has not been renamed.
- **Changes aren't visible:** Save the CSV/INI and click **Sync labels**.

## Building the palette

**For developers or source distributions only.** An ordinary release should include the already-generated `CSVLABEL.odcl`. End users should not need Studio or a build script.

OpenDCL Studio **10.1.2.2 or newer** supports importing `.odcl.json` and exporting the `.odcl` file. Download it from the [OpenDCL releases](https://github.com/opendcl/OpenDCL/releases). Build once after changing the palette's controls, appearance, or layout. Changing CSV data, `labels.ini`, or AutoLISP code alone does not require rebuilding the `.odcl`.

**Windows (graphical):** Open `source/CSVLABEL.odcl.json` in OpenDCL Studio, then save/export it as `CSVLABEL.odcl` beside `CSVLABEL.lsp`.

**Windows (PowerShell):** Use Studio's command-line converter. Substitute your installed Studio executable path:

```powershell
& "C:\path\to\OpenDCL-Studio.exe" ".\source\CSVLABEL.odcl.json" /out ".\CSVLABEL.odcl"
```

**Windows (Command Prompt):**

```bat
"C:\path\to\OpenDCL-Studio.exe" "source\CSVLABEL.odcl.json" /out "CSVLABEL.odcl"
```

**Linux or macOS:** There is no native OpenDCL Studio build path. To produce a `.odcl`, use Windows (physical machine or VM), then copy the generated file into the application directory. The OpenDCL palette itself requires Windows, even though BricsCAD also has macOS and Linux editions.

After building, a deployable Windows package would include `CSVLABEL.lsp`, `CSVLABEL.odcl`, `CSVLABEL.bmp`, a user-facing README, and optional example data. It would not need `CSVLABEL.odcl.json` or any PowerShell build script.

### Callback bindings

JSON event values must include `c:` (for example, `c:CLUISEARCH`). Without that prefix, BricsCAD may invoke a command with no arguments instead of calling the event handler with its parameters. Keep search and selection events synchronous (`EventInvoke=0`); drawing actions use command context (`EventInvoke=1`).

### Native regression checks

After loading the application, open a new empty, unsaved drawing, load `tests/CSVLABEL-smoke.lsp`, and run `CSVLABELTEST`. It uses temporary CSV/INI fixtures and tests native filtering/selection events, multiline previews, the template editor, sync counters, and ID-only lookup. It leaves the fixture-backed palette available for inspection. Test single-label point picking and toolbar clicks interactively as well.
