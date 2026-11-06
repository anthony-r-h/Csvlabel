# CSVLABEL

CSV-driven drawing labels for BricsCAD.

CSVLABEL places compact, framed text annotations in a DWG. Each label refers to a **stable `id` in a CSV** and uses a **named template** to determine which columns appear and how the text is arranged. Change the CSV or a template, then synchronize to update existing labels without moving them.

The optional dockable palette lets you search records, preview their expanded text, select a layout, place multiple labels, and synchronize. Labels use a **middle-center insertion point**.

> **Package status:** This archive contains the AutoLISP source and editable OpenDCL palette definition, **not the compiled `CSVLABEL.odcl`**. The graphical palette requires a one-time Windows build described under [Building the palette](#building-the-palette). The core command-line labeling commands are included, but the OpenDCL palette has **not been tested inside BricsCAD**. Try this on a copy of a drawing first.

## Files

```text
CSVLABEL/
├── README.md
├── CSVLABEL.lsp              AutoLISP application
├── CSVLABEL.odcl             OpenDCL palette; generated once on Windows (not included)
├── source/
│   └── CSVLABEL.odcl.json   Editable palette definition
└── example/
    ├── Book1.csv             Example data
    └── labels.ini            Example label templates
```

The application files can live in a shared tools folder or code repository. Each drawing/project can use its own CSV and `labels.ini`; those files do not have to live beside the script.

## Requirements

- **BricsCAD for Windows** with AutoLISP and ActiveX/VLAX support.
- **OpenDCL Runtime** compatible with your BricsCAD version for the dockable interface: [OpenDCL releases](https://github.com/opendcl/OpenDCL/releases).
- A compiled `CSVLABEL.odcl` alongside `CSVLABEL.lsp` for the graphical interface. This source archive does not yet include one.

OpenDCL's Studio and runtime are Windows applications. The palette is **not supported natively on macOS or Linux**. See [Building the palette](#building-the-palette) for platform-specific notes.

## Installation and first use

1. Install an OpenDCL Runtime supported by your BricsCAD release. If you also install OpenDCL Studio, it includes the runtime.
2. Ensure `CSVLABEL.odcl` has been built and placed beside `CSVLABEL.lsp` (see build instructions below).
3. In BricsCAD, run `APPLOAD` and select `CSVLABEL.lsp`.
4. Run `CSVLABEL` to open the palette. If prompted, browse to `CSVLABEL.odcl`.
5. Click **CSV file...** and select your project's CSV.
6. Click **Templates...** and select its `labels.ini`.

You can start by selecting `example/Book1.csv` and `example/labels.ini`. Source paths are saved with the drawing, so you normally select them only once per DWG.

### Place a label

1. Search by stable ID, item code, or description.
2. Select a record from the results.
3. Select a named template and check the text preview.
4. Click **PLACE LABELS** and click the desired center of each label in the drawing.
5. Press **Enter** or **Esc** to end placement. The palette stays available for the next record.

The click point is the middle of the text box, not a corner. A label remains anchored there when its contents change.

### Update labels

After changing the CSV or template file, click **Sync all**, or run `CLABELSYNC`. Existing labels refresh from their stored stable ID and template name. They keep their placement point. Synchronization is **manual**, not continuous.

### Change label appearance

Select a template in the palette. You can edit its text layout and these settings:

| Setting | Meaning |
| --- | --- |
| Height | Text height in drawing units |
| Wrap width | MTEXT width in drawing units; `0` means no wrapping |
| Layer | Target drawing layer |
| Color | `ByLayer` or an AutoCAD Color Index (ACI) number |
| Frame | Box around the label |
| Mask | Drawing-background mask behind the text |

Click **Save template** to write changes to the selected `labels.ini`; the application creates a backup (`.bak`) before rewriting it. Then click **Sync all** to apply changes to labels already in the drawing.

The preview expands CSV fields and line breaks, but is **not a pixel-perfect representation** of the final BricsCAD font, colours, wrapping, or border.

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
color=ByLayer
border=true
background=true

[cutting]
template={Item}: {Description}\n{Thickness} | x{Count}
height=2.5
width=0
layer=CUTTING_LABELS
color=ByLayer
border=true
background=true
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
- **Changes aren't visible:** Save the CSV/INI and click **Sync all**.

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

After building, a deployable Windows package would include `CSVLABEL.lsp`, `CSVLABEL.odcl`, a user-facing README, and optional example data. It would not need `CSVLABEL.odcl.json` or any PowerShell build script.
