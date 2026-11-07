param(
  [string]$ApplicationDirectory = (Split-Path $PSScriptRoot -Parent)
)

$ErrorActionPreference = 'Stop'
$ApplicationDirectory = (Resolve-Path -LiteralPath $ApplicationDirectory).Path
foreach ($file in @('CSVLABEL.lsp', 'CSVLABEL.odcl', 'CSVLABEL.bmp')) {
  if (-not (Test-Path -LiteralPath (Join-Path $ApplicationDirectory $file))) {
    throw "Missing application file: $file"
  }
}
$cad = [Runtime.InteropServices.Marshal]::GetActiveObject('BricscadApp.AcadApplication')
$group = $cad.MenuGroups.Item(0)
$cuiPath = $group.MenuFileName
$xml = New-Object System.Xml.XmlDocument
$xml.PreserveWhitespace = $true
$xml.Load($cuiPath)
if (-not $xml.SelectSingleNode("//RibbonPanelSource[@UID='rp2DAnnotations']/RibbonRow")) {
  throw 'The current main CUI has no Home / Annotations ribbon panel.'
}

$backup = Join-Path $ApplicationDirectory ('backup-ribbon-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Path $backup | Out-Null
Copy-Item -LiteralPath $cuiPath -Destination (Join-Path $backup 'before-save.cui')
# Remove only our earlier toolbar buttons. Keep the other toolbar contents.
$hideAccess = $false
foreach ($bar in $group.Toolbars) {
  if ($bar.Name -in @('Access', 'Standard', 'CSV Labels')) {
    for ($index = $bar.Count - 1; $index -ge 0; $index--) {
      $item = $bar.Item($index)
      if ($item.Name -eq 'CSV Labels') {
        $item.Delete()
        if ($bar.Name -eq 'Access') { $hideAccess = $true }
      }
    }
  }
}
$group.Save(0)
Copy-Item -LiteralPath $cuiPath -Destination (Join-Path $backup 'before-ribbon.cui')
$xml.Load($cuiPath)
$menuGroup = $xml.SelectSingleNode('/CustSection/MenuGroup')
$row = $xml.SelectSingleNode("//RibbonPanelSource[@UID='rp2DAnnotations']/RibbonRow")
$macroId = 'CSVLABEL_OPEN'
$toolbox = $xml.SelectSingleNode("/CustSection/MenuGroup/MacroGroup[@Name='CSV Labels']")
if (-not $toolbox) {
  $toolbox = $xml.CreateElement('MacroGroup')
  $toolbox.SetAttribute('Name', 'CSV Labels')
  [void]$menuGroup.InsertBefore($toolbox, $menuGroup.SelectSingleNode('MacroGroup'))
}
foreach ($existing in @($xml.SelectNodes("//MenuMacro[@UID='$macroId']"))) {
  [void]$existing.ParentNode.RemoveChild($existing)
}
$menuMacro = $xml.CreateElement('MenuMacro')
$menuMacro.SetAttribute('UID', $macroId)
$macro = $xml.CreateElement('Macro')
$appLispPath = $ApplicationDirectory.Replace('\', '/').Replace('"', '\"')
$command = '^C^C(progn (setq cl:*home* "' + $appLispPath + '") (if (not c:CSVLABEL) (load (strcat cl:*home* "/CSVLABEL.lsp"))) (c:CSVLABEL));'
foreach ($entry in @(@('Name', 'CSV Labels'), @('Command', $command), @('HelpString', 'Find a CSV record and place a label'))) {
  $node = $xml.CreateElement($entry[0])
  $node.InnerText = $entry[1]
  [void]$macro.AppendChild($node)
}
$image = $xml.CreateElement('Image')
$image.SetAttribute('ID', (Join-Path $ApplicationDirectory 'CSVLABEL.bmp'))
[void]$macro.AppendChild($image)
[void]$menuMacro.AppendChild($macro)
[void]$toolbox.AppendChild($menuMacro)
foreach ($existing in @($xml.SelectNodes("//RibbonCommandButton[@MenuMacroID='$macroId']"))) {
  [void]$existing.ParentNode.RemoveChild($existing)
}
$button = $xml.CreateElement('RibbonCommandButton')
$button.SetAttribute('MenuMacroID', $macroId)
$button.SetAttribute('Text', 'CSV Labels')
$button.SetAttribute('ButtonStyle', 'LargeWithText')
[void]$row.AppendChild($button)
if ($hideAccess) {
  $workspace = $cad.ActiveDocument.GetVariable('WSCURRENT')
  foreach ($node in $xml.SelectNodes('//Workspace')) {
    if ($node.SelectSingleNode('Name').InnerText -eq $workspace) {
      $access = $node.SelectSingleNode("WSToolbarRoot/WSToolbar[@pUID='tb2DAccess']")
      if ($access) { $access.SetAttribute('Display', '0') }
    }
  }
}
$candidate = Join-Path $backup 'installed.cui'
$xml.Save($candidate)
$check = New-Object System.Xml.XmlDocument
$check.Load($candidate)
if ($check.SelectNodes("//RibbonCommandButton[@MenuMacroID='$macroId']").Count -ne 1) {
  throw 'Expected exactly one CSV Labels ribbon button.'
}
Copy-Item -LiteralPath $candidate -Destination $cuiPath -Force
$menu = $cuiPath.Replace('\', '/').Replace('"', '\"')
$reload = '(progn (setq cl:ribbon-filedia (getvar "FILEDIA")) (setvar "FILEDIA" 0) (vl-cmdf "_.MENU" "' + $menu + '") (setvar "FILEDIA" cl:ribbon-filedia) (princ))'
$cad.ActiveDocument.SendCommand($reload + [char]10)
Write-Output "Added one icon to Home / Annotations. Backup: $backup"
