Invoke-Expression (&starship init powershell)
Import-Module posh-git
Import-Module PSReadLine
Import-Module PSFzf
Invoke-Expression (& { (zoxide init powershell | Out-String) })

function Invoke-FzfHistory {
    $history = Get-Content (Get-PSReadLineOption).HistorySavePath | fzf
    if ($history) {
        [Microsoft.PowerShell.PSConsoleReadLine]::RevertLine()
        [Microsoft.PowerShell.PSConsoleReadLine]::Insert($history)
    }
}

Set-PSReadLineKeyHandler -Key Ctrl+r -ScriptBlock { Invoke-FzfHistory }

(& uv generate-shell-completion powershell) | Out-String | Invoke-Expression
(& uvx --generate-shell-completion powershell) | Out-String | Invoke-Expression
Invoke-Expression (&scoop-search --hook)

