Invoke-Expression (&starship init powershell)
Import-Module posh-git
Invoke-Expression (& { (zoxide init powershell | Out-String) })
Get-ChildItem "$PROFILE\..\Completions\" | ForEach-Object {
    . $_.FullName
}

