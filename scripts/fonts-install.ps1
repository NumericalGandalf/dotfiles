$shell = New-Object -ComObject Shell.Application
$folder = $shell.Namespace((Get-Location).Path)
$regex = [regex]"\.ttf$"

foreach ($item in $folder.Items()) {
    if (-not $regex.IsMatch($item.Name)) {
        Remove-Item -Path $item.Path -Recurse -Force
    }
}

foreach ($verb in $folder.Items().Verbs()) {
    if ($verb.Name -eq "&Install") {
        $verb.DoIt()
        break
    }
}
