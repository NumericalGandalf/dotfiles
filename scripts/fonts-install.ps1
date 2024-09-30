$shell = New-Object -ComObject Shell.Application
$verbs = $shell.Namespace((Get-Location).Path).Items().Verbs()

foreach ($verb in $verbs) {
    if ($verb.Name -eq "&Install") {
        $verb.DoIt()
        break
    }
}
