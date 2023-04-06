function Select-FilesUpToSize {
    [CmdletBinding()]param(
        [String]$Path = (Get-Location),
        [UInt64]$MaxSize
    )
    $files = Get-ChildItem -Recurse -File $Path | Sort-Object Length
    $SelectedFiles ??= [System.IO.FileInfo[]]@()
    $totalSize = [UInt64]0gb
    $index = [UInt64]0
<# This can probably be optimized, and I’ve attempted it (hence the weird loop),
but I have too little knowledge of PowerShell’s speed & execution manner and I’m in no mood to test.
At this point it’s not the solution I would’ve used if performance was no issue,
but also probably not an actually well-optimized solution.#>
    while ($true) {
        try {
            $file = $files.Get($index)
        } catch [System.IndexOutOfRangeException] {
            break
        }
        $fileSize = $file.Length
        $totalSize += $fileSize
        if ($totalSize -gt $MaxSize) {
            $totalSize -= $fileSize
            break
        }
        $SelectedFiles += $file
        Write-Output $file
        Write-Information @"
Current file:
    Name:       $($file.Name)
    Directory:  $($file.DirectoryName)
    Size:       $fileSize
Progress:
    Index:      $index
    Total size: $totalSize
"@
    $index += 1
    }
}
