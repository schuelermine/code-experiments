#!/usr/bin/env nix-shell
#!nix-shell -i pwsh -p powershell coreutils gnutar nix
<#
.SYNOPSIS
    Download PowerShell of a given version and compute its hash.
.DESCRIPTION
    Download PowerShell of a given version and compute its hash, or download and hash the current PowerShell version
.PARAMETER Version
    The version to hash
.PARAMETER Verify
    Download and hash the current PowerShell version
#>

[CmdletBinding()]
param (
    [Parameter(
        Mandatory = $true,
        ParameterSetName = "get-hashes-for-version"
    )]
    [String]$Version,
    [Parameter(
        Mandatory = $true,
        ParameterSetName = "verify-hashes",
        HelpMessage = "Verify downloads the current "
    )]
    [Switch]$Verify
)

if ($PSCmdlet.ParameterSetName -eq "verify-hashes") {
    $Version = [String]$PSVersionTable.PSVersion
}

$archs = "x64", "arm64"
$plats = "osx", "linux"
foreach ($plat in $plats) {
    foreach ($arch in $archs) {
        <# Unfortunately there’s no temporary directory function in PowerShell #>
        $unzipDest = mktemp -d
        Write-Debug "`$unzipDest = $unzipDest"

        $dlDest = New-TemporaryFile
        Write-Debug "`$dlDest = $dlDest"

        $uri = "https://github.com/PowerShell/PowerShell/releases/download/v$Version/powershell-$Version-$plat-$arch.tar.gz"
        Write-Debug "`$uri = $uri"
        Invoke-WebRequest -OutFile $dlDest -Uri $uri
        Write-Debug "Downloaded"

        tar -xzf $dlDest -C $unzipDest
        Write-Debug "Unzipped"

        $Host.UI.WriteErrorLine("$plat $arch`:")
        Write-Output (nix hash path $unzipDest)

        Remove-Item $dlDest
        Remove-Item $unzipDest -Recurse
    }
}
