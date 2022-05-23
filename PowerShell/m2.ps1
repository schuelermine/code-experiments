function m2 {
    param(
        [Object[]]$ls, <# left list of objects #>
        [Object[]]$rs, <# right list of objects #>
        [ScriptBlock]$ql, <# left condition #>
        [ScriptBlock]$qr, <# right condition #>
        [ScriptBlock]$fl, <# left transformation #>
        [ScriptBlock]$fr, <# right transformation #>
        [ScriptBlock]$m, <# action #>
        [Boolean]$d = $false <# debug #>
    )
    if ($d) {
        $m = {
            param(
                [String]$x,
                [String]$y
            )
            return "$x $y"
        }
    }
    foreach ($x in $ls) {
        if (&$ql $x) {
            &$m $x (&$fl $x)
        }
    }
    foreach ($x in $rs) {
        if (&$qr $x) {
            &$m $x (&$fr $x)
        }
    }
}

function mv {
    param(
        [String]$f,
        [String]$t
    )
    Move-Item -Force $f $t
}

function td {
    param([Object]$n)
    return "/home/anselmschueler/Downloads/$($n.Name)"
}

function tw {
    param([Object]$n)
    return "/media/external-data/Pictures/Wikimedia/$($n.Name)"
}

function nqw {
    param([Object]$f)
    $en = [System.Web.HttpUtility]::UrlEncode($f.name)
    $url = "https://commons.wikimedia.org/w/api.php?action=query&format=json&prop=imageinfo&titles=File:$en"
    $R = Invoke-WebRequest $url | % Content | ConvertFrom-Json
    return [bool]($R.query.pages.PSObject.Properties.name -match "-1")
}

function qw {
    return !(nqw $args)
}

m2 (gci /home/anselmschueler/Downloads) (gci /media/external-data/Pictures/Wikimedia) $Function:qw $Function:nqw $Function:tw $Function:td $Function:mv $args[0]
