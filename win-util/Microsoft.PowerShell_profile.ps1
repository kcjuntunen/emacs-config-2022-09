function View-Csv {
    <#
.SYNOPSIS
View a CSV file using Out-GridView.
.DESCRIPTION
Import a CSV, and pipe to Out-GridView, only shorter.
#>
    Param(
        [Parameter(Mandatory=$true)]
        [string]$FileName
    )
    Process {
        Import-Csv $FileName | Out-GridView
    }
}


function Check-Logs {
    <#
.SYNOPSIS
Dump the last $Days days of files in $Path.
.DESCRIPTION
Since I like to check the Enthus Logs every day, I made a script to
do so in as little as one Cmdlet.
#>
    Param(
        [string]$Path = "\\192.168.250.103\Z-drive\Enthus\Logs\Theisen",
        [int]$Days = 1,
        [string]$Filter = "*"
    )
    Process {
        $date = (Get-Date).AddDays(-$Days)
        #Get-ChildItem $Path | Where-Object LastWriteTime -gt $date | Sort-Object -Descending LastWriteTime
        (Get-ChildItem $Path -Filter $Filter).Where({$_.LastWriteTime -gt $date}) | Sort-Object -Descending LastWriteTime
    }
}

function Get-LastInventoryUpdate {
    <#
.SYNOPSIS
Dump the last $Days days of inventory updates of $Pattern in $Path.
.DESCRIPTION
This helps to conveniently search recent inventory updates for particular item updates.
#>
    Param(
        [Parameter(Mandatory=$true)]
        [string]$Pattern,
        [string]$Path = "\\192.168.250.103\Z-drive\Enthus\Logs\Theisen",
        [int]$Days = 1
    )
    Process {
        $item = Check-Logs -Filter DropShipperInventory* -Days $Days | Select-String -Pattern $Pattern | Select Path | Get-ChildItem
        foreach ($i in $item)
        {
            Write-Host("---------")
            Write-Host($i.FullName)
            Write-Host($i.LastWriteTime)
            #Write-Host(Get-Content $i | Select-String $Pattern)
            Import-Csv $i | Where-Object productid -eq $Pattern
        }
    }
}

function Get-LastProductUpdate {
    <#
.SYNOPSIS
Dump the last $Days days of inventory updates of $Pattern in $Path.
.DESCRIPTION
This helps to conveniently search recent inventory updates for particular item updates.
#>
    Param(
        [Parameter(Mandatory=$true)]
        [string]$Pattern,
        [string]$Path = "\\192.168.250.103\Z-drive\Enthus\Logs\Theisen",
        [int]$Days = 1
    )
    Process {
        $item = Check-Logs -Filter Products* -Days $Days | Select-String -Pattern $Pattern | Select Path | Get-ChildItem
        foreach ($i in $item)
        {
            Write-Host("---------")
            Write-Host($i.FullName)
            Write-Host($i.LastWriteTime)
            #Write-Host(Get-Content $i | Select-String $Pattern)
            Import-Csv $i | Where-Object productid -eq $Pattern
        }
    }
}

function Generate-Password() {
		Add-Type -AssemblyName System.Web
		[System.Web.Security.Membership]::GeneratePassword(15,2)
}

function Get-ServerTimes {
	ForEach ($server in $servers) {
	    $time = ([WMI]'').ConvertToDateTime((gwmi win32_operatingsystem -computername $server).LocalDateTime)
	    $server + '  ' + $time
	}
}

function Kill-ArkadyBase {
	Get-Process | where {$_.ProcessName -match "^(York|Arkad).*"} |kill
}

#  Bash-like prompt
$global:CurrentUser = [System.Security.Principal.WindowsIdentity]::GetCurrent()
function prompt
{
    $splitName = $CurrentUser.Name.Split("\\")
    $machineName = $env:COMPUTERNAME.ToLower()
    $shortName = $splitName[1].ToLower()
    #$host.ui.rawui.WindowTitle = $CurrentUser.Name + " " + $Host.Name + " " + $Host.Version + " Line: " + $host.UI.RawUI.CursorPosition.Y
    $host.ui.rawui.WindowTitle = $CurrentUser.Name + "@${machineName}"

    $splitPwd = $(Get-Location).Path.Split("\\")
    $pwdStr = $splitPwd[$splitPwd.Length - 1]

    if ($pwdStr -eq $shortName) { $pwdStr = "~" }
    if ($pwdStr -eq "") { $pwdStr = "/" }

    Write-Host ($shortName + "@" + $machineName) -nonewline -foregroundcolor Green
    Write-Host ( " " ) -nonewline
    Write-Host ($pwdStr + " ") -nonewline -foregroundcolor Cyan
    Write-Host "$" -nonewline
    return " "
}

Set-PSReadlineKeyHandler -Key Tab -Function Complete

$servers = 'server1', 'server2'

$workarea = "C:\fastrack\workarea\"
