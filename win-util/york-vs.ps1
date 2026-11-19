<#
.SYNOPSIS
    Drive a running Visual Studio from Emacs through its automation (DTE) API.

.DESCRIPTION
    Finds the Visual Studio instance that has -Solution open, starting one
    if needed, and then:

      breakpoint   toggle a breakpoint at -File/-Line
      breakpoints  list breakpoint lines in -File
      debug        set -Project as the startup project and start (or
                   continue) debugging, like F5
      debugtest    debug the test method at -File/-Line
      goto         show -File at -Line in Visual Studio

    breakpoint and breakpoints print the breakpoint lines in -File, one
    per line.  Errors go to stderr with a non-zero exit code.

.EXAMPLE
    powershell -NoProfile -File york-vs.ps1 -Action breakpoint -Devenv C:\...\devenv.exe -Solution C:\src\A.sln -File C:\src\A\B.cs -Line 42
#>
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('breakpoint', 'breakpoints', 'debug', 'debugtest', 'goto')]
    [string]$Action,
    [Parameter(Mandatory = $true)][string]$Devenv,
    [Parameter(Mandatory = $true)][string]$Solution,
    [string]$Project,
    [string]$File,
    [int]$Line,
    [int]$StartTimeout = 180
)

$ErrorActionPreference = 'Stop'

Add-Type -TypeDefinition @'
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

public static class YorkVsRot {
    [DllImport("ole32.dll")]
    static extern int GetRunningObjectTable(int reserved, out IRunningObjectTable rot);
    [DllImport("ole32.dll")]
    static extern int CreateBindCtx(int reserved, out IBindCtx ctx);

    // Visual Studio registers each open solution under its path.  Looking
    // it up by name never calls into other (possibly busy) VS instances.
    public static object Solution(string path) {
        string wanted = Path.GetFullPath(path);
        IRunningObjectTable rot;
        if (GetRunningObjectTable(0, out rot) != 0) return null;
        IEnumMoniker monikers;
        rot.EnumRunning(out monikers);
        var one = new IMoniker[1];
        while (monikers.Next(1, one, IntPtr.Zero) == 0) {
            IBindCtx ctx;
            CreateBindCtx(0, out ctx);
            string name;
            one[0].GetDisplayName(ctx, null, out name);
            if (string.Equals(name, wanted, StringComparison.OrdinalIgnoreCase)) {
                object solution;
                rot.GetObject(one[0], out solution);
                return solution;
            }
        }
        return null;
    }
}
'@

function Invoke-Vs([scriptblock]$Call) {
    # VS rejects COM calls while it is busy (loading, building); retry those.
    # PowerShell wraps COM errors, so look through the inner exceptions.
    for ($try = 0; ; $try++) {
        try {
            return & $Call
        } catch {
            $busy = $false
            for ($e = $_.Exception; $e; $e = $e.InnerException) {
                # RPC_E_CALL_REJECTED, RPC_E_SERVERCALL_RETRYLATER
                if ($e.HResult -in @(-2147418111, -2147417846)) { $busy = $true }
            }
            if (-not $busy -or $try -ge 120) { throw }
            Start-Sleep -Milliseconds 500
        }
    }
}

function Test-SamePath([string]$A, [string]$B) {
    if (-not $A -or -not $B) { return $false }
    return [IO.Path]::GetFullPath($A).TrimEnd('\') -ieq [IO.Path]::GetFullPath($B).TrimEnd('\')
}

function Find-Vs {
    $sln = [YorkVsRot]::Solution($Solution)
    if ($sln) { return Invoke-Vs { $sln.DTE } }
    return $null
}

function Get-Vs {
    $dte = Find-Vs
    if ($dte) { return $dte }
    Start-Process -FilePath $Devenv -ArgumentList "`"$Solution`""
    $deadline = (Get-Date).AddSeconds($StartTimeout)
    while ((Get-Date) -lt $deadline) {
        Start-Sleep -Seconds 1
        $dte = Find-Vs
        if ($dte) {
            # Wait until the projects have loaded.
            while ((Get-Date) -lt $deadline) {
                try { if ($dte.Solution.Projects.Count -gt 0) { return $dte } } catch { }
                Start-Sleep -Seconds 1
            }
        }
    }
    throw "Visual Studio didn't open $Solution within $StartTimeout seconds."
}

function Get-BreakpointLines($dte) {
    Invoke-Vs { @($dte.Debugger.Breakpoints) } |
        Where-Object { Test-SamePath $_.File $File } |
        ForEach-Object { $_.FileLine } |
        Sort-Object -Unique
}

function Get-ComProperty($Object, [string]$Name) {
    # PowerShell can't see the properties of VS 18's project objects, so
    # read them through late binding.
    Invoke-Vs { [System.__ComObject].InvokeMember($Name,
            [Reflection.BindingFlags]::GetProperty, $null, $Object, $null) }
}

function Find-Project($Projects) {
    # Return the UniqueName of the project whose file is $Project.
    $count = Invoke-Vs { $Projects.Count }
    for ($i = 1; $i -le $count; $i++) {
        $p = Invoke-Vs { $Projects.Item($i) }
        if ((Get-ComProperty $p 'Kind') -eq '{66A26720-8FB5-11D2-AA7E-00C04F688DDE}') {
            # A solution folder: its projects hang off its items.
            $items = Get-ComProperty $p 'ProjectItems'
            $n = Get-ComProperty $items 'Count'
            for ($j = 1; $j -le $n; $j++) {
                $sub = Get-ComProperty (Invoke-Vs { $items.Item($j) }) 'SubProject'
                if ($sub -and (Test-SamePath (Get-ComProperty $sub 'FullName') $Project)) {
                    return Get-ComProperty $sub 'UniqueName'
                }
            }
        } elseif (Test-SamePath (Get-ComProperty $p 'FullName') $Project) {
            return Get-ComProperty $p 'UniqueName'
        }
    }
    return $null
}

try {
    switch ($Action) {
        'breakpoints' {
            $dte = Find-Vs
            if ($dte) { Get-BreakpointLines $dte }
        }
        'breakpoint' {
            $dte = Get-Vs
            $here = @(Invoke-Vs { @($dte.Debugger.Breakpoints) } |
                    Where-Object { (Test-SamePath $_.File $File) -and $_.FileLine -eq $Line })
            if ($here.Count -gt 0) {
                foreach ($bp in $here) { Invoke-Vs { $bp.Delete() } | Out-Null }
            } else {
                Invoke-Vs { $dte.Debugger.Breakpoints.Add('', $File, $Line) } | Out-Null
            }
            Get-BreakpointLines $dte
        }
        'debug' {
            $dte = Get-Vs
            # 1 = design (not debugging), 2 = break mode, 3 = running.
            $mode = Invoke-Vs { $dte.Debugger.CurrentMode }
            if ($Project -and $mode -eq 1) {
                $name = Find-Project (Invoke-Vs { $dte.Solution.Projects })
                if (-not $name) { throw "$Project isn't in $Solution." }
                Invoke-Vs { $dte.Solution.SolutionBuild.StartupProjects = $name }
            }
            Invoke-Vs { $dte.MainWindow.Activate() }
            # F5's command: starts debugging, or continues when paused.
            # (Debugger.Go blocks the caller until the session ends.)
            if ($mode -ne 3) {
                Invoke-Vs { $dte.ExecuteCommand('Debug.Start') }
            }
        }
        { $_ -in 'goto', 'debugtest' } {
            $dte = Get-Vs
            Invoke-Vs { $dte.ItemOperations.OpenFile($File) } | Out-Null
            Invoke-Vs { $dte.ActiveDocument.Selection.GotoLine($Line, $false) }
            Invoke-Vs { $dte.MainWindow.Activate() }
            if ($Action -eq 'debugtest') {
                # Test Explorer's "Debug Tests" for the method at the cursor (Ctrl+R, Ctrl+T).
                Invoke-Vs { $dte.ExecuteCommand('TestExplorer.DebugAllTestsInContext') }
            }
        }
    }
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    exit 1
}
