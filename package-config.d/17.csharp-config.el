;;; 17.csharp-config.el --- .NET stuff in Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: tools, lisp, c

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is all stolen from <https://www.emacs.dyerdwelling.family/emacs/20251216082551-emacs--setting-up-emacs-for-c%23-development-on-windows/>.

;;; Code:

(require 'eglot)

(setq eglot-server-programs '((csharp-mode . ("csharp-ls-vs"))))

(setq eglot-ignored-server-capabilities
      '(
        ;; :hoverProvider                    ; Documentation on hover
        ;; :completionProvider               ; Code completion
        ;; :signatureHelpProvider            ; Function signature help
        ;; :definitionProvider               ; Go to definition
        ;; :typeDefinitionProvider           ; Go to type definition
        ;; :implementationProvider           ; Go to implementation
        ;; :declarationProvider              ; Go to declaration
        ;; :referencesProvider               ; Find references
        ;; :documentHighlightProvider        ; Highlight symbols automatically
        ;; :documentSymbolProvider           ; List symbols in buffer
        ;; :workspaceSymbolProvider          ; List symbols in workspace
        ;; :codeActionProvider               ; Execute code actions
        ;; :codeLensProvider                 ; Code lens
        ;; :documentFormattingProvider       ; Format buffer
        ;; :documentRangeFormattingProvider  ; Format portion of buffer
        ;; :documentOnTypeFormattingProvider ; On-type formatting
        ;; :renameProvider                   ; Rename symbol
        ;; :documentLinkProvider             ; Highlight links in document
        ;; :colorProvider                    ; Decorate color references
        ;; :foldingRangeProvider             ; Fold regions of buffer
        ;; :executeCommandProvider           ; Execute custom commands
        ;; :inlayHintProvider                ; Inlay hints
        ))

(use-package dape
  ;; :load-path "z:/SharedVM/source/dape-master"
  :init
  ;; Set key prefix BEFORE loading dape
  (setq dape-key-prefix (kbd "C-c d"))
  :config
  ;; Define common configuration
  (defvar project-netcoredbg-path "C:/Users/k.c.juntunen/opt/netcoredbg/netcoredbg.exe"
    "Path to netcoredbg executable.")
  (defvar project-netcoredbg-log "c:/fastrack/woarkarea/netcoredbg.log"
    "Path to netcoredbg log file.")
  (defvar project-project-root "c:/fastrack/woarkarea"
    "Root directory of PROJECT project.")
  (defvar project-build-config "Debug"
    "Build configuration (Debug or Release).")
  (defvar project-target-arch "x64"
    "Target architecture (x64, x86, or AnyCPU).")

  ;; Helper function to create component configs
  (defun project-dape-config (component-name dll-name &optional stop-at-entry)
    "Create a dape configuration for a component.
COMPONENT-NAME is the component directory name
DLL-NAME is the DLL filename without extension.
STOP-AT-ENTRY if non-nil, stops at program entry point."
    (let* ((component-dir (format "%s/%s" project-project-root component-name))
           (bin-path (format "%s/bin/%s/%s/net9.0"
                             component-dir
                             project-target-arch
                             project-build-config))
           (dll-path (format "%s/%s.dll" bin-path dll-name))
           (config-name (intern (format "netcoredbg-launch-%s" 
                                        (downcase component-name)))))
      `(,config-name
        modes (csharp-mode csharp-ts-mode)
        command ,project-netcoredbg-path
        command-args (,(format "--interpreter=vscode")
                      ,(format "--engineLogging=%s" project-netcoredbg-log))
        normalize-path-separator 'windows
        :type "coreclr"
        :request "launch"
        :program ,dll-path
        :cwd ,component-dir
        :console "externalTerminal"
        :internalConsoleOptions "neverOpen"
        :suppressJITOptimizations t
        :requireExactSource nil
        :justMyCode t
        :stopAtEntry ,(if stop-at-entry t :json-false))))

  ;; Register all component configurations
  (dolist (config (list
                   (project-dape-config "DM" "DM.MSS" t)
                   (project-dape-config "Demo" "Demo.MSS" t)
                   (project-dape-config "Test_001" "Test" t)))
    (add-to-list 'dape-configs config))
  
  ;; Set buffer arrangement and other options
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-debug t)
  (setq dape-repl-echo-shell-output t))

(use-package corfu
	:ensure t
  :custom
  (corfu-auto nil)         ; Manual completion trigger
  (corfu-cycle t)          ; Cycle through candidates
  (corfu-preselect 'first))

(use-package ztree
	:ensure t
  :config
  (setq ztree-diff-filter-list
        '("build" "\\.dll" "\\.git" "bin" "obj"))
  (global-set-key (kbd "C-c z d") 'ztree-diff))

(use-package web-mode
	:ensure t
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))

;;; build script:

;; set PROJECTS
;; set PROJECT_NAMES

;; @echo off
;; setlocal

;; REM =================================================================
;; REM Build Management Script
;; REM =================================================================
;; REM Usage: build-selected.bat [action] [verbosity] [configuration] [platform]
;; REM   action: build, clean, restore, rebuild (default: build)
;; REM   verbosity: quiet, minimal, normal, detailed, diagnostic (default: minimal)
;; REM   configuration: Debug, Release (default: Debug)
;; REM   platform: x64, x86, "Any CPU" (default: x64)
;; REM =================================================================

;; REM Set defaults
;; set ACTION=%1
;; set VERBOSITY=%2
;; set CONFIGURATION=%3
;; set PLATFORM=%4

;; if "%ACTION%"=="" set ACTION=build
;; if "%VERBOSITY%"=="" set VERBOSITY=minimal
;; if "%CONFIGURATION%"=="" set CONFIGURATION=Debug
;; if "%PLATFORM%"=="" set PLATFORM=x64

;; echo Build Script - Action=%ACTION%, Verbosity=%VERBOSITY%, Config=%CONFIGURATION%, Platform=%PLATFORM%
;; echo.

;; REM Common build parameters
;; set BUILD_PARAMS=/p:Configuration=%CONFIGURATION% /p:Platform="%PLATFORM%" /verbosity:%VERBOSITY%

;; REM Set MSBuild target based on action
;; if /I "%ACTION%"=="build" set TARGET=Build
;; if /I "%ACTION%"=="clean" set TARGET=Clean
;; if /I "%ACTION%"=="restore" set TARGET=Restore
;; if /I "%ACTION%"=="rebuild" set TARGET=Rebuild

;; if "%TARGET%"=="" (
;;     echo Error: Invalid action '%ACTION%'. Use: build, clean, restore, or rebuild
;;     exit /b 1
;; )

;; echo Executing %ACTION% action...
;; echo.

;; set PROJECTS[1]=Demo/Demo.csproj
;; set PROJECT_NAMES[1]=Demo

;; set PROJECTS[2]=Test/Test.csproj
;; set PROJECT_NAMES[2]=Test

;; set PROJECT_COUNT=2

;; REM Special handling for rebuild (clean then build)
;; if /I "%ACTION%"=="rebuild" (
;;     echo === CLEANING PHASE ===
;;     for /L %%i in (1,1,%PROJECT_COUNT%) do (
;;         call :process_project %%i Clean
;;         if errorlevel 1 goto :error
;;     )
;;     echo.
;;     echo === BUILDING PHASE ===
;;     set TARGET=Build
;; )

;; REM Process all active projects
;; for /L %%i in (1,1,%PROJECT_COUNT%) do (
;;     call :process_project %%i %TARGET%
;;     if errorlevel 1 goto :error
;; )

;; echo.
;; if /I "%ACTION%"=="clean" (
;;     echo All selected components cleaned successfully!
;; ) else if /I "%ACTION%"=="restore" (
;;     echo All selected components restored successfully!
;; ) else if /I "%ACTION%"=="rebuild" (
;;     echo All selected components rebuilt successfully!
;; ) else (
;;     echo All selected components built successfully!
;; )
;; goto :end

;; :process_project
;;     setlocal EnableDelayedExpansion
;;     set idx=%1
;;     set target=%2
    
;;     REM Get project path and name using the index
;;     for /f "tokens=2 delims==" %%a in ('set PROJECTS[%idx%] 2^>nul') do set PROJECT_PATH=%%a
;;     for /f "tokens=2 delims==" %%a in ('set PROJECT_NAMES[%idx%] 2^>nul') do set PROJECT_NAME=%%a
    
;;     if "!PROJECT_PATH!"=="" goto :eof

;;     echo ----------------------------------------
;;     echo [%idx%/%PROJECT_COUNT%] %target%ing !PROJECT_NAME!...

;;     REM Build the project normally
;;     msbuild "!PROJECT_PATH!" /t:%target% %BUILD_PARAMS%
;;     if errorlevel 1 exit /b 1
    
;; goto :eof

;; :error
;; echo.
;; echo %ACTION% failed! Check the output above for errors.
;; exit /b 1

;; :end
;; echo %ACTION% completed at %time%

(provide '17.csharp-config)
;;; 17.csharp-config.el ends here
