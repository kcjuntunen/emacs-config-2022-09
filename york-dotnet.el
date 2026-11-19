;;; york-dotnet.el --- Build, test, and debug York .NET projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: tools, languages

;;; Commentary:

;; Most York projects target .NET Framework, which netcoredbg (and so
;; dape) can't debug.  Emacs does the editing, building, and test runs;
;; Visual Studio does the debugging, driven from here through its
;; automation API by win-util/york-vs.ps1.
;;
;; C-c y d b   toggle a breakpoint in VS at point (shown in the fringe)
;; C-c y d B   refresh this buffer's breakpoints from VS
;; C-c y d d   start debugging the project at point in VS (F5)
;; C-c y d c   build the solution with MSBuild
;; C-c y d t   run the test at point (C-u: the whole class)
;; C-c y d a   run every test in the project at point
;; C-c y d T   debug the test at point in VS
;; C-c y d g   show point in VS
;;
;; VS is started on the solution if it doesn't have it open yet.
;; Library projects are started with `york-dotnet-test-suite', set in
;; the project's .csproj.user just as VS does it.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)

(defgroup york-dotnet nil
	"Build, test, and debug York .NET projects."
	:group 'york)

(defcustom york-dotnet-test-suite
	"C:/Users/k.c.juntunen/opt/amxts/York.ArkmonTestSuite.exe"
	"Program that library projects are debugged under."
	:type 'file)

(defcustom york-dotnet-configuration "Debug"
	"Build configuration for builds and test runs."
	:type 'string)


(defvar york-vs-script
	(expand-file-name "win-util/york-vs.ps1"
										(file-name-directory (or load-file-name buffer-file-name)))
	"PowerShell script that drives Visual Studio.")

(defvar york-vs--installation nil
	"Cached Visual Studio installation directory.")

(defun york-vs--installation ()
	"Return the newest Visual Studio installation directory."
	(or york-vs--installation
			(let* ((vswhere (expand-file-name "Microsoft Visual Studio/Installer/vswhere.exe"
																				(or (getenv "ProgramFiles(x86)") "C:/Program Files (x86)")))
						 (dir (and (file-executable-p vswhere)
											 (string-trim
												(with-output-to-string
													(call-process vswhere nil standard-output nil
																				"-latest" "-products" "*"
																				"-property" "installationPath"))))))
				(unless (and dir (file-directory-p dir))
					(user-error "Can't find Visual Studio"))
				(setq york-vs--installation (file-name-as-directory dir)))))

(defun york-vs--tool (relative)
	"Return the path to RELATIVE under the Visual Studio installation."
	(expand-file-name relative (york-vs--installation)))

(defun york-dotnet--native (file)
	"Return FILE as an absolute path with backslashes."
	(subst-char-in-string ?/ ?\\ (expand-file-name file)))

;;;; Finding the solution, project, and test

(defun york-dotnet--files-up (regexp)
	"Return files matching REGEXP in the nearest directory upward that has any."
	(when-let* ((dir (locate-dominating-file
										default-directory
										(lambda (d) (directory-files d nil regexp t)))))
		(directory-files dir t regexp)))

(defun york-dotnet--solution ()
	"Return the solution for the current buffer, asking if there are several."
	(let ((slns (york-dotnet--files-up "\\.slnx?\\'")))
		(cond ((null slns) (user-error "No solution found above %s" default-directory))
					((null (cdr slns)) (car slns))
					(t (completing-read "Solution: " slns nil t)))))

(defun york-dotnet--project ()
	"Return the project file for the current buffer."
	(or (car (york-dotnet--files-up "\\.csproj\\'"))
			(user-error "No project found above %s" default-directory)))

(defun york-dotnet--project-property (csproj property)
	"Return the first value of PROPERTY in CSPROJ, or nil."
	(with-temp-buffer
		(insert-file-contents csproj)
		(when (re-search-forward (format "<%s>\\([^<]*\\)</%s>" property property) nil t)
			(string-trim (match-string 1)))))

(defun york-dotnet--output-dll (csproj)
	"Return the DLL that CSPROJ builds in `york-dotnet-configuration'."
	(let ((name (or (york-dotnet--project-property csproj "AssemblyName")
									(file-name-base csproj)))
				(tfm (or (york-dotnet--project-property csproj "TargetFramework")
								 (when-let* ((tfms (york-dotnet--project-property csproj "TargetFrameworks")))
									 (car (split-string tfms ";" t))))))
		;; SDK-style projects add the target framework to the output path.
		(expand-file-name (concat name ".dll")
											(file-name-concat (file-name-directory csproj) "bin"
																				york-dotnet-configuration (or tfm "")))))

(defconst york-dotnet--test-attribute
	"^[ \t]*\\[\\(?:[A-Za-z.]+\\.\\)?\\(?:Data\\)?\\(?:TestMethod\\|Test\\|TestCase\\|Fact\\|Theory\\)\\_>"
	"Regexp for an MSTest, NUnit, or xUnit test-method attribute.")

(defun york-dotnet--test-at-point (&optional class-only)
	"Return (FILTER . LINE) for the test method at point.
FILTER is a vstest /TestCaseFilter value and LINE is the method's line.
With CLASS-ONLY, the filter selects every test in the class at point."
	(save-excursion
		(end-of-line)
		(let* ((namespace (save-excursion
												(goto-char (point-min))
												(and (re-search-forward
															"^[ \t]*namespace[ \t]+\\([A-Za-z_][A-Za-z0-9_.]*\\)" nil t)
														 (match-string-no-properties 1))))
					 (method-line nil)
					 (method
						(unless class-only
							(unless (re-search-backward york-dotnet--test-attribute nil t)
								(user-error "No test method at point"))
							;; Skip the attribute lines to reach the signature.
							(while (looking-at-p "[ \t]*\\[")
								(forward-line 1))
							(setq method-line (line-number-at-pos))
							(unless (re-search-forward "\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*\\(?:<[^>]*>\\)?[ \t]*("
																				 (line-end-position) t)
								(user-error "Can't read the test method's name"))
							(match-string-no-properties 1)))
					 (class (progn
										(unless (re-search-backward
														 "\\_<class[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
											(user-error "No class at point"))
										(match-string-no-properties 1)))
					 (full (string-join (delq nil (list namespace class method)) ".")))
			(cons (if method
								(concat "FullyQualifiedName=" full)
							(concat "FullyQualifiedName~" full "."))
						(or method-line (line-number-at-pos))))))

;;;; Building and running tests

(defun york-dotnet--msbuild-command (target &optional solution)
	"Return an MSBuild command line that builds TARGET.
SOLUTION, when given, sets SolutionDir for a project build."
	(mapconcat
	 #'shell-quote-argument
	 (append (list (york-vs--tool "MSBuild/Current/Bin/MSBuild.exe")
								 (expand-file-name target)
								 "-restore" "-p:RestorePackagesConfig=true"
								 (concat "-p:Configuration=" york-dotnet-configuration)
								 "-m" "-v:minimal" "-nologo")
					 (when solution
						 (list (concat "-p:SolutionDir="
													 (file-name-directory solution)))))
	 " "))

(defun york-dotnet-build ()
	"Build the solution for the current buffer with MSBuild."
	(interactive)
	(let ((solution (york-dotnet--solution)))
		(save-some-buffers (not compilation-ask-about-save))
		(let ((default-directory (file-name-directory solution)))
			(compilation-start (york-dotnet--msbuild-command solution)
												 nil (lambda (_) "*york-build*")))))

(defun york-dotnet--run-tests (filter)
	"Build the project at point and run its tests that match FILTER.
A nil FILTER runs every test in the project."
	(let* ((solution (york-dotnet--solution))
				 (csproj (york-dotnet--project))
				 (dll (york-dotnet--output-dll csproj))
				 (vstest (mapconcat
									#'shell-quote-argument
									(append (list (york-vs--tool
																 "Common7/IDE/Extensions/TestPlatform/vstest.console.exe")
																(expand-file-name dll))
													(when filter (list (concat "/TestCaseFilter:" filter))))
									" ")))
		(save-some-buffers (not compilation-ask-about-save))
		(let ((default-directory (file-name-directory csproj)))
			(compilation-start (concat (york-dotnet--msbuild-command csproj solution)
																 " && " vstest)
												 nil (lambda (_) "*york-test*")))))

(defun york-dotnet-test-at-point (&optional class-only)
	"Run the test method at point.  With CLASS-ONLY (\\[universal-argument]), run its class."
	(interactive "P")
	(york-dotnet--run-tests (car (york-dotnet--test-at-point class-only))))

(defun york-dotnet-test-project ()
	"Run every test in the project at point."
	(interactive)
	(york-dotnet--run-tests nil))

;;;; Talking to Visual Studio

(defun york-vs--call (action args &optional on-success)
	"Run york-vs.ps1 ACTION with ARGS in the background.
ARGS is a plist of script parameters, such as (:File F :Line 3).  When
the script succeeds, call ON-SUCCESS with its output lines."
	(let ((buf (generate-new-buffer " *york-vs*"))
				(params (cl-loop for (key value) on args by #'cddr
												 when value
												 append (list (concat "-" (substring (symbol-name key) 1))
																			(if (numberp value) (number-to-string value) value)))))
		(make-process
		 :name "york-vs"
		 :buffer buf
		 :noquery t
		 :command (append (list "powershell" "-NoProfile" "-NonInteractive"
														"-ExecutionPolicy" "Bypass"
														"-File" york-vs-script
														"-Action" action
														"-Devenv" (york-dotnet--native
																			 (york-vs--tool "Common7/IDE/devenv.exe")))
											params)
		 :sentinel
		 (lambda (proc _event)
			 (when (memq (process-status proc) '(exit signal))
				 (let ((output (with-current-buffer buf (string-trim (buffer-string)))))
					 (kill-buffer buf)
					 (if (zerop (process-exit-status proc))
							 (when on-success
								 (funcall on-success (split-string output "[\r\n]+" t)))
						 (message "Visual Studio: %s" output))))))))

(defun york-vs--file-args ()
	"Return the york-vs.ps1 arguments for the current solution and position."
	(unless buffer-file-name
		(user-error "This buffer isn't visiting a file"))
	(list :Solution (york-dotnet--native (york-dotnet--solution))
				:File (york-dotnet--native buffer-file-name)
				:Line (line-number-at-pos)))

;;;; Breakpoints

(defface york-vs-breakpoint '((t :inherit error))
	"Face for Visual Studio breakpoint marks in the fringe.")

(when (fboundp 'define-fringe-bitmap)
	(define-fringe-bitmap 'york-vs-breakpoint
		[#x3c #x7e #xff #xff #xff #xff #x7e #x3c]))

(defun york-vs--show-breakpoints (buffer lines)
	"Mark breakpoint LINES (strings of line numbers) in BUFFER's fringe."
	(when (buffer-live-p buffer)
		(with-current-buffer buffer
			(remove-overlays (point-min) (point-max) 'york-vs-breakpoint t)
			(save-excursion
				(dolist (line lines)
					(when (string-match-p "\\`[0-9]+\\'" line)
						(goto-char (point-min))
						(forward-line (1- (string-to-number line)))
						(let ((ov (make-overlay (point) (point))))
							(overlay-put ov 'york-vs-breakpoint t)
							(overlay-put ov 'before-string
													 (propertize "●" 'display
																			 '(left-fringe york-vs-breakpoint
																										 york-vs-breakpoint))))))))))

(defun york-vs-toggle-breakpoint ()
	"Toggle a Visual Studio breakpoint on the current line."
	(interactive)
	(let ((buffer (current-buffer)))
		(message "Toggling breakpoint in Visual Studio...")
		(york-vs--call "breakpoint" (york-vs--file-args)
									 (lambda (lines)
										 (york-vs--show-breakpoints buffer lines)
										 (message "Breakpoints: %s"
															(if lines (string-join lines ", ") "none"))))))

(defun york-vs-refresh-breakpoints ()
	"Show this buffer's Visual Studio breakpoints in the fringe."
	(interactive)
	(let ((buffer (current-buffer)))
		(york-vs--call "breakpoints" (york-vs--file-args)
									 (lambda (lines) (york-vs--show-breakpoints buffer lines)))))

;;;; Debugging

(defun york-dotnet--startable-p (csproj)
	"Return non-nil if VS can start CSPROJ: an executable or a start program."
	(let ((user (concat csproj ".user")))
		(or (member (york-dotnet--project-property csproj "OutputType") '("Exe" "WinExe"))
				(and (file-exists-p user)
						 (york-dotnet--project-property user "StartProgram")))))

(defun york-dotnet--add-start-program (csproj)
	"Make `york-dotnet-test-suite' the start program of library CSPROJ.
Asks first, and only creates a .csproj.user file; it never edits one."
	(let ((user (concat csproj ".user")))
		(cond
		 ((york-dotnet--startable-p csproj))
		 ((file-exists-p user)
			(message "%s has no StartProgram; set one in VS (Project > Properties > Debug)"
							 (file-name-nondirectory user)))
		 ((y-or-n-p (format "Debug %s under %s? "
												(file-name-base csproj)
												(file-name-nondirectory york-dotnet-test-suite)))
			(with-temp-file user
				(insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
								"<Project ToolsVersion=\"Current\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">\n"
								(format "  <PropertyGroup Condition=\"'$(Configuration)|$(Platform)' == '%s|AnyCPU'\">\n"
												york-dotnet-configuration)
								"    <StartAction>Program</StartAction>\n"
								(format "    <StartProgram>%s</StartProgram>\n"
												(york-dotnet--native york-dotnet-test-suite))
								"  </PropertyGroup>\n"
								"</Project>\n"))
			(message "Wrote %s; if VS already has the solution open, reload the project"
							 (file-name-nondirectory user))))))

(defun york-vs-debug ()
	"Start debugging the project at point in Visual Studio, like F5.
Sets it as the startup project.  If VS is paused, continues instead."
	(interactive)
	(let ((args (york-vs--file-args))
				(csproj (york-dotnet--project)))
		(save-some-buffers (not compilation-ask-about-save))
		(york-dotnet--add-start-program csproj)
		(message "Starting the debugger in Visual Studio...")
		(york-vs--call "debug" (append (list :Project (york-dotnet--native csproj)) args)
									 (lambda (_) (message "Debugging %s in Visual Studio"
																				(file-name-base csproj))))))

(defun york-vs-debug-test-at-point ()
	"Debug the test method at point in Visual Studio."
	(interactive)
	(let ((line (cdr (york-dotnet--test-at-point))))
		(save-some-buffers (not compilation-ask-about-save))
		(message "Debugging the test in Visual Studio...")
		(york-vs--call "debugtest" (plist-put (york-vs--file-args) :Line line))))

(defun york-vs-goto ()
	"Show the current file and line in Visual Studio."
	(interactive)
	(york-vs--call "goto" (york-vs--file-args)))

;;;; Keys

(defvar-keymap york-dotnet-map
	:doc "Build, test, and debug York .NET projects."
	"b" #'york-vs-toggle-breakpoint
	"B" #'york-vs-refresh-breakpoints
	"d" #'york-vs-debug
	"c" #'york-dotnet-build
	"t" #'york-dotnet-test-at-point
	"a" #'york-dotnet-test-project
	"T" #'york-vs-debug-test-at-point
	"g" #'york-vs-goto)

(global-set-key (kbd "C-c y d") york-dotnet-map)

(provide 'york-dotnet)
;;; york-dotnet.el ends here
