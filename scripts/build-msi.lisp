;;;; Builds a WiX XML file for building an MSI installer
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf)

(asdf:load-system :clpm-asdf)
(asdf:load-system :alexandria)
(asdf:load-system :s-xml)
(asdf:load-system :uuid)

(defun files-for-installer (o s)
  (let* ((staging-directory (asdf:component-pathname (asdf-release-ops::release-system-release-module o s)))
         (input-files (asdf:input-files o s)))
    (values (mapcar (lambda (pn)
                      (enough-namestring pn staging-directory))
                    input-files)
            staging-directory)))

(defun gen-uuid ()
  (format nil "~A" (uuid:make-v4-uuid)))

(defun sanitize-id (id)
  (substitute #\_ #\- id))

(defun gather-all-files (file-list dir-list component-base-id dir-name file-base-id dir-base-id)
  (let* ((direct-files (remove-if #'pathname-directory file-list))
         (dirs nil)
         (dir-id (uiop:strcat dir-base-id "." dir-name))
         (out `(,@(when (equal dir-name "bin")
                    `((("Component" "Id" "CLPM_SetPATH"
                                    "Guid" "5514D118-8FAD-497C-ACFB-BAC6C0161E69"
                                    "DiskId" "1"
                                    "Win64" "yes")
                       (("CreateFolder"))
                       (("Environment" "Id" "Env_PATH"
                                       "System" "yes"
                                       "Action" "set"
                                       "Name" "PATH"
                                       "Part" "last"
                                       "Value" "[BINDIR]")))))
                ("Directory" "Name" ,dir-name
                             "Id" ,(cond
                                    ((null dir-list)
                                     "INSTALLDIR")
                                    ((equal dir-name "bin")
                                     "BINDIR")
                                    (t
                                     (sanitize-id dir-id))))))
         (component-name (sanitize-id (uiop:strcat component-base-id "_" dir-name)))
         (component-names (list component-name))
         (component `((("CreateFolder"))
                      ("Component"
                       "Id" ,component-name
                       "Guid" ,(gen-uuid)
                       "DiskId" "1"
                       "Win64" "yes"))))
    (dolist (not-direct-file (set-difference file-list direct-files))
      (let ((first-dir (second (pathname-directory not-direct-file))))
        (push (subseq not-direct-file (1+ (length first-dir)))
              (alexandria:assoc-value dirs first-dir :test #'equal))))
    (dolist (file direct-files)
      (push `(("File" "Name" ,file
                      "Id" ,(sanitize-id (uiop:strcat file-base-id "." dir-name "." file))
                      "Source" ,(format nil "~{~A/~}~A" dir-list file)))
            component))
    (push (nreverse component) out)
    (dolist (dir dirs)
      (multiple-value-bind (subdir-tree sub-component-names)
          (gather-all-files (cdr dir) (append dir-list (list (car dir)))
                            component-name (car dir) (uiop:strcat file-base-id "." (car dir))
                            dir-name)
        (setf component-names (append sub-component-names component-names))
        (push subdir-tree out)))

    (values (nreverse out) component-names)))

(defun build-wxs (files)
  (multiple-value-bind (dirs components)
      (gather-all-files files nil "CLPM" (clpm-asdf::base-version) "CLPMFile" "CLPMDir")
    `(("Wix" "xmlns" "http://schemas.microsoft.com/wix/2006/wi")
      (("Product" "Id" "*"
                  "Name" "Common Lisp Package Manager (CLPM)"
                  ;; We can only use base version in WXS.
                  "Version" ,(clpm-asdf::base-version)
                  "Manufacturer" "https://www.clpm.dev"
                  "UpgradeCode" "D499FD17-A05C-41F8-837E-8B1BF4C4403B"
                  "Language" "1033")
       (("Package" "Id" "*"
                   "Manufacturer" "https://www.clpm.dev"
                   "InstallerVersion" "200"
                   "Compressed" "yes"
                   "Platform" "x64"
                   "InstallScope" "perMachine"))
       (("Media" "Id" "1"
                 "Cabinet" "clpm.cab"
                 "EmbedCab" "yes"))
       (("Property" "Id" "PREVIOUSVERSIONSINSTALLED"
                    "Secure" "yes"))
       (("Upgrade" "Id" "D499FD17-A05C-41F8-837E-8B1BF4C4403B")
        (("UpgradeVersion" "Minimum" "0.1.0"
                           "Maximum" "99.0.0"
                           "Property" "PREVIOUSVERSIONSINSTALLED"
                           "IncludeMinimum" "yes"
                           "IncludeMaximum" "no")))
       (("InstallExecuteSequence")
        (("RemoveExistingProducts" "After" "InstallInitialize")))

       (("Directory" "Id" "TARGETDIR"
                     "Name" "SourceDir")
        (("Directory" "Id" "ProgramFiles64Folder"
                      "Name" "PFiles")
         (("Directory" "Id" "BaseFolder"
                       "Name" "CLPM")
          ,dirs)))

       (("Feature" "Id" "Minimal"
                   "Title" "CLPM Executable"
                   "ConfigurableDirectory" "INSTALLDIR"
                   "Level" "1")
        ,@(mapcar (lambda (id) `(("ComponentRef" "Id" ,id))) components)
        (("Feature" "Id" "SetPath"
                    "Level" "1"
                    "Title" "Set Environment Variable: PATH")
         (("ComponentRef" "Id" "CLPM_SetPATH"))))
       (("WixVariable" "Id" "WixUILicenseRtf"
                       "Value" "License.rtf"))
       (("Property" "Id" "WIXUI_INSTALLDIR"
                    "Value" "INSTALLDIR"))
       (("UIRef" "Id" "WixUI_FeatureTree"))))))


(defun make-msi ()
  (multiple-value-bind (relative-files staging-directory)
      (files-for-installer (asdf:make-operation 'asdf-release-ops:dynamic-release-archive-op)
                           (asdf:find-system :clpm))
    (uiop:copy-file (asdf:system-relative-pathname :clpm-asdf "License.rtf")
                    (merge-pathnames "License.rtf" staging-directory))
    (with-open-file (s (merge-pathnames "clpm.wxs" staging-directory)
                       :if-exists :supersede :direction :output)
      (format s "<?xml version='1.0'?>~%")
      (s-xml:print-xml (build-wxs relative-files) :stream s :pretty t))

    (uiop:with-current-directory (staging-directory)
      (let* ((wix-base (uiop:parse-native-namestring (uiop:getenv "WIX") :ensure-directory t))
             (candle (uiop:native-namestring (merge-pathnames "bin/candle.exe" wix-base)))
             (light (uiop:native-namestring (merge-pathnames "bin/light.exe" wix-base)))
             (extension (uiop:native-namestring (merge-pathnames "bin/WixUIExtension.dll" wix-base)))
             (msi (asdf:system-relative-pathname :clpm-asdf
                                                 (uiop:strcat "releases/dynamic/"
                                                              "clpm-" (clpm-asdf::clpm-version)
                                                              "-windows-amd64.msi"))))
        (uiop:run-program `(,candle "clpm.wxs")
                          :output :interactive
                          :error-output :interactive)
        (uiop:run-program `(,light "clpm.wixobj" "-ext" ,extension "-cultures:en-us" "-out" "clpm.msi")
                          :output :interactive
                          :error-output :interactive)
        (uiop:copy-file (merge-pathnames "clpm.msi" staging-directory)
                        msi)))))

(make-msi)
