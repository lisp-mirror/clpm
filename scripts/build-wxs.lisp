;;;; Builds a WiX XML file for building an MSI installer
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(load (merge-pathnames "../logical-pathname.lisp" *load-truename*))

(setup-asdf "default")

(asdf:load-system :s-xml)
(asdf:load-system :uuid)

(defun gen-uuid ()
  (format nil "~A" (uuid:make-v4-uuid)))

(defun sanitize-id (id)
  (substitute #\_ #\- id))

(defun directory-tree (pn component-base-id file-base-id dir-base-id)
  (let* ((files (directory (merge-pathnames "*.*" pn)))
         (dirs (directory (merge-pathnames "*/" pn)))
         (dir-name (car (last (pathname-directory pn))))
         (dir-id (uiop:strcat dir-base-id "." dir-name))
         (out `(("Directory" "Name" ,dir-name "Id" ,(sanitize-id dir-id))))
         (component-name (sanitize-id (uiop:strcat component-base-id "_" dir-name)))
         (component-names (list component-name))
         (component (list (list (list "CreateFolder"))
                          (list "Component"
                                "Id" component-name
                                "Guid" (gen-uuid)
                                "DiskId" "1"
                                "Win64" "yes"))))
    (dolist (file files)
      (unless (or (uiop:directory-pathname-p file)
                  (equal "fasl" (pathname-type file)))
        (let ((enough (enough-namestring file pn)))
          (push `(("File" "Name" ,enough
                          "Id" ,(sanitize-id (uiop:strcat file-base-id "." dir-name "." enough))
                          "Source" ,(enough-namestring file *build-root-pathname*)))
                component))))
    (push (nreverse component) out)
    (dolist (dir dirs)
      (multiple-value-bind (subdir-tree sub-component-names)
          (directory-tree dir component-name (uiop:strcat file-base-id "." dir-name) dir-id)
        (setf component-names (append sub-component-names component-names))
        (push subdir-tree out)))
    (values (nreverse out) component-names)))

(defun directory-files (pn)
  (let ((files (directory (merge-pathnames "*.*" pn)))
        (out))
    (dolist (file files)
      (unless (or (uiop:directory-pathname-p file)
                  (equal "fasl" (pathname-type file)))
        (push `(("File" "Name" ,(enough-namestring file pn)
                        "Source" ,(enough-namestring file *build-root-pathname*)))
              out)))
    out))

(defun build-wxs ()
  (multiple-value-bind (src-dir-tree src-dir-component-names)
      (directory-tree (merge-pathnames "lib/clpm/src/clpm/" *build-root-pathname*)
                      "CLPM_Src" "CLPM_SrcFile" "CLPM_SrcDir")
    `(("Wix" "xmlns" "http://schemas.microsoft.com/wix/2006/wi")
      (("Product" "Id" "*"
                  "Name" "Common Lisp Package Manager (CLPM)"
                  "Version" "0.2.1"
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
          (("Directory" "Id" "VersionFolder"
                        "Name" "v0.2.1")
           (("Directory" "Id" "INSTALLDIR")
            (("Directory" "Id" "BINDIR"
                          "Name" "bin")
             (("Component" "Id" "CLPM_SetPATH"
                           "Guid" "5514D118-8FAD-497C-ACFB-BAC6C0161E69"
                           "DiskId" "1"
                           "Win64" "yes")
              (("CreateFolder"))
              (("Environment" "Id" "Env_PATH"
                              "System" "yes"
                              "Action" "set"
                              "Name" "PATH"
                              "Part" "last"
                              "Value" "[BINDIR]")))
             (("Component" "Id" "CLPM_Bin"
                           "Guid" "E7F4D949-DEBC-4FF9-9D60-D455BC11F606"
                           "DiskId" "1"
                           "Win64" "yes")
              (("File" "Name" "clpm.exe"
                       "Source" "bin/clpm.exe"))))

            (("Directory" "Id" "LIBDIR"
                          "Name" "lib")
             (("Component" "Id" "CLPM_Libs"
                           "Guid" "35C91C7A-ED22-40B5-A761-730C57CCF803"
                           "DiskId" "1"
                           "Win64" "yes")
              (("CreateFolder"))
              ,@(directory-files (merge-pathnames "lib/clpm/" *build-root-pathname*)))
             (("Component" "Id" "CLPM_SetHOME"
                           "Guid" "E75E9C41-C6B9-44FE-90B3-8055B22341D0"
                           "DiskId" "1"
                           "Win64" "yes")
              (("CreateFolder"))
              (("Environment" "Id" "Env_CLPM_HOME"
                              "System" "yes"
                              "Action" "set"
                              "Name" "CLPM_HOME"
                              "Part" "all"
                              "Value" "[LIBDIR]")))
             (("Directory" "Id" "SrcDir"
                           "Name" "src")
              (("Directory" "Id" "ClientDir"
                            "Name" "clpm-client")
               (("Component" "Id" "CLPM_Client"
                             "Guid" "6B847710-C813-4E8E-861B-7C0B767F4C36"
                             "DiskId" "1"
                             "Win64" "yes")
                ,@(directory-files (merge-pathnames "lib/clpm/src/clpm-client/" *build-root-pathname*))))
              (("Directory" "Id" "GrovelerDir"
                            "Name" "clpm-groveler")
               (("Component" "Id" "CLPM_Groveler"
                             "Guid" "639DDC25-7C6D-4F7B-809E-A7989A3D15BE"
                             "DiskId" "1"
                             "Win64" "yes")
                ,@(directory-files (merge-pathnames "lib/clpm/src/clpm-groveler/" *build-root-pathname*))))
              ,src-dir-tree)))))))

       (("Feature" "Id" "Minimal"
                   "Title" "CLPM Executable"
                   "ConfigurableDirectory" "INSTALLDIR"
                   "Level" "1")
        (("ComponentRef" "Id" "CLPM_Bin"))
        (("ComponentRef" "Id" "CLPM_Libs"))
        (("ComponentRef" "Id" "CLPM_Client"))
        (("ComponentRef" "Id" "CLPM_Groveler"))
        ,@(mapcar (lambda (id) `(("ComponentRef" "Id" ,id))) src-dir-component-names)
        (("Feature" "Id" "SetPath"
                    "Level" "1"
                    "Title" "Set Environment Variable: PATH")
         (("ComponentRef" "Id" "CLPM_SetPATH")))
        (("Feature" "Id" "SetHome"
                    "Level" "1"
                    "Title" "Set Environment Variable: CLPM_HOME")
         (("ComponentRef" "Id" "CLPM_SetHOME"))))
       (("WixVariable" "Id" "WixUILicenseRtf"
                       "Value" "License.rtf"))
       (("Property" "Id" "WIXUI_INSTALLDIR"
                    "Value" "INSTALLDIR"))
       (("UIRef" "Id" "WixUI_FeatureTree"))))))

(defun write-wxs ()
  (with-open-file (s (merge-pathnames "clpm.wxs" *build-root-pathname*)
                   :if-exists :supersede :direction :output)
    (format s "<?xml version='1.0'?>~%")
    (s-xml:print-xml (build-wxs) :stream s :pretty t)))

(print (build-wxs))
(write-wxs)
