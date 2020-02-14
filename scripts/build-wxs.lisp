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

(defun directory-tree (pn file-base-id dir-base-id)
  (let ((files (directory (merge-pathnames "*.*" pn)))
        (dirs (directory (merge-pathnames "*/" pn)))
        (out nil))
    (dolist (file files)
      (unless (or (uiop:directory-pathname-p file)
                  (equal "fasl" (pathname-type file)))
        (let ((enough (enough-namestring file pn)))
          (push `(("File" "Name" ,enough
                          "Id" ,(uiop:strcat file-base-id "." enough)
                          "Source" ,(enough-namestring file *build-root-pathname*)))
                out))))
    (dolist (dir dirs)
      (let ((name (car (last (pathname-directory dir)))))
        (push `(("Directory" "Name" ,name
                             "Id" ,(uiop:strcat dir-base-id "." name))
                ,@(directory-tree dir (uiop:strcat file-base-id "." name)
                                  (uiop:strcat dir-base-id "." name)))
              out)))
    out))

(defun build-wxs ()
  `(("Wix" "xmlns" "http://schemas.microsoft.com/wix/2006/wi")
    (("Product" "Id" "*"
                "Name" "Common Lisp Package Manager (CLPM)"
                "Version" "0.2.0"
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
                    "Nmae" "PFiles")
       (("Directory" "Id" "BaseFolder"
                     "Name" "CLPM")
        (("Directory" "Id" "VersionFolder"
                      "Name" "0.2.0")
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
            (("CreateFolder")))
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
           (("Component" "Id" "LibFiles"
                         "Guid" "FABDC0CC-CFE3-479D-914D-4EB739D8C513")
            ,@(directory-tree (merge-pathnames "lib/" *build-root-pathname*)
                              "File_lib" "Directory_lib"))))))))

     (("Feature" "Id" "Minimal"
                 "Title" "CLPM Executable"
                 "ConfigurableDirectory" "INSTALLDIR"
                 "Level" "1")
      (("ComponentRef" "Id" "CLPM_Bin"))
      (("ComponentRef" "id" "CLPM_Libs"))
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
     (("UIRef" "Id" "WixUI_FeatureTree")))))

(defun write-wxs ()
  (with-open-file (s (merge-pathnames "clpm.wxs" *build-root-pathname*)
                   :if-exists :supersede :direction :output)
    (format s "<?xml version='1.0'?>~%")
    (s-xml:print-xml (build-wxs) :stream s :pretty t)))

(print (build-wxs))
(write-wxs)
