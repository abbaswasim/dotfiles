;; init-auto-complete.el ; auto-complete settings
; start auto-complete with emacs
(require 'auto-complete)

; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun personal:ac-c-header-init ()
    (require 'auto-complete-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.2/include")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
    (add-to-list 'achead:include-directories '"/usr/include")
    (add-to-list 'achead:include-directories '"/System/Library/Frameworks")
    (add-to-list 'achead:include-directories '"/Library/Frameworks")
)

; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'personal:ac-c-header-init)
(add-hook 'c-mode-hook 'personal:ac-c-header-init)

; turn on Semantic, this mean autocomplete for user defined types
(semantic-mode 1)

; define a function which adds semantic as a suggestion backend to auto complete and hook this function to c-mode-common-hook
(defun personal:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
)

(add-hook 'c-mode-common-hook 'personal:add-semantic-to-autocomplete)
(add-hook 'c++-mode-common-hook 'personal:add-semantic-to-autocomplete)

(provide 'init-auto-complete)
