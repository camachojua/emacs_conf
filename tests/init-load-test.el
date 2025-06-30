(require 'ert)

(ert-deftest init-load-test ()
  "Load init.el without errors."
  (load-file "init.el"))
