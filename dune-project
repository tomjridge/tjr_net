(lang dune 2.0)
(name tjr_net)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_net))
(homepage             https://github.com/tomjridge/tjr_net)
(documentation        https://tomjridge.github.io/tjr_net)
(generate_opam_files  true)

(package 
 (name tjr_net)
 (synopsis "Basic networking utilities")
 (depends lwt))