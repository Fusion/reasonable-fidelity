(lang dune 2.1)
(name reasonable-fidelity)

(generate_opam_files true)

(license Apache)
(maintainers "Chris F Ravenscroft <chris@voilaweb.com>")
(authors "Chris F Ravenscroft <chris@voilaweb.com>")
(source (github fusion/reasonable-fidelity))

(package
  (name reasonable-fidelity)
  (depends
    (ocaml (= 4.09))
    (cohttp (= 2.5.0))
    (cohttp-lwt-unix (= 2.5.0))
    (lwt (= 5.1.1))
    (toml (= 5.0.0))
    (yojson (= 1.7.0))
  )
  (synopsis "Check fidelity and repeatability of online tests")
)

(package
  (name reasonable-fidelity-test)
  (depends
    rely.lib
    reasonable-fidelity.lib
  )
  (synopsis "Check fidelity and repeatability of online tests")
)

