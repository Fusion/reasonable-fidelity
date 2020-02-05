(lang dune 2.1)
(name reasonablefidelity)

(generate_opam_files true)

(license Apache)
(maintainers "Chris F Ravenscroft <chris@voilaweb.com>")
(authors "Chris F Ravenscroft <chris@voilaweb.com>")
(source (github fusion/reasonable-fidelity))

(package
  (name reasonablefidelity)
  (depends
    (ocaml (>= 4.09))
    cohttp
    cohttp-lwt-unix
    curses
    lwt
    lymp
    str
    toml
    yojson
  )
  (conflicts
    reasonable-fidelity
  )
  (synopsis "Check fidelity and repeatability of online tests")
)

