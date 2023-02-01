(lang dune 3.6)

(name gradescope_submit)

(generate_opam_files true)

(source
 (github nmittu/gradescope-submit))

(authors "Nikhil Mittu")

(maintainers "nick@mittudev.com")

(license MIT)

(documentation https://github.com/nmittu/gradescope-submit)

(package
 (name gradescope_submit)
 (synopsis "A small script to submit to Gradescope via GitHub")
 (description "Submits the current git repository to Gradescope")
 (depends (ocaml (>="4.12")) dune (core (>="v0.15.1")) (core_unix (>="v0.15.2")) (cohttp (>="5.0.0")) (cohttp-lwt-unix (>="5.0.0")) (lambdasoup (>="0.7.3")) (toml (>="7.1.0")) (yojson (>="2.0.2")))
 (tags
  (topics git gradescope https api)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
