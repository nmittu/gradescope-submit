(lang dune 3.6)

(name gradescope_submit)

(generate_opam_files true)

(source
 (github nmittu/gradescope-submit))

(authors "Nikhil Mittu")

(maintainers "Nikhil Mittu")

(license MIT)

(documentation https://github.com/nmittu/gradescope-submit)

(package
 (name gradescope_submit)
 (synopsis "A small script to submit to Gradescope via GitHub")
 (description "Submits the current git repository to Gradescope")
 (depends ocaml dune core core_unix cohttp cohttp-lwt-unix lambdasoup toml yojson)
 (tags
  (topics git gradescope https api)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
