#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"jenga-rules"
  [ oasis_lib "jenga_rules"
  ; file "META" ~section:"lib"
  ; tree "scripts" ~dest:"scripts" ~section:"libexec"
  ]
