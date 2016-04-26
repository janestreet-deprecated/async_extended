#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"async_extended"
  [ oasis_lib "async_extended"
  ; file "META" ~section:"lib"
  ]
