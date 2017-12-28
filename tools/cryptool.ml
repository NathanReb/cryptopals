let main_term =
  let open Cmdliner in
  let info = Term.info ~doc:"Crypto utility tool" "cryptool" in
  Cli_help.term, info

let terms =
  [ Cli_help.cmd
  ; Cli_lexicon.cmd
  ]

let () =
  match Cmdliner.Term.eval_choice ~catch:true main_term terms with
    | `Error _
      -> exit 1
    | `Ok ()
    | `Help
    | `Version
      -> exit 0
