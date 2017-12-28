let run cmd_opt = `Help (`Auto, cmd_opt)

let term =
  let open Cmdliner in
  let docv = "COMMAND" in
  let doc = "Display this command's manpage" in
  let cmd_name = Arg.(value & pos 0 (some string) None & info ~doc ~docv []) in
  Term.(ret (const run $ cmd_name))

let info = Cmdliner.Term.info ~doc:"Show command's manpage" "help"

let cmd = term, info
