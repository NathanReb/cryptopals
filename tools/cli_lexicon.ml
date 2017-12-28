let run file =
  let open CCResult.Infix in
  Ffile.Infile.of_string file >|= fun infile ->
  let lexicon = Lexicon.of_file infile in
  Printf.printf "Lexicon built from %s:\n%s\n" file ([%show: Lexicon.t] lexicon)

let term =
  let open Cmdliner in
  let file =
    let docv = "FILE" in
    let doc = "Input text file" in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv [])
  in
  Tterm.result @@
  Term.(const run $ file)

let info =
  let open Cmdliner in
  Term.info ~doc:"Show the given text file lexicon" "lexicon"

let cmd = term, info
