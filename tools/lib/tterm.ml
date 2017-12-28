let result t =
  let open Cmdliner.Term in
  term_result ~usage:false (const (CCResult.map_err (fun msg -> `Msg msg)) $ t)
