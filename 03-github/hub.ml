open Cmdliner

let user_t =
  let doc = "GitHub user/organisation to read" in
  let open Arg in
  value & opt string "ocaml" & info ["user"; "u"] ~doc

let repo_t =
  let doc = "GitHub repository to read" in
  let open Arg in
  value & opt string "ocaml" & info ["repository"; "r"] ~doc

(* This helper takes a list of subcommand and parses the positional
 * arguments to return the command (if matched) and any additional arguments *)
let mk_subcommands commands =
  let command =
    let doc = Arg.info ~docv:"COMMAND" [] in
    let commands =
      List.fold_left
        (fun acc (c,f,_,_) -> (c,f) :: acc) [] commands in
    Arg.(value & pos 0 (some & enum commands) None & doc)
  in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_right 0 string [] & doc)
  in
  command, params

(* The issue command by default lists issues and has a show subcommand *)
let issue_cmd =
  let commands = [
    "show", `show, ["NUMBER"], "Displays issue $(i,NUMBER)";
  ] in
  let man =
    [ `S Manpage.s_description
    ; `P "This command manipulates GitHub issues"
    ]
  in
  let doc = "The issue command" in
  let command, params = mk_subcommands commands in
  let issue command params user repo =
    match command, params with
    | None, [] -> Commands.list_issues ~user ~repo (); `Ok ()
    | Some `show, [number] -> Commands.show_issue ~user ~repo (int_of_string number); `Ok ()
    | Some `show, _ -> `Error (false, "Expect one issue number for hub issue command")
    | _ -> `Error (false, "Unrecognised hub issue command")
  in
  Term.(ret (const issue $command $params $user_t $repo_t),
        info "issue" ~man ~doc)

(* The pr command has a list command (which is also the default) and a checkout command *)
let pr_cmd =
  let commands = [
    "checkout", `checkout, ["NUMBER"; "[BRANCH]"], "Checks out the branch for a PR in this clone";
    "list", `list, [], "Lists PRs";
  ] in
  let man =
    [ `S Manpage.s_description
    ; `P "This command manipulates GitHub pull requests"
    ]
  in
  let doc = "The pr command" in
  let command, params = mk_subcommands commands in
  let issue command params user repo =
    match command, params with
    | None, _
    | Some `list, _ ->
      Commands.list_prs ~user ~repo (); `Ok ()
    | Some `checkout, [number] ->
      Commands.checkout_pr ~user ~repo (int_of_string number); `Ok ()
    | Some `checkout, [number; branch] ->
      Commands.checkout_pr ~user ~repo ~branch (int_of_string number); `Ok ()
    | Some `checkout, _ ->
      `Error (false, "Expect a pr number and optionally a branch for hub pr checkout command")
    | _ -> `Error (false, "Unrecognised hub pr command")
  in
  Term.(ret (const issue $command $params $user_t $repo_t), info "pr" ~man ~doc)

(* The default command is just the help screen *)
let default_cmd =
  let doc = "Hub is a tool that wraps git in order to extend with extra functionality that makes \
             it better when working with GitHub. It's written in pure OCaml which makes it even \
             more cool." in
  let sdocs = Manpage.s_common_options in
  let man_xrefs = [] in
  let man =
    [ `S Manpage.s_description
    ; `P "The $(tname) tool provides cool features."
    ; `P "You'd expect to have more paragraphs in the final man page!"
    ]
  in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()),
        info "hub" ~version:"ReasonConf2019" ~doc ~man_xrefs ~sdocs ~man)

let cmds = [issue_cmd; pr_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
