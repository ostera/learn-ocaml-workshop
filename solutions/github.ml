(* Four possible subcommands for hub *)

let show_issue ~user ~repo num : unit =
  let (issue : Github_t.issue) =
    let open Github.Monad in
    Github.Issue.get ~user ~repo ~num ()
    >|= Github.Response.value |> Github.Monad.run |> Lwt_main.run
  in
  Printf.printf "%d %s\n\n%s\n" issue.issue_number issue.issue_title issue.issue_body 

let read f n s =
  let rec loop n s =
    let open Github.Monad in
    Github.Stream.next s
    >>= function
    | Some (item, next) when n > 0 ->
      f item;
      loop (pred n) next
    | _ ->
      return ()
  in
  loop n s
  |> Github.Monad.run
  |> Lwt_main.run

let list_issues ~user ~repo () : unit =
  let print_issue {Github_t.issue_number; issue_title; _} =
    Printf.printf "%d %s\n" issue_number issue_title
  in
  read print_issue 10 @@ Github.Issue.for_repo ~user ~repo ()

let list_prs ~user ~repo () : unit =
  let print_pr {Github_t.pull_number; pull_title; _} =
    Printf.printf "%d %s\n" pull_number pull_title
  in
  read print_pr 10 @@ Github.Pull.for_repo ~user ~repo ()

let checkout_pr ~user ~repo ?branch num : unit =
  let main =
    let open Github.Monad in
    Github.Pull.get ~user ~repo ~num ()
    >>~ fun {Github_t.pull_head = {branch_ref; branch_user; branch_repo; _}; _} ->
      let branch =
        match branch with
        | None -> branch_ref
        | Some branch -> branch
      in
      let branch_slug =
        match (branch_user, branch_repo) with
        | (Some {Github_t.user_login; _}, Some {Github_t.repository_name; _}) ->
            Printf.sprintf "%s/%s:" user_login repository_name
        | _ -> ""
      in
      Printf.printf "Head is %s%s checkout to %s\n" branch_slug branch_ref branch;
      return ()
  in
  Lwt_main.run @@ Github.Monad.run main
