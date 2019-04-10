(* Four possible subcommands for hub *)

let show_issue ~user:_ ~repo:_ _number : unit =
  failwith "Display summary information for user/repo#number"

let list_issues ~user:_ ~repo:_ () : unit =
  failwith "List issues for user/repo"

let list_prs ~user:_ ~repo:_ () : unit =
  failwith "List pull requests for user/repo"

let checkout_pr ~user:_ ~repo:_ ?branch:_ _number : unit =
  failwith "Checkout user/repo#number either to the name of pull request branch \
            or to the branch name given in ?branch"
