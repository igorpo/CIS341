open Datastructures

let liveness_analysis_tests =
  [ "llprograms/analysis9.ll", lblm[
      "_entry", uids[]
      ; "body", uids["1"; "2"; "d0"; "d1"]
      ; "end", uids["1"]
      ; "guard", uids["1"; "2"; "d0"]
    ]
  ; "llprograms/analysis8.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis7.ll", lblm[
      "_entry", uids[]
      ; "body", uids["1"; "4"]
      ; "end", uids["1"]
      ; "guard", uids["1"; "4"]
    ]
  ; "llprograms/analysis5.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis4.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["2"; "3"]
      ; "l2", uids["3"]
    ]
  ; "llprograms/analysis3.ll", lblm[
      "_entry", uids[]
      ; "l2", uids["1"]
      ; "l3", uids["2"]
      ; "l4", uids["3"]
      ; "l5", uids["4"]
      ; "l6", uids["5"]
      ; "l7", uids["6"]
      ; "l8", uids["7"]
      ; "l9", uids["8"]
      ; "lexit", uids["9"]
    ]
  ; "llprograms/analysis2.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["1"; "3"]
      ; "l2", uids[]
    ]
  ; "llprograms/analysis19.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis18.ll", lblm[
      "_entry", uids[]
      ; "bar", uids["1"; "2"; "3"; "4"]
      ; "foo", uids["1"; "sa"]
    ]
  ; "llprograms/analysis16.ll", lblm[
      "_entry", uids[]
      ; "continue_loop", uids["1"; "2"; "5"]
      ; "else", uids["1"; "2"; "5"; "7"]
      ; "foo", uids["1"; "2"]
      ; "if", uids["1"; "2"; "5"; "7"]
      ; "loop", uids["1"; "2"]
      ; "reta", uids["1"]
      ; "retb", uids["2"]
    ]
  ; "llprograms/analysis15.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis14.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis13.ll", lblm[
      "_entry", uids[]
      ; "correct", uids["7"]
      ; "five", uids["1"; "6"; "7"]
      ; "four", uids["1"; "5"; "6"; "7"]
      ; "one", uids["1"; "2"; "3"; "4"; "5"; "6"; "7"]
      ; "three", uids["1"; "4"; "5"; "6"; "7"]
      ; "two", uids["1"; "3"; "4"; "5"; "6"; "7"]
      ; "wrong", uids["1"]
    ]
  ; "llprograms/analysis12.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis11.ll", lblm[
      "_entry", uids[]
      ; "foo", uids[]
    ]
  ; "llprograms/analysis10.ll", lblm[
      "_entry", uids[]
      ; "else", uids["1"; "7"]
      ; "merge", uids["1"; "7"]
      ; "then", uids["1"; "7"]
    ]
  ; "llprograms/analysis1.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["1"; "3"]
    ]
  ]

let alias_analysis_tests =
  let open Alias.SymPtr in
  [ "llprograms/analysis9.ll", lblm[
      "_entry", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "body", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "end", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "guard", uidm["1", Unique; "2", Unique; "argv", MayAlias]
    ]
  ; "llprograms/analysis8.ll", lblm[
      "_entry", uidm["argv", MayAlias; "tmp1", Unique]
    ]
  ; "llprograms/analysis7.ll", lblm[
      "_entry", uidm["2", Unique; "4", Unique; "argv", MayAlias]
      ; "body", uidm["2", Unique; "4", Unique; "argv", MayAlias]
      ; "end", uidm["2", Unique; "4", Unique; "argv", MayAlias]
      ; "guard", uidm["2", Unique; "4", Unique; "argv", MayAlias]
    ]
  ; "llprograms/analysis5.ll", lblm[
      "_entry", uidm["argv", MayAlias]
    ]
  ; "llprograms/analysis4.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
      ; "l1", uidm["3", MayAlias; "6", MayAlias; "7", MayAlias; "arcv", MayAlias]
      ; "l2", uidm["3", MayAlias; "6", MayAlias; "7", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis3.ll", lblm[
      "_entry", uidm["arcv", MayAlias]
      ; "l2", uidm["arcv", MayAlias]
      ; "l3", uidm["arcv", MayAlias]
      ; "l4", uidm["arcv", MayAlias]
      ; "l5", uidm["arcv", MayAlias]
      ; "l6", uidm["arcv", MayAlias]
      ; "l7", uidm["arcv", MayAlias]
      ; "l8", uidm["arcv", MayAlias]
      ; "l9", uidm["arcv", MayAlias]
      ; "lexit", uidm["arcv", MayAlias]
    ]
  ; "llprograms/analysis2.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
      ; "l1", uidm["3", MayAlias; "5", MayAlias; "arcv", MayAlias]
      ; "l2", uidm["3", Unique; "arcv", MayAlias]
    ]
  ; "llprograms/analysis19.ll", lblm[
      "_entry", uidm["1", Unique; "2", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis18.ll", lblm[
      "_entry", uidm["1", Unique; "arcv", MayAlias]
      ; "bar", uidm["1", Unique; "arcv", MayAlias; "sa", Unique; "sb", Unique; "sc", Unique]
      ; "foo", uidm["1", Unique; "arcv", MayAlias; "sa", Unique; "sb", Unique; "sc", Unique]
    ]
  ; "llprograms/analysis16.ll", lblm[
      "_entry", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "continue_loop", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "else", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "foo", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "if", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "loop", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "reta", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
      ; "retb", uidm["1", Unique; "2", Unique; "arcv", MayAlias]
    ]
  ; "llprograms/analysis15.ll", lblm[
      "_entry", uidm["arcv", MayAlias; "head", MayAlias; "link", MayAlias; "link2", MayAlias; "next", MayAlias; "next2", MayAlias; "val", MayAlias; "val2", MayAlias]
    ]
  ; "llprograms/analysis14.ll", lblm[
      "_entry", uidm["1", MayAlias; "2", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis13.ll", lblm[
      "_entry", uidm["arcv", MayAlias]
      ; "correct", uidm["arcv", MayAlias]
      ; "five", uidm["arcv", MayAlias]
      ; "four", uidm["arcv", MayAlias]
      ; "one", uidm["arcv", MayAlias]
      ; "three", uidm["arcv", MayAlias]
      ; "two", uidm["arcv", MayAlias]
      ; "wrong", uidm["arcv", MayAlias]
    ]
  ; "llprograms/analysis12.ll", lblm[
      "_entry", uidm["arcv", MayAlias]
    ]
  ; "llprograms/analysis11.ll", lblm[
      "_entry", uidm["argv", MayAlias]
      ; "foo", uidm["4", Unique; "5", Unique]
    ]
  ; "llprograms/analysis10.ll", lblm[
      "_entry", uidm["4", Unique; "7", Unique; "argv", MayAlias]
      ; "else", uidm["4", Unique; "7", Unique; "argv", MayAlias]
      ; "merge", uidm["4", Unique; "7", Unique; "argv", MayAlias]
      ; "then", uidm["4", Unique; "7", Unique; "argv", MayAlias]
    ]
  ; "llprograms/analysis1.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
      ; "l1", uidm["3", MayAlias; "4", MayAlias; "arcv", MayAlias]
    ]
  ]
