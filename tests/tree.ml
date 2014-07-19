open Helpers
open Helpers.Result
open Neorest
open CalendarLib
open Printf

(*
let () = Http_client.Convenience.http_verbose ~verbose_status:true ()
 *)
type 'a event = { event_desc: 'a; when_: Calendar.t }

module API = Neorest.Make(struct let server="localhost" let port=7474 end)

let next_year : int -> (int option,_) Result.t = fun cur ->
  let next_cmd = "MATCH (years:YEAR) WHERE years.n > {y} RETURN min(years.n)" in
  let (ans: string) = API.post_cypher ~params:["y", `Int cur] next_cmd in
  match to_json ans |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[`Null]] -> OK None
  | `List[`List[`Int x]]-> OK (Some x)
  | _ -> Error ()

let prev_year : int -> (int option,_) Result.t = fun cur ->
  let next_cmd = "MATCH (years:YEAR) WHERE years.n < {y} RETURN max(years.n)" in
  let (ans: string) = API.post_cypher ~params:["y", `Int cur] next_cmd in
  match to_json ans |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[`Null]] -> OK None
  | `List[`List[`Int x]]-> OK (Some x)
  | _ -> Error ()

let connect_years from dest =
  let cmd = "MATCH (l:YEAR{n: {ll} }), (r:YEAR{n: {rr} })
             WITH l as l, r as r
             CREATE l-[edge:NEXT_YEAR]->r
             RETURN edge
             " in
  let (ans: string) = API.post_cypher ~params:[ ("ll", `Int from); ("rr",`Int dest) ] cmd in
  print_endline ans;
  match to_json ans |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[_]] -> OK ()
  | _ -> Error ()


let make_nodes events =
  let () = match API.remove_all () with
    | OK () -> ()
    | Error () -> fprintf stderr "Can't connect to database"; exit 1
  in
  let f = fun {event_desc; when_} ->
    (* Add year *)
    let y = Calendar.year when_ in
    let p1 = ["y", `Int y] in
    let _ = API.post_cypher "merge (:YEAR{n: {y}})" ~params:p1 in
    (* add month *)
    let m = Calendar.month when_ |> Date.int_of_month in
    let p2 = ("m",`Int m) :: p1 in
    let _ = API.post_cypher "MATCH (y:YEAR{n: {y}})
                             CREATE UNIQUE y-[:HAS_MONTH]->(m:MONTH{n: {m}})"
                            ~params:p2
    in
    (* add day *)
    let d = Calendar.day_of_month when_ in
    let p3 = ("d",`Int d) :: p2 in
    let _ = API.post_cypher "MATCH (y:YEAR{n: {y}}), (m:
                             CREATE UNIQUE y-[:HAS_MONTH]->(m:MONTH{n: {m}})"
                            ~params:p3
    in

    let cmd = sprintf
                "MERGE (y:YEAR {n:{yy}}),
                       (m:MONTH{n:{mm}}),
                       (d:DAY  {n:{dd}}),
                       (e:EVENT{desc: {desc} })
                 ON
                 MATCH y-[:MONTH]->m-[:DAY]->d-[:EVENT]->e;
                 "
    in
    let params = [ ("yy", `Int y)
                 ; ("mm", `Int m)
                 ; ("dd", `Int d)
                 ; ("desc", `String (Printer.CalendarPrinter.to_string when_))
                 ]
    in
    let (_: string) =  API.post_cypher ~params cmd in
    let _ = next_year y
      >>= function
        | Some next -> printf "There is next year %d\n%!" next; connect_years y next
        | None -> OK ()
    in
    let _ = prev_year y
            >>= function
              | Some prev -> printf "There is prev year %d\n%!" prev; connect_years prev y
              | None -> OK ()
    in
    ()
  in
  List.iter events ~f

let events =
  let open Calendar in
  [ { event_desc="event1"; when_ = make 2009 08 23 15 32 43 }
  ; { event_desc="event2"; when_ = make 2012 10 21 12 13 14 }
  ; { event_desc="event3"; when_ = make 2009 09 22 15 32 43 }
  ;
  ]

let () =
  make_nodes events

