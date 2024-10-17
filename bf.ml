type instruction =
   | In | Out
   | Increase | Decrease
   | Move | Jump
   | MoveBack | JumpBack
   | Ignore

let parse =
  List.map
     @@ function
     | ',' -> In | '.' -> Out
     | '+' -> Increase | '-' -> Decrease
     | '>' -> Move | '<' -> MoveBack
     | '[' -> Jump | ']' -> JumpBack
     | _ -> Ignore

exception Unbalanced

let execute instructions =
  let brackets =
    List.(
      filter_map (fun t ->
        match snd t with
        | Jump | JumpBack -> Some t
        | _ -> None
      )
      @@
      (
        instructions |> mapi (fun idx instruction -> (idx, instruction))
      )
    )
  in
  let bracket_map =
    let table = Hashtbl.create (List.length brackets) in
    let rec aux stack = function
      | [] -> table
      | (idx, Jump) :: rest -> aux ((idx, Jump) :: stack) rest
      | (i', JumpBack) :: rest -> (
        match stack with
        | [] -> raise Unbalanced
        | (i, Jump) :: stack_tail ->
            Hashtbl.add table i i' ;
            Hashtbl.add table i' i ;
            aux stack_tail rest
        | _ -> assert false
      ) | _ -> assert false
    in
      aux [] brackets
  in

  let length = List.length instructions in

  if length > 0 then (
    let output = ref [] in
    let pointer = ref 0 in
    let cursor = ref (-1) in
    let tape = Array.init (2 lsl 16) (fun _ -> 0) in

    let cell () = tape.(!pointer) in
    let advance () = cursor := !cursor + 1 in
    let jump () = cursor := Hashtbl.find bracket_map !cursor in

    while !cursor < length - 1 do
      advance () ;

      match List.nth instructions !cursor with
      | In -> tape.(!pointer) <- Char.code (input_char stdin)
      | Out ->
          output := (Char.chr (cell ())) :: !output ;
          print_char (List.hd !output) ;
          flush stdout

      | Move -> pointer := !pointer + 1
      | MoveBack -> pointer := !pointer - 1

      | Jump -> if cell () = 0 then jump ()
      | JumpBack -> if cell () <> 0 then jump ()

      | Increase -> tape.(!pointer) <- tape.(!pointer) + 1
      | Decrease -> tape.(!pointer) <- tape.(!pointer) - 1

      | Ignore -> ()
    done ;

    String.of_seq (List.to_seq !output)
  ) else
    "No instructions to execute"

let () =
  let tokens =
    List.of_seq
    @@ String.to_seq
    @@ really_input_string stdin (in_channel_length stdin)
  in
  let filter =
    List.filter (
      fun i -> match i with
      | Ignore -> false | _ -> true
    )
  in
    tokens |> parse |> filter |> execute |> print_string

(* ./bf < fib.bf *)
