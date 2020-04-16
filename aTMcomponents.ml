(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

let database : account_spec list ref = ref [];;

let initialize (lst : account_spec list) : unit = 
  database := lst 

let acquire_id : unit -> id = fun () ->
  print_string "Input your ID: ";
  read_int ()

let acquire_amount : unit -> int = fun () ->
  print_string "Enter amount: ";
  read_int()

let rec acquire_act : unit -> action = fun () ->
  print_string "Enter action (B for balance, W for withdraw, D for deposit, N for next, F for finished): ";
  let c = read_line() in 
  begin match c with 
  | "B" | "b" -> Balance 
  | "W" | "w" -> 
    let amount = acquire_amount () in Withdraw amount 
  | "D" | "d" -> 
    let amount = acquire_amount () in Deposit amount 
  | "N" | "n" -> Next
  | "F" | "f" -> Finished
  | _ -> 
      print_endline "Invalid action. Starting over.";
      acquire_act ()
  end

let get_balance (id : int) : int = 
  let record = (List.find (fun r -> r.id = id) !database) in record.balance 

let get_name (id : int) : string = 
  let record = (List.find (fun r -> r.id = id) !database) in record.name

let update_balance (id : int) (amount : int) : unit = 
  let record = (List.find (fun r -> r.id = id) !database) in 
  database := {name=record.name; id=record.id; balance=amount} :: 
    !database

let present_message (msg : string) : unit = 
  print_endline msg 

let deliver_cash (amt : int) : unit = 
  present_message ("You got " ^ (string_of_int amt) ^ " cash")
