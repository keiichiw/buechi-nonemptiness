open Printf
open Syntax
type id = int
type edge = id * id * label

let max_v = 100
let states : (string * id) list ref = ref []
let edges : (edge * id) list ref = ref []
let adj : ((id * label) list) array = Array.make max_v []
let r_adj = Array.make max_v []
let s0_list : id list ref = ref []
let f_list  : id list ref = ref []


let add_op s t =
  match s, t with
  | Some x, _ -> Some x
  | _, Some x -> Some x
  | _ -> None

let rec create_list size v =
  match size with
  | 0 -> []
  | _ -> v :: (create_list (size - 1) v)


let state_id name =
  try
    List.assoc name !states
  with
  | Not_found ->
    let n = List.length !states in
    states := (name, n) :: !states;
    n
let state_name v_id =
  let rec go = function
    | (name, x) :: es when x = v_id ->
      name
    | _ :: es ->
      go es
    | [] -> raise Not_found in
  go !states

let add_edge src dst label =
  let id1 = state_id src in
  let id2 = state_id dst in
  let e_id = List.length !edges in
  edges := ((id1, id2, label), e_id) :: !edges;
  Array.set adj   id1 ((id2, label) :: adj.(id1));
  Array.set r_adj id2 ((id1, label) :: adj.(id2))


let find_path s g =
  let v_num = List.length !states in
  let used = Array.make v_num false in
  let ans = ref [] in
  let q = Queue.create () in
  List.iter (fun (d, _) -> Queue.push [(s, d)] q) adj.(s);
  while not (Queue.is_empty q) do
    let p = Queue.pop q in
    let (_, t) = List.hd p in
    if not used.(t) then begin
      Array.set used t true;
      if t = g then begin
        ans := p;
        Queue.clear q
      end else
        List.iter
          (fun (dst, _) ->
            if not used.(dst) then
              Queue.push ((t, dst)::p) q)
          adj.(t)
    end
  done;
  List.iter (fun (a, b) -> printf "ans %d->%d\n" a b) !ans;
  List.rev !ans

let load_data elist =
  let go = function
    | S0 q ->
      s0_list := (state_id q) :: !s0_list
    | F q  ->
      f_list := (state_id q) :: !f_list
    | D ((src, label), dst) ->
      add_edge src dst label in
  List.iter go elist


let find_lasso init_state =
  let v_num = List.length !states in
  let used = Array.make v_num false in
  let cmp  = Array.make v_num (-1) in
  let c_sz = Array.make v_num 0 in
  let vs   = ref [] in

  let rec dfs v_id =
    Array.set used v_id true;
    List.iter
      (fun (t, _) -> if not used.(t) then dfs t)
      adj.(v_id);
    vs := v_id :: !vs in

  let rec rdfs v_id k =
    Array.set used v_id true;
    Array.set cmp  v_id k;
    Array.set c_sz k (1 + c_sz.(k));
    List.iter
      (fun (t, _) -> if not used.(t) then rdfs t k)
      r_adj.(v_id) in

  let in_cycle q =
    let c_id = cmp.(q) in
    c_sz.(c_id) >= 2 in

  let same_component q =
    let c_id = cmp.(q) in
    let (_, l) =
      Array.fold_left
        (fun (idx, l) s ->
          if s = c_id then
            idx + 1, idx :: l
          else
            idx + 1, l)
        (0, []) cmp in
    l in

  let lasso q =
    if in_cycle q then
      let path  = find_path init_state q in
      let cycle = same_component q in
      Some (path, cycle)
    else
      None in

  dfs init_state;
  Array.fill used 0 v_num false;
  let go k v =
    if not used.(v) then
      (rdfs v k; k+1)
    else k in
  ignore (List.fold_left go 0 !vs);

  List.fold_left
    (fun x q -> add_op (lasso q) x)
    None !f_list

let out_ch = ref stderr

let write fmt =
  ksprintf (fun s -> fprintf (!out_ch) "%s\n" s ) fmt

let write_edge s t l =
  let a = state_name s in
  let b = state_name t in
  write "  %s -> %s [label = \"%c\"];" a b l

let write_edge_clr s t l color =
  let a = state_name s in
  let b = state_name t in
  write "  %s -> %s [label = \"%c\", color=%s];" a b l color


let write_dotfile name path cycle =

  let rec in_path s t p =
    List.exists
      (fun (a, b) -> a = s && b = t)
      p in

  let rec in_cycle s t c =
    let a = List.exists (fun x -> x = s) c in
    let b = List.exists (fun x -> x = t) c in
    a && b in

  List.iter (fun x -> printf "cycle %d\n" x) cycle;
  List.iter (fun (a, b) -> printf "path %d->%d\n" a b) path;
  write "digraph %s {" name;
  List.iter
    (fun ((s, t, l), _) ->
      if in_cycle s t cycle then
        write_edge_clr s t l "yellow"
      else if in_path s t path then
        write_edge_clr s t l "red"
      else
        write_edge s t l)
    !edges;
  write "}"

let main oc elist =
  out_ch := oc;
  load_data elist;
  let lasso =
    List.fold_left
      (fun x q -> add_op x (find_lasso q))
      None !s0_list in
  match lasso with
  | None ->
    printf "empty!\n";
    write_dotfile " hoge" [] []
  | Some (path, cycle) ->
    printf "nonempty!\n";
    write_dotfile " hoge" path cycle
