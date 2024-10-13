type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}


let date_fm _d =
    let corrige_longueur x =
    (* Ajoute un éventuel 0 devant le nombre et le convertie en string *)
        if x<10 then
            "0"^string_of_int x
        else
            string_of_int x
    in
    let time = Unix.localtime (_d) in 
    let h = corrige_longueur time.tm_hour in 
    let m = corrige_longueur time.tm_min in 
    let s = corrige_longueur time.tm_sec in 
    let j = corrige_longueur time.tm_mday in 
    let mois = corrige_longueur (time.tm_mon + 1) in 
    let a = corrige_longueur (time.tm_year + 1900) in 
    h^":"^m^":"^s^"-"^j^"/"^mois^"/"^a


let set_head _l =
    let contenu = String.concat "\n" (List.map Digest.to_hex _l) in
    let oc = open_out ".ogit/HEAD" in  
    output_string oc contenu;
    close_out oc


let get_head () =
    let ic = open_in ".ogit/HEAD" in 
    let rec iter contenu =
        try iter ((input_line ic)::contenu)
        with End_of_file -> contenu 
    in 
    let liste = iter [] in
    close_in ic;
    List.map Digest.from_hex liste


let make_commit _s  _h =
    let parents = get_head() in 
    let date = Unix.time() in 
    {parents; date; message=_s; content=_h}


let init_commit () =
    let date = Unix.time() in 
    {parents=[]; date; message="init commit"; content=Objects.store_work_directory()}


let store_commit _c =
    let parents = String.concat ";" (List.map Digest.to_hex _c.parents) in
    let contenu = parents^"\n"^(date_fm _c.date)^"\n"^_c.message^"\n"^Digest.to_hex _c.content in
    let hash_contenu = Digest.string contenu in
    let oc = open_out (".ogit/logs/"^Digest.to_hex hash_contenu) in
    output_string oc contenu;
    close_out oc;
    hash_contenu


let read_commit _h =
    let ic = open_in (".ogit/logs/"^Digest.to_hex _h) in 
    let parents = try List.map Digest.from_hex (String.split_on_char ';' (input_line ic))
                  with _ -> [] (* Liste vide si il n'y a pas de parents *)
    in

    (* Renvoie la liste des valeurs de la date fm *)
    let split_val s =
        let l = String.split_on_char '-' s in 
        match l with
            | e1::e2::[] -> (String.split_on_char ':' e1)@(String.split_on_char '/' e2)
            | _ -> []
    in
    let liste_val = List.map int_of_string (split_val (input_line ic)) in 
    let date = match liste_val with
        | h::m::s::j::mois::a::[] -> begin
            fst (Unix.mktime { 
                    Unix.tm_hour = h; 
                    Unix.tm_min = m; 
                    Unix.tm_sec = s;
                    Unix.tm_mday = j; 
                    Unix.tm_mon = mois-1; 
                    Unix.tm_year = a-1900;
                    Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = false}) end
        | _ -> 0.
    in

    let message = input_line ic in 
    let content = Digest.from_hex (input_line ic) in 
    close_in ic;
    {parents; date; message; content}
