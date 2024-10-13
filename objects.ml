type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list


(** Génère le contenu d'un fichier versionné *)
let rec gen_cont_vers = function
    | Text str -> str
    | Directory [] -> ""
    | Directory ((nom,b,h,_)::tl) -> begin
        let d = if b=true then ";d;" 
                          else ";t;"
        in
        if tl=[] then nom^d^(Digest.to_hex h)
                 else nom^d^(Digest.to_hex h)^"\n"^(gen_cont_vers (Directory tl)) end


let hash _obj =
    Digest.string (gen_cont_vers _obj)


let is_known _h =
    Sys.file_exists (".ogit/objects/"^(Digest.to_hex _h))


(** Renvoie le contenu d'un fichier texte *)
let get_content fichier =
    if Sys.file_exists fichier then
        let ic = open_in fichier in
        let contenu = ref "" in
        let rec iter () =
        (* Lecture caractère par caractère à cause de problèmes avec input_line sur ligne vide *)
            contenu := !contenu^(String.make 1 (input_char ic));
            iter ()
        in
        try iter () with
            | End_of_file -> close_in ic; !contenu
    else
        raise (Invalid_argument ("Le fichier '"^fichier^"' n'existe pas"))


(** Fonction équivalente avec In_channel *)
let _get_content fichier =
    if Sys.file_exists fichier then
        In_channel.with_open_text fichier In_channel.input_all
    else
        raise (Invalid_argument ("Le fichier '"^fichier^"' n'existe pas"))


let read_text_object _h =
    get_content (".ogit/objects/"^(Digest.to_hex _h))


let store_object _obj =
    let contenu = gen_cont_vers _obj in
    let nom = Digest.to_hex (Digest.string contenu) in
    let oc = open_out (".ogit/objects/"^nom) in
    output_string oc contenu;
    close_out oc;
    Digest.string contenu


(** Renvoie l'objet versionnée correspondant*)
let rec create_obj_t fichier =
    if not (Sys.is_directory fichier) then Text(get_content fichier)
    else begin
        (* Renvoie la liste des fichiers et répertoires ne commençant pas par un point *)
        let liste_fichiers arr =
            let rec iter l = function
                | i when i>=Array.length arr -> l
                | i -> if arr.(i).[0]= '.' then iter l (i+1) else iter (arr.(i)::l) (i+1)
            in iter [] 0
        in

        let contenu = liste_fichiers (Sys.readdir fichier) in
        let rec iter resultat l = begin match l with
            | [] -> resultat
            | element::tl -> begin
                let objet = create_obj_t (fichier^"/"^element) in
                let l = if (Sys.is_directory (fichier^"/"^element))=true then
                    iter ((element, true, (hash objet), objet)::resultat) tl
                else
                    let h = Digest.file (fichier^"/"^element) in
                    iter ((element, false, h, objet)::resultat) tl in
                l
                end
            end

        in
        let res = iter [] contenu in
        Directory res
    end


let store_work_directory () =
    (* Créer tout les fichiers versionnés récursivement*)
    let rec iter = function
        | Text contenu -> ignore(store_object (Text contenu))
        | Directory liste -> begin
            (* Créer le fichier correspondant au quadruplet *)
            let aux (_,_,_,obj) = iter obj; ignore (store_object obj) in
            List.iter aux liste end

    in
    let obj_repo = create_obj_t "." in
    iter obj_repo; 
    store_object obj_repo

    
let rec read_directory_object _h =
    if is_known _h  then
        let contenu = read_text_object _h in 
        let l = String.split_on_char '\n' contenu in 
        let l = List.map (String.split_on_char ';') l in 
        let rec aux = function
            | [] -> []
            | [nom;t;h]::tl when t="t" -> (nom, false, Digest.from_hex h, Text (read_text_object (Digest.from_hex h)))::(aux tl)
            | [nom;t;h]::tl when t="d" -> (nom, true, Digest.from_hex h, (read_directory_object (Digest.from_hex h)))::(aux tl)
            | _ -> raise (Invalid_argument ((Digest.to_hex _h)^" n'est pas le hash d'un repertoire"))
        in
        Directory (aux l)
    else
        raise (Invalid_argument ((Digest.to_hex _h)^" n'existe pas dans objects"))


let clean_work_directory () =
    let rec iter rep =
        let contenu = Sys.readdir rep in (* On récupère le contenu du répertoire *)
        for i=0 to (Array.length contenu)-1 do 
          if contenu.(i).[0] <> '.' then begin (* Tous les fichiers cachés sont ignorés *)
            let file = rep^"/"^contenu.(i) in (* Le nom du fichier avec le chemin à partir de repo *)
            if Sys.is_directory file then
              iter file
            else
                Sys.remove file (* Suppression du fichier *)
            end; 
          done;
        ignore(Sys.command ("rmdir "^rep^" 2>/dev/null")) (* Suppression du répertoire (l'erreur est redirigée si il n'est pas vide) *)
    in iter "."


let rec restore_work_directory _obj =
    let rec iter = function
        | [] -> ()

        | (nom,b,_,obj)::tl when b=true -> begin (* Si c'est un répertoire *)
            ignore(Sys.command ("mkdir "^nom));
            Sys.chdir nom;
            restore_work_directory obj; (* Son contenu est restauré récursivement *)
            Sys.chdir "..";
            iter tl end

        | (nom,_,_,obj)::tl -> begin (* Sinon on créer le fichier avec son contenu *)
            let oc = open_out nom in 
            output_string oc (gen_cont_vers obj);
            close_out oc;
            iter tl end
    in
    match _obj with
        | Directory liste -> iter liste
        | _ -> raise (Invalid_argument "Cet objet ne represente pas un repertoire")

let rec merge_work_directory_I _obj =
    let rec iter conflit = function
        | [] -> conflit (* Renvoie le booléen conflit *)

        (* Si c'est un répertoire qui existe dans l'état actuel *)
        | (nom,b,_,obj)::tl when b=true && Sys.file_exists nom -> begin
            Sys.chdir nom;
            let conflit_rec = merge_work_directory_I obj in
            Sys.chdir "..";
            let conflit = conflit || conflit_rec in
            iter conflit tl end

        (* Si il n'existe pas on le créer *)
        | (nom,b,h,obj)::tl when b=true -> begin
            restore_work_directory (Directory [(nom,b,h,obj)]);
            iter conflit tl end

        (* Si c'est un fichier qui n'existe pas dans l'état actuel *)
        | (nom,_,_,obj)::tl when not (Sys.file_exists nom) -> begin
            let oc = open_out nom in 
            output_string oc (gen_cont_vers obj);
            close_out oc;
            iter conflit tl end

        (* Si le fichier existe déjà mais avec le même contenu on passe à la suite *)
        | (nom,_,h,_)::tl when h=hash (create_obj_t nom) -> iter conflit tl

        (* Si le fichier existe déjà mais avec un contenu différent : conflit *)
        | (nom,_,_,obj)::tl -> begin
            Sys.rename nom (nom^"..cl");
            let oc = open_out (nom^"..cr") in 
            output_string oc (gen_cont_vers obj);
            close_out oc;
            iter true tl end
    in
    match _obj with
    | Text _ -> false
    | Directory liste -> iter false liste
    